test_that("collapse_errors keeps flagged rows and joins messages", {
  df <- data.frame(id = 1:3,
                   err1 = c("A", NA, NA),
                   err2 = c("B", NA, "C"))
  out <- collapse_errors(df, "^err")
  expect_equal(out$id, c(1, 3))
  expect_identical(out$errors, c("A; B", "C"))
})

test_that("missing_summary counts and sorts missing values", {
  df <- data.frame(a = c(1, NA, NA, 4), b = c(1, 2, 3, NA))
  out <- missing_summary(df, c("a", "b"), c("Var A", "Var B"))
  expect_identical(out$Variable, c("Var A", "Var B"))
  expect_identical(out$`Manquant (n)`, c(2L, 1L))
  expect_identical(out$`Manquant (%)`, c("50.0%", "25.0%"))
})

test_that("qa_exempt reads rule-specific exemptions", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("# comment", "rule,patient_site_id,reason",
               "err10,ID1,ok", "err10,ID2,ok", "err15,ID3,ok"), path)
  expect_setequal(qa_exempt("err10", path), c("ID1", "ID2"))
  expect_length(qa_exempt("err99", path), 0)
  expect_length(qa_exempt("err10", "does-not-exist.csv"), 0)
})

test_that("apply_corrections overrides values by patient ID", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("patient_site_id,variable,value,reason", "P2,evd_status,Non cas,test",
               "P9,evd_status,Confirmé,absent ID", "P1,unknown_var,x,test"), path)
  df <- data.frame(patient_site_id = c("P1", "P2"), evd_status = c("Confirmé", "Confirmé"))
  expect_warning(out <- apply_corrections(df, path), "unknown variable")
  expect_identical(out$evd_status, c("Confirmé", "Non cas"))
})

test_that("freq_table returns counts, percentages and a total row", {
  df <- data.frame(x = c("a", "a", "b", NA))
  out <- freq_table(df, "x", "Catégorie")
  expect_identical(names(out), c("Catégorie", "N", "%"))
  expect_identical(as.character(out$Catégorie), c("a", "b", "Inconnu", "Total"))
  expect_equal(out$N, c(2, 1, 1, 4))
})

test_that("bivariate_table computes crude odds ratios against the first level", {
  set.seed(1)
  df <- data.frame(
    exp = factor(rep(c("ref", "exposed"), each = 100), levels = c("ref", "exposed")),
    y = c(rep(c(TRUE, FALSE), c(20, 80)), rep(c(TRUE, FALSE), c(50, 50)))
  )
  out <- bivariate_table(df, "y", c("Exposition" = "exp"))
  expect_equal(nrow(out), 2)
  expect_identical(out$`OR (IC 95%)`[1], "Réf.")
  # OR = (50/50) / (20/80) = 4
  expect_match(out$`OR (IC 95%)`[2], "^4\\.00 ")
  expect_identical(out$`Issue (n)`, c(20L, 50L))
})

test_that("bivariate_table flags non-estimable odds ratios", {
  df <- data.frame(exp = factor(rep(c("a", "b"), each = 10)),
                   y = c(rep(TRUE, 10), rep(c(TRUE, FALSE), 5)))
  out <- bivariate_table(df, "y", c("E" = "exp"))
  expect_identical(out$`OR (IC 95%)`, c("Réf.", "NC"))
})
