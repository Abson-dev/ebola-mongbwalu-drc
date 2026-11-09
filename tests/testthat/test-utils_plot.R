test_that("vis_epicurve counts rows per Monday-starting week", {
  df <- data.frame(
    d = as.Date(c("2026-11-02", "2026-11-08", "2026-11-09", "2026-11-15", NA)),
    g = c("a", "b", "a", "a", "a")
  )
  p <- vis_epicurve(df, date_measure = "d")
  expect_s3_class(p, "ggplot")
  ld <- ggplot2::layer_data(p)
  expect_equal(sum(ld$y), 4)
  expect_equal(sort(ld$y), c(2, 2))
})

test_that("vis_epicurve drops dates on/after date_max and stacks by group", {
  df <- data.frame(d = as.Date(c("2026-11-02", "2026-11-03", "2026-11-10")),
                   g = c("a", "b", "a"))
  p <- vis_epicurve(df, date_measure = "d", date_max = as.Date("2026-11-09"),
                    fill_by = "g", pal = c(a = "red", b = "blue"))
  ld <- ggplot2::layer_data(p)
  expect_equal(nrow(ld), 2)
  expect_equal(sum(ld$ymax - ld$ymin), 2)
})

test_that("plot_age_pyramid puts the first level on the left", {
  df <- data.frame(age_group = factor(c("0-4", "0-4", "5-14")),
                   sex_f = factor(c("Féminin", "Masculin", "Féminin"), levels = c("Féminin", "Masculin")))
  p <- plot_age_pyramid(df)
  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$n_signed[p$data$.split == "Féminin"] <= 0))
})
