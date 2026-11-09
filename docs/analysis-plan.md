# Plan d'analyse de la line list — CTE de Mongbwalu

**Rapport :** Situation épidémiologique de la maladie à virus Ebola (souche Bundibugyo) dans la zone de santé de Mongbwalu, province de l'Ituri, RDC
**Auteur du plan :** Dr Abdul Salam Mbengue (09/11/2026)
**Implémentation :** [`R/07_analysis_plan.R`](../R/07_analysis_plan.R) → [`reports/analysis_plan_report.qmd`](../reports/analysis_plan_report.qmd)

```bash
Rscript scripts/render.R analysis_plan            # Word + Excel (tous les tableaux)
```

## Correspondance entre le plan et la line list

Le plan cite des variables d'une autre base (Resultat_labo, Type_de_prelevement, Signe…). Le tableau ci-dessous donne la variable correspondante dans la line list MSF (noms après `janitor::clean_names()`).

| Élément du plan | Variable du plan | Line list MSF | Remarque |
|---|---|---|---|
| Cas confirmés | `Resultat_labo = confirmé` | `evd_status == "Confirmé"` | Classification finale |
| Cas suspects | « tous les résultats autres que Confirmé » | `evd_status != "Confirmé"` | Le rapport distingue aussi Suspect, Probable et Non cas |
| Personnel de santé (+ structure) | — | `job` commençant par « Personnel de santé », `hf_name_job` | |
| Décès | `Type_de_prelevement` = écouvillon post-mortem | `type_of_exit == "Décédé"` **ou** `dead_upon_arrival == "Oui"` | Le type de prélèvement n'existe pas dans la line list CTE |
| Âge (années et mois) | `Age_annees`, `Age_mois` | `age` + `age_unit` → `age_years` | Mois / 12, jours / 365 ; > 100 ans → manquant |
| Tranches d'âge | 0-9, 5-14, 15-44, 40-49, 50+ | 0-4, 5-14, 15-44, 45-49, 50+ | Les tranches du plan se chevauchent ; voir `cfg$age_breaks_plan` |
| Sexe | `Sexe` | `sex` (M/F) | |
| Profession | `Profession` | `job` | Très incomplet : interpréter avec prudence |
| Enceinte / allaitante | — | `pregnant`, `breastfeeding` | Calculé chez les femmes |
| Aire de santé de résidence | `Aire_de_santé_de_residence` | `adm3_name_res` (et `adm2_name_res` pour la zone) | AS hors ZS Mongbwalu regroupées par zone |
| Semaine de notification | — | `date_notification` | |
| Signes cliniques | `Signe` | une colonne par signe (`fever`, `nausea`, `diarrhoea`, …) | % parmi Oui/Non renseignés |
| Convulsions | `Signe` | — | Non collecté |
| Saignements | `Signe` | `bleeding`, `bleeding_epistaxis`, `bleeding_vagina`, `bleeding_vomit` (hématémèse), `bleeding_gum`, `bleeding_other`, … | |
| Type de prélèvement | `Type_de_prelevement` | — | Non disponible ; remplacé par le nombre de prélèvements |
| Résultats de laboratoire | `Resultat_labo` | `lab_result_1` … `lab_result_n` | |
| Délai de prise en charge | `Date_notification − Date_debut_symptomes` | `date_notification − date_symptom_onset` | < 0 ou > 60 jours exclus |
| Classes de délai | < 1 j, 1-2 j, 2-5 j, 5 j | < 1 j, 1-2 j, 3-5 j, > 5 j | Classes non chevauchantes ; 3 classes (0-2, 3-5, > 5) en bivarié |
| Âge en 2 classes | < 15 ans / ≥ 15 ans | `age_cat2` | |
| Localisation en 2 classes | Mongbwalu = 1, autres = 2 | `residence_cat2` : AS Mongbwalu vs autres localisations | Interprétation : **aire** de santé de Mongbwalu |

## Contenu du rapport

### 1. Analyse univariée

1. **Chiffres clés** — total, confirmés, probables, suspects, non-cas, non confirmés, personnels de santé (et structure), décès (dont décès à l'arrivée et parmi les confirmés).
2. **Dernière semaine** — admissions (hors décès à l'arrivée, comme le sitrep) et variation, confirmés, décès.
3. **Sociodémographie des cas confirmés** — moyenne, écart type, médiane, IIQ, min, max de l'âge ; tranches d'âge ; sexe et sex-ratio ; pyramide des âges ; profession ; grossesse ; allaitement ; tableaux âge × sexe (cumul et 3 dernières semaines) ; proportion de moins de 5 ans par semaine.
4. **Localisation** — aire de santé de résidence × statut ; provenance des confirmés de la semaine.
5. **Courbes épidémiques** — confirmés par semaine de notification selon le statut vital ; décès par jour et par semaine ; admissions par semaine et classification ; tableau de synthèse du CTE.
6. **Signes cliniques et saignements** des cas confirmés.
7. **Mortalité** — décès par statut (au CTE / à l'arrivée), par aire de santé, létalité des confirmés (IC 95 % de Wilson).
8. **Laboratoire** — résultat du premier prélèvement et nombre de prélèvements par statut.
9. **Délai de prise en charge** — médiane (IIQ) et classes, confirmés vs non confirmés.

### 2. Analyses bivariées

Odds ratios bruts (régression logistique, IC 95 % de Wald) et test du χ² (Fisher si un effectif théorique < 5). Un OR non estimable (0 % ou 100 % d'événements dans une modalité) est signalé « NC ».

| Issue | Population | Expositions |
|---|---|---|
| Confirmation | Patients avec classification finale (confirmés + non-cas) | âge (< 15 / ≥ 15), résidence (AS Mongbwalu / autres), délai de prise en charge ; détail par aire de santé |
| Décès | Cas confirmés à l'issue connue (décès ou guérison) | idem ; létalité par aire de santé |

### 3. Qualité des données

Pourcentage de valeurs manquantes pour les variables sociodémographiques, géographiques, cliniques et de laboratoire clés (toutes admissions) ; grossesse et allaitement chez les femmes de 13 à 50 ans.

## Points d'attention

- **Létalité** : le rapport présente deux valeurs. Celle du plan inclut les décès à l'arrivée confirmés ; celle du sitrep les exclut (patients pris en charge au CTE uniquement).
- **Biais d'information** : la profession et les fiches d'investigation sont très incomplètes. Les signes cliniques ne sont pas renseignés pour tous les patients ; les pourcentages sont calculés parmi les patients renseignés, et le dénominateur figure dans chaque tableau.
- **Délai de notification** : dans la line list du CTE, la date de notification est presque toujours égale à la date d'admission. Le délai mesure donc surtout le délai début des symptômes → admission.
