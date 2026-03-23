library(data.table)
library(torr)

# totp <- "O:\\Prosjekt\\Rusdata\\Rusundersokelsen\\Datasets\\Rusus_2025\\rus2025_v3" #
# totf <- "rus2025_fhi_hdir.csv"                                                      #
#                                                                                     #
# tot <- fread(file.path(totp, totf))                                                 #

source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/folder-path.R")
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/setup.R")

# source("https://raw.githubusercontent.com/folkehelsestats/toir/refs/heads/main/unodc/fun-weighted-unweighted-ci.R")
source("https://raw.githubusercontent.com/folkehelsestats/toir/refs/heads/main/reports/functions/fun-weighted-ci-total.R")
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-percent-weighted.R")

## -----------------------------------------
## 2025 data
## -----------------
dt <- readRDS(file.path(Rususdata, "Rusus_2025", "rusus2025_20251126.rds"))
setDT(dt)

setnames(dt, names(dt), tolower(names(dt)))

dt[, vekt1 := vekt]
dt[, vekt := gsub(",", ".", vekt)]
dt[, vekt := as.numeric((vekt))]

# Populasjon -------------------------------------------------------------------

dt[is.na(tob2), roykPop := 1]
dt[, roykPop := fifelse(!(tob2 %in% 8:9), 1, NA)]

dt[, snusPop  := data.table::fifelse(tob60 %in% 1:3, 1L,NA)]
dt[, esigPop  := data.table::fifelse(tobe %in% 1:3, 1L, NA)]

popCols <- c("roykPop", "snusPop", "esigPop")
dt[, totPop_n := rowSums(.SD, na.rm = TRUE), .SDcols = popCols]
dt[, totPop := fifelse(totPop_n %in% 1:3, 1, 0)]


# -----------------------------------------------------------------------------
# Cases -----------------------------------------------------------------------
## Røyker daglig, av og til, aldri eller tidligere
## 1 - daglig
## 2 - av og til

# rule: in 1:2 -> 1; otherwise (incl. NA) -> 0
dt[, royk1 := data.table::fifelse(tob1  %in% 1:2, 1L, 0L, na = 0L)]
dt[, royk2 := data.table::fifelse(tob2  %in% 1:2, 1L, 0L, na = 0L)]
dt[, snus1  := data.table::fifelse(tob60 %in% 1:2, 1L, 0L, na = 0L)]
dt[, esig1  := data.table::fifelse(tobe %in% 1:2, 1L, 0L, na = 0L)]


# Alle -------------------------------------------------------------------------
dt[, combi3 := royk2 + snus1 + esig1]

# Kategorier per kombinasjoner ---
dt[, combiCat := data.table::fcase(
  combi3 == 3L, "royk+snus+esig",
  combi3 == 2L & royk2 == 1L & snus1 == 1L,  "royk+snus",
  combi3 == 2L & royk2 == 1L & esig1 == 1L,  "royk+esig",
  combi3 == 2L & snus1  == 1L & esig1 == 1L, "snus+esig",
  combi3 == 1L & royk2 == 1L,                "bare royk",
  combi3 == 1L & snus1  == 1L,               "bare snus",
  combi3 == 1L & esig1  == 1L,               "bare esig",
  combi3 == 0L,                              "none",
  default = NA_character_
)]

dt[, combiCase := fifelse(combi3 == 3, 1, 0)]

dt[, popSub := fifelse(combi3 != 0, 1, NA)]

# Dual bruker, combi bruker inkludert ------------------------------------------
dt[, dual_rs_n := royk2 + snus1]   # royk + snus
dt[, dual_re_n := royk2 + esig1]   # royk + esig
dt[, dual_se_n := snus1 + esig1]   # snus + esig

dt[, dual_rs := fifelse(dual_rs_n == 2, 1, 0)]
dt[, dual_re := fifelse(dual_re_n == 2, 1, 0)]
dt[, dual_se := fifelse(dual_se_n == 2, 1, 0)]


## Kjønn fordeling
## At least one of them ----------------------------------------------

# Røyk + snus
rs <- calc_percentage_total_ci(dt,
                               outcome_var = "dual_rs",
                               group_vars = "kjonn",
                               denominator_var = "popSub",
                               weight_var = "vekt",
                               include_total = TRUE
                               )

rs[, type := "Snus og sigarettar"]

# Røyk + e-sig
re <- calc_percentage_total_ci(dt,
                               outcome_var = "dual_re",
                               group_vars = "kjonn",
                               denominator_var = "popSub",
                               weight_var = "vekt",
                               include_total = TRUE
                               )

re[, type := "Vape og sigarettar"]

# Snus + e-sig
se <- calc_percentage_total_ci(dt,
                               outcome_var = "dual_se",
                               group_vars = "kjonn",
                               denominator_var = "popSub",
                               weight_var = "vekt",
                               include_total = TRUE
                               )

se[, type := "Vape og snus"]

# Combi av alle 3
rse <- calc_percentage_total_ci(dt,
                               outcome_var = "combiCase",
                               group_vars = "kjonn",
                               denominator_var = "popSub",
                               weight_var = "vekt",
                               include_total = TRUE
                               )


rse[, type := "Vape, snus og sigarettar"]


dualDT <- data.table::rbindlist(list(rs, re, se, rse))

### Alders fordeling -----------------------------------------------------------
## Age ---------------------------------------------------------------
# dt <- torr::group_age_standard(dt, "alder", type = "rusund", new_var = "agecat")

dt <- torr::group_age(dt, var = "alder",
                   breaks = c(16, 25, 55, Inf),
                   labels = c("16-24", "25-54", "55-79"),
                   new_var = "age3str",
                   copy = FALSE)

idxAge <- dt[, .N, keyby = age3str][, idxAge := .I]
dt[idxAge, on = "age3str", idxAge := i.idxAge]


source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-percent-weighted.R")

## Denominator -----------------------------------------------------------------
## Only those who use at lease one of the three
dtt <- dt[popSub == 1]

# Røyk + snus

dtt[combi3 == 3, dual_rs := 3]
rsAge <- proscat_weighted(x = "idxAge", "dual_rs", weight = "vekt", d = dtt, total = TRUE)
rsAge[idxAge, on = "idxAge", alder := i.age3str]
rsAge[is.na(alder), alder := "Totalt"]
rsAge <- rsAge[dual_rs != 0]

rsAge[, type := "Snus og sigarettar"]
rsAge[, type2 := "rs"]

colx <- c("idxAge", "weighted_count", "sum")
rsAge[, (colx) := NULL]
data.table::setnames(rsAge, "dual_rs", "dual")

rsWide <- dcast(rsAge, alder ~ dual, value.var = "percentage")

highchart() |>
  hc_chart(type = "column") |>
  hc_xAxis(categories = rsWide$alder) |>
  hc_plotOptions(column = list(stacking = "normal"))|>
  hc_add_series(name = "dual", data = rsWide$`1`, stack = "stack1")



# Røyk + e-sig

dtt[combi3 == 3, dual_re := 3]
reAge <- proscat_weighted(x = "idxAge", "dual_re", weight = "vekt", d = dtt, total = TRUE)
reAge[idxAge, on = "idxAge", alder := i.age3str]
reAge[is.na(alder), alder := "Totalt"]
reAge <- reAge[dual_re != 0]

reAge[, type := "Vipe og sigarettar"]
reAge[, type2 := "re"]

colx <- c("idxAge", "weighted_count", "sum")
reAge[, (colx) := NULL]
data.table::setnames(reAge, "dual_re", "dual")

reWide <- dcast(reAge, alder ~ dual, value.var = "percentage")

# Snus + e-sig

dtt[combi3 == 3, dual_se := 3]
seAge <- proscat_weighted(x = "idxAge", "dual_se", weight = "vekt", d = dtt, total = TRUE)
seAge[idxAge, on = "idxAge", alder := i.age3str]
seAge[is.na(alder), alder := "Totalt"]
seAge <- seAge[dual_se != 0]

seAge[, type := "Vipe og snus"]
seAge[, type2 := "se"]

colx <- c("idxAge", "weighted_count", "sum")
seAge[, (colx) := NULL]
data.table::setnames(seAge, "dual_se", "dual")

seWide <- dcast(seAge, alder ~ dual, value.var = "percentage")

# Combi av alle 3

dtt[combi3 == 3, combiCase := 3]
rseAge <- proscat_weighted(x = "idxAge", "combiCase", weight = "vekt", d = dtt, total = TRUE)
rseAge[idxAge, on = "idxAge", alder := i.age3str]
rseAge[is.na(alder), alder := "Totalt"]
rseAge <- rseAge[combiCase != 0]

rseAge[, type := "Vape, snus og sigarettar"]
rseAge[, type3 := "rse"]
colx <- c("idxAge", "weighted_count", "sum")
rseAge[, (colx) := NULL]
data.table::setnames(rseAge, "combiCase", "dual")


dualDTAge <- data.table::rbindlist(list(rsAge, reAge, seAge, rseAge))

dualCat <- data.table(
  dual = c(1,3),
  dualStr = c("dual", "trippel")
)

dualDTAge[dualCat, on = "dual", dualstr := i.dualStr]

# # -------------------------------
# # 2. Reshape to wide format
# # -------------------------------
# dtt_wide <- dcast(dualDTAge, alder ~ type2 + dual, value.var = "percentage")

# # -------------------------------
# # 3. Build stacked chart
# # -------------------------------
# highchart() |>
#   hc_chart(type = "column") |>
#   hc_xAxis(categories = dtt_wide$alder) |>
#   hc_plotOptions(column = list(stacking = "normal")) |>
#   hc_add_series(name = "Snus og sigarettar", data = dtt_wide$rs_1, stack = "stack1") |>
#   hc_add_series(name = "Vape, snus og sigarettar", data = dtt_wide$rs_3, stack = "stack1") |>
#   hc_add_series(name = "Vipe og sigarettar", data = dtt_wide$re_1, stack = "stack2") |>
#   hc_add_series(name = "Vape, snus og sigarettar", data = dtt_wide$re_3, stack = "stack2") |>
#   hc_add_series(name = "Vipe og snus", data = dtt_wide$se_1, stack = "stack3") |>
#   hc_add_series(name = "Vape, snus og sigarettar", data = dtt_wide$se_3, stack = "stack3") |>
#   hc_add_series(name = "Vape, snus og sigarettar", data = dtt_wide$rse_3, stack = "stack4") |>
#   hc_title(text = "Stacked Column Chart (highcharter)") |>
#   hc_yAxis(title = list(text = "Value")) |>
#   highcharter::hc_exporting(
#       enabled       = TRUE,
#       filename      = "highdir-figure",
#       accessibility = list(enabled = TRUE)
#     )


# spec_d <- hd_spec(data = dualDTAge, x = "alder", y = "percentage", group = "type")

# hd_make(spec_d, "stacked_column", stack = "type")
