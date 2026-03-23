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

re[, type := "Vipe og sigarettar"]

# Snus + e-sig
se <- calc_percentage_total_ci(dt,
                               outcome_var = "dual_se",
                               group_vars = "kjonn",
                               denominator_var = "popSub",
                               weight_var = "vekt",
                               include_total = TRUE
                               )

se[, type := "Vipe og snus"]

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
                   new_var = "age3",
                   copy = FALSE)

# Røyk + snus
rsAge <- calc_percentage_total_ci(dt,
                               outcome_var = "dual_rs",
                               group_vars = "age3",
                               denominator_var = "popSub",
                               weight_var = "vekt",
                               include_total = FALSE
                               )

rsAge[, type := "Snus og sigarettar"]

# Røyk + e-sig
reAge <- calc_percentage_total_ci(dt,
                               outcome_var = "dual_re",
                               group_vars = "age3",
                               denominator_var = "popSub",
                               weight_var = "vekt",
                               include_total = FALSE
                               )

reAge[, type := "Vipe og sigarettar"]

# Snus + e-sig
seAge <- calc_percentage_total_ci(dt,
                               outcome_var = "dual_se",
                               group_vars = "age3",
                               denominator_var = "popSub",
                               weight_var = "vekt",
                               include_total = FALSE
                               )

seAge[, type := "Vipe og snus"]

# Combi av alle 3
rseAge <- calc_percentage_total_ci(dt,
                               outcome_var = "combiCase",
                               group_vars = "age3",
                               denominator_var = "popSub",
                               weight_var = "vekt",
                               include_total = FALSE
                               )


rseAge[, type := "Vape, snus og sigarettar"]


dualDTAge <- data.table::rbindlist(list(rsAge, reAge, seAge, rseAge))
