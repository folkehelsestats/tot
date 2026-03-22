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
dt[, roykPop1 := data.table::fifelse(tob1  %in% 1:3, 1L, 0L, na = 0L)]
dt[, roykPop2 := data.table::fifelse(tob2  %in% 1:3, 1L, 0L, na = 0L)]
dt[, snusPop1  := data.table::fifelse(tob60 %in% 1:3, 1L, 0L, na = 0L)]
dt[, esigPop1  := data.table::fifelse(tobe %in% 1:3, 1L, 0L, na = 0L)]

popCols <- c("roykPop2", "snusPop1", "esigPop1")
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

# dt[, .N, keyby = combiCat]

# Dual bruker, combi bruker inkludert ------------------------------------------
dt[, dual_rs_n := royk2 + snus1]   # royk + snus
dt[, dual_re_n := royk2 + esig1]   # royk + esig
dt[, dual_se_n := snus1 + esig1]   # snus + esig

dt[, dual_rs := fifelse(dual_rs_n == 2, 1, 0)]
dt[, dual_re := fifelse(dual_re_n == 2, 1, 0)]
dt[, dual_se := fifelse(dual_se_n == 2, 1, 0)]


## Age ---------------------------------------------------------------
dt <- torr::group_age_standard(dt, "alder", type = "rusund", new_var = "agecat")

## At least one of them ----------------------------------------------

# Røyk + snus
rs <- calc_percentage_total_ci(dt,
                               outcome_var = "dual_rs",
                               group_vars = "kjonn",
                               denominator_var = "totPop",
                               weight_var = "vekt",
                               include_total = TRUE
                               )

# Røyk + e-sig
re <- calc_percentage_total_ci(dt,
                               outcome_var = "dual_re",
                               group_vars = "kjonn",
                               denominator_var = "totPop",
                               weight_var = "vekt",
                               include_total = TRUE
                               )

# Snus + e-sig
se <- calc_percentage_total_ci(dt,
                               outcome_var = "dual_se",
                               group_vars = "kjonn",
                               denominator_var = "totPop",
                               weight_var = "vekt",
                               include_total = TRUE
                               )
