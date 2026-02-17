## Mean number of cigarettes smoked per day
## --------------------------------------

## Path to data and standard setup
## --------------------------------------------------
defaultOpt <- options()
## options(scipen = 999) # disable scientific notation
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/folder-path.R")
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/setup.R")

source("https://raw.githubusercontent.com/folkehelsestats/toir/refs/heads/main/unodc/fun-weighted-unweighted-ci.R")
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-percent-weighted.R")

dt <- readRDS(file.path(Rususdata, "Rusus_2025", "rusus2025_20251126.rds"))
setDT(dt)

setnames(dt, names(dt), tolower(names(dt)))

dt[, .N, keyby = kjonn]

## Check for missing value for vekting
dt[, vekt2 := as.numeric(sub(",", ".", vekt))]
dt[is.na(vekt2), .N]
dt[, nyvekt2 := vekt2/mean(vekt2, na.rm = TRUE)] #standardisert vekt

## Check frequencies for tob1, tob2, tob13, tob14
tvars <- c("tob1", "tob2", "tob13", "tob14")

for (i in tvars) {
    dt[, .N, keyby = get(i)]
}

# Build a list of frequency tables, one per variable
freq_list <- setNames(
  lapply(tvars, function(v) {
    out <- dt[, .N, by = mget(v)]      # mget() preserves the original column name
    setorder(out, -N)                  # sort descending by frequency
    out
  }),
  tvars
)

# Inspect
freq_list[["tob1"]]

## Varables
## ---------
## tob1 - Hender det at du røyker? 1,2
## tob2 - Røyker du daglig eller av og til? 1,2
## tob13 - Har du noen gang røykt daglig? 1,2 Hvis tob2 == 2
## tob14 - Har du noen gang røykt daglig eller av og til? 1,2,3 Hvis tob1 == 2

dt[, roykstatus := fcase(
     tob1 == 1 & tob2 == 1, 1, #Daglig
     tob2 == 2 & tob13 == 1, 2, #Av og til, daglig før
     tob2 == 2 & tob13 == 2, 3, #Av og til, aldri daglig
     tob1 == 2 & tob14 == 1, 4, #Ikke nå, daglig før
     tob1 == 2 & tob14 == 2, 5, #Ikke nå, av og til før
     tob1 == 2 & tob14 == 3, 6, #Aldri røkt
     default = NA
     )]

dt[!is.na(roykstatus), dagroyk := fifelse(roykstatus == 1, 1, 0)]
dt[, .N, keyby = dagroyk]

## Hvor mange sigaretter røyker du gjennomsnittlig pr.dag?
dt[, .N, keyb = tob31a]
dt[, antroyk := fifelse(tob31a %in% c(998, 999), NA, tob31a)]
x <- dt[, .N, keyby = antroyk]
x[!is.na(antroyk), sum(N)]

## Mean sigaretter pr. dag
## ----------------------

library(survey)
des <- svydesign(ids = ~1, weights = ~nyvekt2, data = dt)
model <- svyglm(antroyk ~ kjonn, design = des)

## Estimated marginal means (justerte means)
emmeans::emmeans(model, ~ kjonn)
emmeans::emmeans(model, ~ 1) #total mean, uavhengig av kjønn

em_total  <- summary(emmeans(model, ~ 1))
em_kjonn  <- summary(emmeans(model, ~ kjonn))

library(data.table)

result <- rbind(
  data.table(
    group = "Total",
    adj_mean = em_total$emmean,
    SE = em_total$SE,
    lower_95CI = em_total$lower.CL,
    upper_95CI = em_total$upper.CL
  ),
  data.table(
    group = em_kjonn$kjonn,
    adj_mean = em_kjonn$emmean,
    SE = em_kjonn$SE,
    lower_95CI = em_kjonn$lower.CL,
    upper_95CI = em_kjonn$upper.CL
  )
)

result


## Alternative approach using lm() and emmeans() without survey design
## -------------------------------------------------------------------

library(data.table)
library(emmeans)

# Modell
model <- lm(antroyk ~ kjonn, weights = vekt2, data = dt)
model <- lm(antroyk ~ kjonn, weights = nyvekt2, data = dt)

# 1) Total justert mean
em_total <- emmeans(model, ~ 1)
em_total_smry <- summary(em_total)

dt_total <- data.table(
  group = "Total",
  adj_mean = em_total_smry$emmean,
  SE = em_total_smry$SE,
  lower_95CI = em_total_smry$lower.CL,
  upper_95CI = em_total_smry$upper.CL,
  adj_enhet = em_total_smry$emmean / 1.5,
  SE_enhet = em_total_smry$SE / 1.5,
  lower_enhet = em_total_smry$lower.CL / 1.5,
  upper_enhet = em_total_smry$upper.CL / 1.5
)

# 2) Justert mean for hvert nivå av kjonn (1 og 2)
em_kjonn <- emmeans(model, ~ kjonn)
em_kjonn_smry <- summary(em_kjonn)

dt_kjonn <- data.table(
  group = em_kjonn_smry$kjonn,
  adj_mean = em_kjonn_smry$emmean,
  SE = em_kjonn_smry$SE,
  lower_95CI = em_kjonn_smry$lower.CL,
  upper_95CI = em_kjonn_smry$upper.CL,
  adj_enhet = em_kjonn_smry$emmean / 1.5,
  SE_enhet = em_kjonn_smry$SE / 1.5,
  lower_enhet = em_kjonn_smry$lower.CL / 1.5,
  upper_enhet = em_kjonn_smry$upper.CL / 1.5
)

# 3) Kombiner alt i én tabell
result <- rbind(dt_total, dt_kjonn)
result

## Manually without emmeans
## ----------------------
model <- svyglm(antroyk ~ kjonn, design = des)

newdat <- data.table::data.table(
  kjonn = unique(dt$kjonn)
)

pred <- predict(model, newdata = newdat, se.fit = TRUE)

newdat[, emmean := pred$fit]
newdat[, SE     := pred$se.fit]
newdat[, lower  := emmean - 1.96*SE]
newdat[, upper  := emmean + 1.96*SE]

## Total justert mean
newdat_total <- data.table::data.table(
  kjonn = NA  # model will use intercept-only prediction
)

pred_total <- predict(model, newdata = newdat_total, se.fit = TRUE)

em_total <- data.table::data.table(
  emmean = pred_total$fit,
  SE     = pred_total$se.fit,
  lower  = pred_total$fit - 1.96 * pred_total$se.fit,
  upper  = pred_total$fit + 1.96 * pred_total$se.fit
)
