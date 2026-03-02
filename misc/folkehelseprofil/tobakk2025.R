## Datasett til Folkehelseprofilen 2025:
## ----------------------------------------- Tobakk Analysen for tidligere
## levering kan ses i "tobakkstall til profilene.do" filen, men det er ikke
## tydelig hvordan definisjon for royk1, royk2 og royk3 er gjort, så jeg har
## valgt å gjøre det på nytt basert på tob1, tob2, tob13 og tob14 som brukes i
## OECD filen. Det kan være at det er noen små forskjeller i definisjonen.

## Path to data and standard setup
## --------------------------------------------------
defaultOpt <- options()
## options(scipen = 999) # disable scientific notation
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/folder-path.R")
source("https://raw.githubusercontent.com/folkehelsestats/rusus/refs/heads/main/setup.R")

source("https://raw.githubusercontent.com/folkehelsestats/toir/refs/heads/main/unodc/fun-weighted-unweighted-ci.R")
source("https://raw.githubusercontent.com/folkehelsestats/toa/refs/heads/main/rusund/functions/fun-percent-weighted.R")

## -----------------------------------------
## 2025 data
## -----------------
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

## Rrøykerstatus 1-6: 1 = Røyker nå, 2 = Røykte før, 3 = Ikke nå, 4 = Aldri røkt
## for Folkehelseprofilen 2025.
dt[, roykstat2 := fcase(roykstatus == 1, 1, #Røyker nå
                        roykstatus %in% c(2, 3), 2, #Røykte før
                        roykstatus %in% c(4,5), 3, #Ikke nå
                        roykstatus == 6, 4, #Aldri røkt
                        default = NA)]

dt[, roykstat2chr := factor(roykstat2, levels = 1:4, labels = c("daglig", "avogtil", "tidligere", "aldri"))]

## Age groups for Folkehelseprofilen 2025
breaks <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
labels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
            "50-54", "55-59", "60-64", "65-69", "70-74", "75+")

dt <- torr::group_age(dt, "alder",
                      breaks = breaks,
                      labels = labels,
                      missing_values = c(998, 999),  # "no answer", "don't know"
                      new_var = "age5",
                      copy = TRUE)

## Variable for utdanning - utdann_4grp
## Variable for fylke - fylke_2025

vars <- c("roykstat2chr", "kjonn", "age5", "utdann_4gr", "fylke_2025")

dx <- groupingsets(
  dt,
  j = .(N = .N),
  by = vars,
  sets = list(
    vars
    )
)

royk <- dcast(dx, kjonn + age5 + utdann_4gr + fylke_2025 ~ roykstat2chr, value.var = c("N"))
royk[, `NA` := NULL]

fwrite(royk, "misc//folkehelseprofil/roykstatus_2025.csv")



### -----------------
### Data for 2024
### -----------------
DT <- readRDS(file.path(Rususdata, "Rusus_2024", "rus2024.rds"))
DT <- as.data.table(DT)

setnames(DT, names(DT), tolower(names(DT)))

DT[, .N, keyby = kjonn]

## Check for missing value for vekting
DT[, vekt2 := as.numeric(sub(",", ".", vekt))]
DT[is.na(vekt2), .N]
DT[, nyvekt2 := vekt2/mean(vekt2, na.rm = TRUE)] #standardisert vekt

## Check frequencies for tob1, tob2, tob13, tob14
tvars <- c("tob1", "tob2", "tob13", "tob14")

for (i in tvars) {
    DT[, .N, keyby = get(i)]
}

# Build a list of frequency tables, one per variable
freq_list <- setNames(
  lapply(tvars, function(v) {
    out <- DT[, .N, by = mget(v)]      # mget() preserves the original column name
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

DT[, roykstatus := fcase(
     tob1 == 1 & tob2 == 1, 1, #Daglig
     tob2 == 2 & tob13 == 1, 2, #Av og til, daglig før
     tob2 == 2 & tob13 == 2, 3, #Av og til, aldri daglig
     tob1 == 2 & tob14 == 1, 4, #Ikke nå, daglig før
     tob1 == 2 & tob14 == 2, 5, #Ikke nå, av og til før
     tob1 == 2 & tob14 == 3, 6, #Aldri røkt
     default = NA
     )]

## Rrøykerstatus 1-6: 1 = Røyker nå, 2 = Røykte før, 3 = Ikke nå, 4 = Aldri røkt
## for Folkehelseprofilen 2025.
DT[, roykstat2 := fcase(roykstatus == 1, 1, #Røyker nå
                        roykstatus %in% c(2, 3), 2, #Røykte før
                        roykstatus %in% c(4,5), 3, #Ikke nå
                        roykstatus == 6, 4, #Aldri røkt
                        default = NA)]

DT[, roykstat2chr := factor(roykstat2, levels = 1:4, labels = c("daglig", "avogtil", "tidligere", "aldri"))]

## Age groups for Folkehelseprofilen 2025
breaks <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
labels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
            "50-54", "55-59", "60-64", "65-69", "70-74", "75+")

DT <- torr::group_age(DT, "alder",
                      breaks = breaks,
                      labels = labels,
                      missing_values = c(998, 999),  # "no answer", "don't know"
                      new_var = "age5",
                      copy = TRUE)

## Variable for utdanning - utdann_4grp
## Variable for fylke - fylke_2025

vars <- c("roykstat2chr", "kjonn", "age5", "utdann_4gr", "fylke_2024")

DX <- groupingsets(
  DT,
  j = .(N = .N),
  by = vars,
  sets = list(
    vars
    )
)

royk24 <- dcast(DX, kjonn + age5 + utdann_4gr + fylke_2024 ~ roykstat2chr, value.var = c("N"))
royk24[, `NA` := NULL]

fwrite(royk24, "misc//folkehelseprofil/roykstatus_2024.csv")
