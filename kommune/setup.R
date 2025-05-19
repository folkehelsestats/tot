
#Kommunenes forvaltning

library("tcltk")
pkgs <- c("data.table", "rio", "codebook", "sjPlot", "here", "skimr", "fs")
sapply(pkgs, require, character.only = T)

fpath <- "o:/Prosjekt/Rusdata/Kommunenes forvaltning/Tobakkskadeloven/Data_Stata"
setwd(fpath)


dt2018 <- rio::import("KFT_2018_2019.dta")

## sjPlot::view_df(dt2018,
##                 show.type = T,
##                 show.frq = T,
##                 show.prc = T,
##                 show.string.values = F,
##                 file = here::here("R-codes", "codebook_forvaltning.html"))

setDT(dt2018)
vars <- grep("^q10a.*resp$", names(dt2018), value = TRUE)

skimr::skim(dt2018[, ..vars])

dt2018[, bruddtot := rowSums(.SD, na.rm = TRUE), .SDcols = vars]
sum(dt2018$bruddtot, na.rm = T)

## ----------------
files <- grep("dta$", list.files(fpath), value = TRUE)

# Recode kommuner
komm <- data.table::fread("kommuner2018_til_2023.csv")

DD <- vector("list", length(files))

for (i in seq_along(files)){
  DD[[i]] <- setDT(rio::import(files[i]))
}

colall <- lapply(DD, names)

# Find column names common to all datasets
colja <- Reduce(intersect, colall)

# Check the data type of each common column in all datasets
common_col_types <- lapply(colja, function(col) {
  sapply(DD, function(dt) {
    if (col %in% names(dt)) {
      class(dt[[col]])
    } else {
      NA  # If the column is missing in a dataset
    }
  })
})

# Combine results into a data.table for better readability
result <- data.table::data.table(
  Column = colja,
  Types = common_col_types
)

print(result)

# Find unique column names for each dataset
unique_colnames <- lapply(colall, function(cols) setdiff(cols, colja))

# Output results
cat("Common column names across all datasets:\n")
print(colja)

cat("\nUnique column names for each dataset:\n")
for (i in seq_along(unique_colnames)) {
  cat(paste0("Dataset ", i, ":\n"))
  print(unique_colnames[[i]])
}

## Recode kommuner
komm <- data.table::fread("kommuner2018_til_2023.csv")
dx1 <- DD[[1]]

dx1[komm, on = .("kommunenr" = oldCode), newKomm := currentCode]
dx1[is.na(newKomm), newKomm := kommunenr]
