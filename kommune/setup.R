
#Kommunenes forvaltning

library("tcltk")
pkgs <- c("data.table", "rio", "here", "fs")
sapply(pkgs, require, character.only = T)

wpath <- "o:/Prosjekt/Rusdata/Kommunenes forvaltning/Tobakkskadeloven/Data_Stata"
setwd(wpath)

## ----------------

filePath <- file.path(wpath, "org")
fname <- grep("^KFT.*dta$", list.files(filePath), value = TRUE)
files <- file.path(filePath, fname)

DD <- vector("list", length(files))

for (i in seq_along(files)){
  DD[[i]] <- setDT(rio::import(files[i]))
}

## Function --------------

## kommunernummer
komm <- data.table::fread("kommuner2018_til_2023.csv")

## Colnames for kommunenr in different files
komNames <- c("kommunenr", "Utvalg_Kommunenummer",
              "kommunenr", "Utvalg_Kommunenummer",
              "Utvalg_Kommunenummer", "utvalg_kommunenummer")

DD <- mapply(
  FUN = recode_komm,
  dt = DD,
  col = komNames,
  MoreArgs = list(code = komm),
  SIMPLIFY = FALSE
)

## Fylkernummer
fylke <- data.table::fread("kommune_fylke2023.csv")

for (i in seq_along(DD)){
  DD[[i]][fylke, on = .(newKomm = targetCode), newFylke := sourceCode]
}

newFiles <- paste("New", fname, sep = "_")
mapply(function(data, fname) export(data, fname), DD, newFiles)
