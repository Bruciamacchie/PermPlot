# ----- Chargement des packages nécessaires -----
suppressMessages({
  library("tidyverse")
  library("sf")
  library("rlang")
  library("readxl")
  library("here")
  library("openxlsx")
})

rm(list = ls()) # suppression des objets dans l'environnement global
rep = "~/pCloud Sync/Packages/PermPlot"

source(paste(rep,"Scripts/Fonctions.R", sep="/"))

# setwd("/Users/maxbruciamacchie/pCloudSync/PPForets")
# CreerDossier(1, "Windstein.xlsx")



