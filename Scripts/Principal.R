
#################### Installation si nécessaire puis activation des différentes librairies ###############
listLibrairies <- c("tidyverse","sf","readxl","xlsx","tcltk","knitr")

for (i in 1:length(listLibrairies)) {
  gf_verif_library(listLibrairies[i])
}

#################### Choix du fichier et répertoire de travail ###############
inv = file.choose()
inv = "Brin.xlsx"


repGF <- dirname(inv)
setwd(repGF)
# # le code ci-dessous permet d'aller directement au répertoire qui contient le fichier source
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("..")
# repGF = getwd()

# source("Scripts/gf_Xls2Rdata.R")
# source("Scripts/gf_Calculs.R")
# source("Scripts/gf_AgregArbres.R")


#################### Lancement des différentes étapes ###############
gf_Xls2Rdata(repGF,inv)
# load("Tables/gfDonneesBrutes.Rdata")
gf_Calculs()
gf_AgregArbres()

load("Tables/gfTablesElaboreesPlac.RData")
write.xlsx(gfPlaClasse, file = "Resultats.xlsx",
           sheetName="gfPlaClasse", append=F)
write.xlsx(gfPlaDendro, file = "Resultats.xlsx",
           sheetName="gfPlaDendro", append=F)

source("Scripts/gf_EditPlanArbres.R")
# gf_EditPlansArbres(repGF)


