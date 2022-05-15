# TableNoms
Tables   <- read_excel("~/pCloud Sync/Packages/PermPlot/inst/NomsTables.xlsx", sheet="Feuilles")
Colonnes <- read_excel("~/pCloud Sync/Packages/PermPlot/inst/NomsTables.xlsx", sheet="Colonnes")

usethis::use_data(Tables, overwrite = T)
usethis::use_data(Colonnes, overwrite = T)
