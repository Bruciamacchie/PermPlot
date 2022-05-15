
# -------------- Noms de stockage
CreerDossier <- function(no, fname){
  prefix <- paste0("excel/", no)
  test_FILE0 <<- file.path(prefix, fname)
  test_FILE1 <<- file.path(prefix, "out/job1/gfDonneesBrutes.Rdata")
  test_FILE2 <<- file.path(prefix, "out/job3/gfTablesBrutes.Rdata")
  test_FILE3 <<- file.path(prefix, "out/job4/gfTablesElaboreesPlac.Rdata")
  test_FILE4 <<- file.path(prefix, "out/job5/gfTablesElaborees.Rdata")
  testing <<- TRUE
}

# -------------- Verification du classeur ------------------
# file = test_FILE0
gf_VerifClasseur <- function(file) {
  data(Tables)
  data(Colonnes)
  msg = NULL

  sheets_LIST <- excel_sheets(file)
  sheets_VALID <- Tables$Sheet[which(Tables$Sheet %in% sheets_LIST)]
  sheets_INVALID <- setdiff(Tables$Sheet, sheets_VALID)
  if (length(sheets_INVALID)) {
    msg <- paste("le classeur ne contient pas les feuilles :", sheets_INVALID)
  }




  NomCol <- data.frame()
  for (i in 1:length(sheets_VALID))
    noms <-
    tab <- data.frame()

  sheets_NBCOl <- length(sheets_NAMES)
  column_VALID <- which(sheets_LIST %in% sheets_NAMES)
  column_INVALID <- sheets_NAMES[which(!sheets_NAMES %in% sheets_LIST)]
  if (length(column_VALID) < sheets_NBCOl) {
    stop(
      "Les noms d'onglet du classeur Excel en import ne sont pas corrects.\n\nRappel : liste des onglets devant figurer dans le classeur (",
      sheets_NBCOl, " au total) =\n'", paste0(sheets_NAMES, collapse = "','"),
      "'\nIl manque les onglets :\n", paste0(column_INVALID, collapse = ", ")
    )
  }
}

# liste des fichiers Excel et choix
ChoixDossier <- function() {
  listFich <- dir_ls(regexp = "xlsx", recurse = T)

}






