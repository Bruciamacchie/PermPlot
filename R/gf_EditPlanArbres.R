#' Edition des fichiers Rnw.
#' @description Edition des plans d'arbres
#'
#' @return La fonction édite les plans d'arbres par placettes
#'
#' Exemple : 1 RB Grands Monts.pdf.
#' @param repGF = répertoire de travail
#' @author Bruciamacchie Max
#' @import tcltk
#' @import knitr
#' @import stringr
#' @export

gf_EditPlansArbres <- function(repGF, ChoixPlac=NULL) {
  IdArbres <- NULL
  ValArbres <- NULL
  Forets <- NULL
  Placettes <- NULL

  load("Tables/gfDonneesBrutes.Rdata")
  Arbres <- left_join(IdArbres,ValArbres)
  # if(!is.null(ChoixPlac)) {
  #   t1 <- Arbres %>%
  #     filter(NumPlac %in% ChoixPlac)
  # } else {
  #   t1 <- Arbres
  # }

  Liste <- sort(unique(Arbres$NumForet))
  if (is.element(NA,Liste)) warning("NumForet vide d\u00E9tect\u00E9")

  Liste <- paste0(Liste, "-", Forets$Nom[match(Liste,Forets$NumForet)])

  Msg_AllCheck <- "Editer les plans pour toutes les for\u00EAts"
  Liste1 <- c(Msg_AllCheck, Liste)
  ListForet <- tk_select.list(as.character(Liste1), multiple = T, title = "Choisir une ou plusieurs for\u00EAts")
  if (is.element(Msg_AllCheck,ListForet)) {ListForet=Liste}

  for (foret in ListForet) {
    Choix <- as.numeric(str_sub(foret, 1, str_locate(foret, "-")[,1]-1))
    NomForet <- Forets$Nom[match(Choix,Forets$NumForet)]

    Name <- paste0(Choix, "-",NomForet)
    Name_rep <- gsub(" ", "", NomForet, fixed = T)
    Name_rep <- gsub("'", "", Name_rep, fixed = T)
    Name_rep <- str_replace_all(Name_rep, "\u00EA", "e")
    Name_rep <- str_replace_all(Name_rep, "\u00E2", "a")
    Name_rep <- str_replace_all(Name_rep, "\u00E9", "e")
    Name_rep <- str_replace_all(Name_rep, "\u00E8", "e")
    Name_rep <- str_replace_all(Name_rep, "\u00FB", "u")
    Name_rep <- str_replace_all(Name_rep, "\u00EE", "i")
    Name_rep <- str_replace_all(Name_rep, "\u00F4", "o")

    dir.create(paste0("Out/",Choix,"-",Name_rep), showWarnings = F)
    dir.create(paste0("Out/",Choix,"-",Name_rep,"/Remesures"), showWarnings = F)
    dir.create(paste0("Out/",Choix,"-",Name_rep,"/Remesures/PlansArbres"), showWarnings = F)
    dir.create(paste0("Out/",Choix,"-",Name_rep,"/Remesures/PlansArbres/Figures"), showWarnings = F)

    repFigures <- paste0(repGF,"/Out/",Choix,"-",Name_rep,"/Remesures/PlansArbres/Figures/") #changement : répertoire sauvegarde des figures
    repPdf <- paste0(dirname(repFigures),"/") # répertoire sauvegarde du/des pdf
    output <- paste0(repPdf,paste0(Choix,"_PlansArbres.tex"))

    suppressWarnings(knit2pdf(input="Template/gf_PlanArbres.Rnw",
                              output = output,
                              compiler = "pdflatex",
                              envir = parent.frame(),
                              # clean=TRUE,
                              quiet = TRUE))

    if (paste0(Choix,"_PlansArbres.aux") %in% list.files(repPdf)) {
      file.remove(paste0(Choix,"_PlansArbres.aux"))
    }
    if (paste0(Choix,"_PlansArbres.out") %in% list.files(repPdf)) {
      file.remove(paste0(Choix,"_PlansArbres.out"))
    }
  }

  tk_messageBox(type="ok",
                message="Edition(s) des plans d'arbres par placettes termin\u00E9(s)",
                icon="info")
}
