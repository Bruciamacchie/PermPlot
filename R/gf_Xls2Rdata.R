#' Import des données d'inventaire (modèle PermGF - utilisation locale)
#' @description Permet d'importer les données d'inventaire contenues dans le classeur Excel de saisie.
#'
#' @return  Les données importées sont archivées au format .RData dans le fichier gfDonneesBrutes.RData
#'
#' @author Bruciamacchie Max, Demets Valentin
#'
#' @param repGF = répertoire de travail
#' @param fich = classeur(s) excel(s) contenant les données d'inventaire
#'
#' @import tcltk
#' @import tools
#' @import readxl
#'
#' @export

gf_Xls2Rdata <- function(repGF, fich) {

  # ---------- Répertoire de travail ----------
  setwd(repGF)

  # ---------------- Import des données ----------
  ListDF <- c("Forets","Cycles","Echantillonnages","Placettes","Regroups",
              "Tiges","Reges","PCQM","Cercles","BMSLineaires","BMSCercles",
              "Coords","Reperes",
              "Quals","Tarifs","AcctD","Tiers","Essences","EssReg","EssInd",
              "Prix", # PouvoirAchat, VariationPU,
              "CodeEcolos","CodeDurete","CodeEcorce","CodeTypoArbres",
              "CoeffBOBIBE","Listes")
  NomSheet <- c("Foret","Cycle","Echantillonnage","Placettes","Regroup",
                "Arbres","Rege","PCQM","Cercle","BMSLineaire","BMSCercle",
                "Coord","Reperes",
                "Qual","Tarif","AccD","Tiers","Essences","EssReg","EssInd",
                "Prix",
                "CodeEcologie","CodeDurete","CodeEcorce","CodeTypoArbres",
                "BOBIBE","Listes")
  for (i in 1:length(ListDF)) {
    x <- read_excel(fich, sheet=NomSheet[i])
    assign(ListDF[i], x)
    print(paste0("# ----- Import de la feuille ",NomSheet[i]," termin\u00E9"))
  }
  print(paste0("################## Import du classeur ",basename(fich)," termin\u00E9"))

  # ---------- Corrections et réorganisation de tables ----------
  Cycles <- Cycles %>%
    rename("Annee"="Ann\u00E9e")
  if (dim(Cycles)[1] > 0) {
    Cycles <- Cycles %>%
      mutate(Date=as.Date(Date, format = "%d.%m.%Y"))
  }

  Tiges <- Tiges %>%
    mutate(Dist = round(Dist,1))

  # --- Tiges (données feuille Arbres)
  if (dim(Placettes)[1] > 0) {

    Tiges <- full_join(Placettes[,c("NumForet","NumPlac","Cycle")], Tiges)
    # mettre full_join pour le cas où il y aurait une placette de la feuille Arbres qui
    # aurait été oubliée dans la feuille Placettes

    # Mise en forme des tables arbres
    IdArbres <- Tiges %>%
      dplyr::select(NumForet,NumPlac,NumArbre,Essence,Azimut,Dist)
    IdArbres <- dplyr::distinct(IdArbres)
    IdArbres$IdArbre <- 1:dim(IdArbres)[1]

    Arbres <- left_join(Tiges,IdArbres) %>%
      mutate(CodeEcolo=tolower(CodeEcolo))
    ValArbres <- subset(Arbres, select=c("IdArbre",names(Arbres)[!names(Arbres) %in% names(IdArbres)]))
  } else {
    IdArbres <- data.frame()
    ValArbres <- data.frame()
    Arbres <- data.frame()
  }

  # --- BMSLineaires
  # Traitement des vides et oublis (Angle de 'BMSLineaires', Pente et CoeffPente de 'Placettes')
  if (dim(BMSLineaires)[1] > 0) {
    BMSLineaires$Angle[which(is.na(BMSLineaires$Angle))] <- 0
  }

  # --- Placettes
  Placettes$Pente[which(is.na(Placettes$Pente))] <- 0
  Placettes$CoeffPente[which(is.na(Placettes$CoeffPente))] <- 1

  # --- Reges
  if (dim(Reges)[1] > 0) {
    Reges <- filter(Reges,
                    !is.na(Essence)) %>%
      mutate(Recouv=ifelse(is.na(Recouv),0,Recouv),
             Class1=ifelse(is.na(Class1),0,Class1),
             Class2=ifelse(is.na(Class2),0,Class2),
             Class3=ifelse(is.na(Class3),0,Class3),
             Rejet=ifelse(is.na(Rejet),0,Rejet),
             Abroutis=ifelse(is.na(Abroutis),0,Abroutis))
  }
  # --- AcctD
  if (dim(AcctD)[1] > 0) {
    AcctD$Cycle <- 1 # l'IFN correspond au cycle 1 avant c'\u00E9tait 0
    AcctD <- group_by(AcctD,
                      NumForet,Essence) %>%
      mutate(Cycle=1,
             AcctDmoy=mean(AccD,na.rm=T),
             AcctD=ifelse(is.na(AccD),AcctDmoy,AccD),
             AcctDmoy=NULL) %>%
      ungroup()
  }

  # ---  Duplicata des tables Prix et Qual
  # (si plusieurs forêts mélangées, risque de redondance des notations => duplicati)
  Quals <- distinct(Quals)
  Prix  <- distinct(Prix)

  # ---------- Sauvegarde ----------
  rm(Tiges)
  dir.create("Tables", showWarnings = F)
  save(Forets,Cycles,Echantillonnages,Placettes,Regroups,
       IdArbres,ValArbres,Reges,PCQM,Cercles,
       BMSLineaires,BMSCercles,Coords,Reperes,
       Quals,Tarifs,AcctD,Tiers,Essences,EssReg,EssInd,
       Prix,CodeEcolos,CodeDurete,CodeEcorce,CodeTypoArbres,
       CoeffBOBIBE,Listes,
       file = "Tables/gfDonneesBrutes.Rdata")
  # ---------- FIN ----------
  print("Importation termin\u00E9e. Fichier d'inventaire archiv\u00E9")
}
