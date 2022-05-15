#' Aggrégation des arbres à l'échelle de la placette.
#' @description Sous-programme permettant d'aggréger toutes les variables à l'échelle de la placette.
#' Il fournit de nombreux tableaux croisés.
#' @return Les différentes tables sont enregistrées dans le dossier Tables
#'
#' @author Bruciamacchie Max
#' @import tidyverse
#'
#' @export

gf_AgregArbres <- function() {
  if (!("gfTablesBrutes.RData" %in% list.files("Tables"))) {
    stop("Utiliser au préalable la fonction gf_Import")
  } else {load("Tables/gfTablesBrutes.RData")}
  ############################## Tableaux arbres #####################
  ListePlacettes <- Placettes %>%
    dplyr::select(NumPlac)
  NbPlac <- dim(ListePlacettes)[1]

  # ------------- Essences principales
  gfPlaDendro <- arbres %>%
    group_by(NumPlac) %>%
    summarise(across(c(Nha,Gha,Vha,VcHa,VpHa,Gain), sum))
  gfPlaDendro <- ListePlacettes %>%
    left_join(gfPlaDendro, by = "NumPlac")
  gfPlaDendro[is.na(gfPlaDendro)] <- 0

  gfPlaCat <- arbres %>%
    left_join(EssReg, by = c("NumForet","Essence")) %>%
    group_by(NumPlac,Cat) %>%
    summarise(across(c(VcHa), sum))
  gfPlaCat <- ListePlacettes %>%
    left_join(gfPlaCat, by = "NumPlac")

  gfPlaEss <- arbres %>%
    left_join(EssReg, by = c("NumForet","Essence")) %>%
    group_by(NumPlac,EssReg) %>%
    summarise(across(c(VcHa), sum))

  gfPlaEssCat <- arbres %>%
    left_join(EssReg, by = c("NumForet","Essence")) %>%
    group_by(NumPlac,EssReg,Cat) %>%
    summarise(across(c(Nha,Gha,Vha), sum))
  gfPlaEssCat <- ListePlacettes %>%
    left_join(gfPlaEssCat, by = "NumPlac") %>%
    mutate(Nha = ifelse(is.na(Nha), 0, Nha),
           Gha = ifelse(is.na(Gha), 0, Gha),
           Vha = ifelse(is.na(Vha), 0, Vha))

  gfPlaEssClasse <- arbres %>%
    left_join(EssReg, by = c("NumForet","Essence")) %>%
    group_by(EssReg, Classe) %>%
    summarise(across(c(Nha,Gha,Vha), sum)) %>%
    mutate(Nha = Nha / NbPlac)

  gfPlaEssClasseQual <- arbres %>%
    left_join(EssReg, by = c("NumForet","Essence")) %>%
    group_by(EssReg, Classe, Reg2) %>%
    summarise(across(c(Nha,Gha,Vha), sum)) %>%
    mutate(Nha = Nha / NbPlac) %>%
    filter(EssReg %in% c("CHE", "HET")) %>%
    filter(!is.na(Reg2))

  gfPlaClasse <- arbres %>%
    group_by(NumPlac, Classe) %>%
    summarise(across(c(Nha,Gha,Vha), sum))


#   # --------- Tableaux par placettes
#   gfPla                <- sumArbres(tab,var,regroup)
#   gfPlaDendro          <- sumArbres(den,var,regroup)
#   gfPlaEss             <- sumArbres(tab,var,c(regroup,"Essence"))
#   gfPlaEssDen          <- sumArbres(den,var,c(regroup,"Essence"))
#   gfPlaEssReg          <- sumArbres(tab,var,c(regroup,"EssReg"))
#   gfPlaEssRegDen       <- sumArbres(den,var,c(regroup,"EssReg"))
#   gfPlaEssPrinc        <- sumArbres(tab,var,c(regroup,"EssPrinc"))
#   gfPlaEssPrincDen     <- sumArbres(den,var,c(regroup,"EssPrinc"))
#   gfPlaQual2           <- sumArbres(den,var,c(regroup,"Reg2"))
#   gfPlaCat             <- sumArbres(tab,var,c(regroup,"Cat"))
#   gfPlaCatDen          <- sumArbres(den,var,c(regroup,"Cat"))
#   gfPlaCatEssReg       <- sumArbres(tab,var,c(regroup,"Cat","EssReg"))
#   gfPlaCatEssRegDen    <- sumArbres(den,var,c(regroup,"Cat","EssReg"))
#   gfPlaCatEssPrinc     <- sumArbres(tab,var,c(regroup,"Cat","EssPrinc"))
#   gfPlaCatEssPrincDen  <- sumArbres(den,var,c(regroup,"Cat","EssPrinc"))
#   gfPlaEssRegQual      <- sumArbres(tab,var,c(regroup,"EssReg","Reg1"))
#   gfPlaEssRegQualDen   <- sumArbres(den,var,c(regroup,"EssReg","Reg1"))
#   gfPlaClasse          <- sumArbres(tab,var,c(regroup,"Classe"))
#   gfPlaClasseQual1     <- sumArbres(tab,var,c(regroup,"Classe","Reg1"))
#   gfPlaClasseQual1Den  <- sumArbres(den,var,c(regroup,"Classe","Reg1"))
#   gfPlaClasseQual2     <- sumArbres(tab,var,c(regroup,"Classe","Reg2"))
#   gfPlaClasseQual2Den  <- sumArbres(den,var,c(regroup,"Classe","Reg2"))
#   gfPlaClasseEssReg    <- sumArbres(tab,var,c(regroup,"Classe","EssReg"))
#   gfPlaClasseEssRegDen <- sumArbres(den,var,c(regroup,"Classe","EssReg"))
#   gfPlaCatQual1        <- sumArbres(tab,var,c(regroup,"Cat","Reg1"))
#   gfPlaCatQual1Den     <- sumArbres(den,var,c(regroup,"Cat","Reg1"))
#   gfPlaCatQual2        <- sumArbres(tab,var,c(regroup,"Cat","Reg2"))
#   gfPlaCatQual2Den     <- sumArbres(den,var,c(regroup,"Cat","Reg2"))
#   gfPlaEssRegQual1     <- sumArbres(tab,var,c(regroup,"EssReg","Reg1"))
#   gfPlaEssRegQual1Den  <- sumArbres(den,var,c(regroup,"EssReg","Reg1"))
#   gfPlaEssRegQual2     <- sumArbres(tab,var,c(regroup,"EssReg","Reg2"))
#   gfPlaEssRegQual2Den  <- sumArbres(den,var,c(regroup,"EssReg","Reg2"))
#   gfPlaPerches         <- sumArbres(Perches,var,regroup)
#   gfPlaPerchesEss      <- sumArbres(Perches,var,c(regroup,"EssReg"))
#   gfPlaPerchesQual     <- sumArbres(Perches,var,c(regroup,"Reg1"))
#
#   t1 <- subset(gfPlaClasseQual1, Reg1!="D")
#   gfPlaClasseQualABC   <- sumArbres(t1,var,c(regroup,"Classe"))
#   t1 <- subset(gfPlaClasseQual1, Reg1=="D")
#   gfPlaClasseQualD   <- sumArbres(t1,var,c(regroup,"Classe"))
#   if(dim(Gestion)[1] > 0) {
#     gfPlaGestion <- sumArbres(Gestion,var,regroup)
#   } else {
#     gfPlaGestion  <- data.frame()
#   }
#   if(dim(Gestion)[1] > 0) {
#     gfPlaGestionCoupe  <- sumArbres(Gestion,var,c(regroup,"Coupe"))
#   } else {
#     gfPlaGestionCoupe  <- data.frame()
#   }
#   if(dim(Gestion)[1] > 0) {
#     gfPlaGestionClasseEssReg <- sumArbres(Gestion,var,c(regroup,"Classe","EssReg"))
#   } else {
#     gfPlaGestionClasseEssReg  <- data.frame()
#   }
#   if(dim(PF)[1] > 0) {
#     gfPlaPFClasseEssReg <- sumArbres(PF,var,c(regroup,"Classe","EssReg"))
#   } else {
#     gfPlaPFClasseEssReg  <- data.frame()
#   }
#
#   ############################## Tableaux BMS #######################
#   # ------------------- Echantillonnage linéaire
#   if(dim(BMSLineaires)[1] > 0) {
#     temp <- subset(BMSLineaires, select=c(1:6,10,12))
#     temp$Classe <- floor(temp$Diam/5+0.5)*5
#     temp$StadeE <- floor(temp$Stade/10)
#     temp$StadeD <- temp$Stade - temp$StadeE*10
#     BMSolLin <- summaryBy(Vha ~ NumForet + NumPlac + Cycle + Classe + StadeE + StadeD,
#     											 data=temp, FUN= sum, na.rm=T, keep.names=T)
#     # BMSolLin <- merge(BMSolLin, Placettes[,1:3], by=c("NumForet","NumPlac"))
#     gfPlaBMS <- summaryBy(Vha ~ NumForet + NumPlac + Strate + Cycle,
#                           data=BMSolLin, FUN= sum, na.rm=T, keep.names=T)
#     gfPlaBMSClasse <- summaryBy(Vha ~ NumForet + NumPlac + Strate + Cycle + Classe,
#                           data=BMSolLin, FUN= sum, na.rm=T, keep.names=T)
#     gfPlaBMSStadeE <- summaryBy(Vha ~ NumForet + NumPlac + Strate + Cycle + StadeE,
#                                 data=BMSolLin, FUN= sum, na.rm=T, keep.names=T)
#     gfPlaBMSStadeD <- summaryBy(Vha ~ NumForet + NumPlac + Strate + Cycle + StadeD,
#                                 data=BMSolLin, FUN= sum, na.rm=T, keep.names=T)
#
#   } else {
#     gfPlaBMS <- data.frame()
#     gfPlaBMSClasse <- data.frame()
#     gfPlaBMSStadeE <- data.frame()
#     gfPlaBMSStadeD <- data.frame()
#   }
#
#   # --------- BMS echantillonnage cercle
#   if(dim(BMSsup30)[1] > 0) {
#     BMSsup30$StadeE <- floor(BMSsup30$Stade/10)
#     BMSsup30$StadeD <- BMSsup30$Stade - BMSsup30$StadeE*10
#     BMSsup30V     <- summaryBy(Vha ~ NumForet + NumPlac + Strate + Cycle + Classe + StadeD + StadeE,
#                                data=BMSsup30, FUN=sum, keep.names=T)
#     BMSsup30VEss  <- summaryBy(Vha ~ NumForet + NumPlac + Strate + Cycle + Essence,
#                                data=BMSsup30, FUN=sum, keep.names=T)
#     BMSsup30VClasse <- summaryBy(Vha ~ NumForet + NumPlac + Strate + Cycle + Classe,
#                                  data=BMSsup30, FUN=sum, keep.names=T, na.rm=T)
#     BMSsup30VStadeD <- summaryBy(Vha ~ NumForet + NumPlac + Strate + Cycle + StadeD,
#                                  data=BMSsup30, FUN=sum, keep.names=T, na.rm=T)
#     BMSsup30VStadeE <- summaryBy(Vha ~ NumForet + NumPlac + Strate + Cycle + StadeE,
#                                  data=BMSsup30, FUN=sum, keep.names=T, na.rm=T)
#   } else {
#     BMSsup30V       <- data.frame()
#     BMSsup30VEss    <- data.frame()
#     BMSsup30VClasse <- data.frame()
#     BMSsup30VStadeD <- data.frame()
#     BMSsup30VStadeE <- data.frame()
#   }
#
#   ############################## Tableaux BMP #######################
#   if(dim(BMP)[1] > 0) {
#     temp <- BMP
#     temp$Stade[which(is.na(temp$Stade))] <- 11
#     temp$Classe <- floor(temp$Diam/5+0.5)*5
#     temp$StadeE <- floor(temp$Stade/10)
#     temp$StadeD <- temp$Stade - temp$StadeE*10
#     # temp <- merge(temp, Placettes[,1:3], by=c("NumForet","NumPlac"))
#     gfPlaBMP <- summaryBy(Gha + Vha ~ NumForet + NumPlac + Strate + Cycle,
#   									data=temp, FUN= sum, na.rm=T, keep.names=T)
#     gfPlaBMPClasse <- summaryBy(Gha + Vha ~ NumForet + NumPlac + Strate + Cycle + Classe,
#                                 data=temp, FUN= sum, na.rm=T, keep.names=T)
#     gfPlaBMPStadeE <- summaryBy(Gha + Vha ~ NumForet + NumPlac + Strate + Cycle + StadeE,
#                           data=temp, FUN= sum, na.rm=T, keep.names=T)
#     gfPlaBMPStadeD <- summaryBy(Gha + Vha ~ NumForet + NumPlac + Strate + Cycle + StadeD,
#                           data=temp, FUN= sum, na.rm=T, keep.names=T)
#   } else {
#     gfPlaBMP <- data.frame()
#     gfPlaBMPClasse <- data.frame()
#     gfPlaBMPStadeE <- data.frame()
#     gfPlaBMPStadeD <- data.frame()
#   }
#
#   ############################## Tableaux codes ecologiques #######################
# # ncol <- dim(Codes)[2]
# # EcoPla <- aggregate(Codes[,8:(ncol-1)], by = list(Codes$NumForet,Codes$Cycle,Codes$NumPlac), FUN = sum)
# # names(EcoPla)[1:3] <- c("NumForet","Cycle","NumPlac")
# # EcoPla <- merge(EcoPla,Placettes[,1:5], by = c("NumForet","NumPlac"), all.x=T)
# #
# # EcoPlaClasse <- aggregate(Codes[,8:(ncol-1)], by = list(Codes$NumForet,Codes$Cycle,Codes$NumPlac,Codes$Classe), FUN = sum)
# # names(EcoPlaClasse)[1:4] <- c("NumForet","Cycle","NumPlac","Classe")
# # EcoPlaClasse <- merge(EcoPlaClasse,Placettes[,1:5], by = c("NumForet","NumPlac"), all.x=T)
# #
# # Codes <- merge(Codes, EssReg, by = c("NumForet","Essence"), all.x=T)
# # EcoPlaEss <- aggregate(Codes[,8:(ncol-1)], by = list(Codes$NumForet,Codes$Cycle,Codes$NumPlac,Codes$EssReg), FUN = sum)
# # names(EcoPlaEss)[1:4] <- c("NumForet","Cycle","NumPlac","EssReg")
# # EcoPlaEss <- merge(EcoPlaEss,Placettes[,1:5], by = c("NumForet","NumPlac"), all.x=T)
#
#
#   ############################## Tableaux taillis #######################
# gfPlaTaillis <- data.frame(NumForet=integer(),NumPlac=integer(),Strate=character(),Cycle=integer(),
#                       Nha=numeric(),Gha=numeric(),Vha=numeric())
# gfPlaTaillisEss <- data.frame(NumForet=integer(),NumPlac=integer(),Strate=character(),Cycle=integer(),
#                            Essence=character(),Nha=numeric(),Gha=numeric(),Vha=numeric())
# gfPlaTaillisEssReg <- data.frame(NumForet=integer(),NumPlac=integer(),Strate=character(),Cycle=integer(),
#                               EssReg=character(),Nha=numeric(),Gha=numeric(),Vha=numeric())
#
# if (dim(Taillis)[1] >0) {
#   temp <- Taillis
#   # temp <- merge(temp, Placettes[,c(1:4)], by=c("NumForet","NumPlac"))
#   temp <- merge(temp, EssReg, by=c("NumForet","Essence"), all.x=T)
#   names(temp)[which(names(temp)=="Poids")] <- "Nha"
#   gfPlaTaillisEss <- summaryBy(Nha + Gha + Vha ~  NumForet + NumPlac + Strate + Cycle + Essence,
#                                data=temp, FUN= sum, na.rm=T, keep.names=T)
#   gfPlaTaillisEssReg <- summaryBy(Nha + Gha + Vha ~  NumForet + NumPlac + Strate + Cycle + EssReg,
#                                   data=temp, FUN= sum, na.rm=T, keep.names=T)
#   gfPlaTaillis <- summaryBy(Nha + Gha + Vha ~  NumForet + NumPlac + Strate + Cycle,
#                             data=temp, FUN= sum, na.rm=T, keep.names=T)
# }
#
#
#   ############################## Tableaux regeneration ####################
#   gfPlaRege <- data.frame(NumForet=integer(),NumPlac=integer(),Strate=character(),Cycle=integer(),
#                       Recouv=numeric(),Classe1=numeric(),Classe2=numeric(),Classe3=numeric())
#   gfPlaRegeEss <- data.frame(NumForet=integer(),NumPlac=integer(),Strate=character(),Cycle=integer(),
#                       Essence=character(),Recouv=numeric(),Classe1=numeric(),Classe2=numeric(),Classe3=numeric())
#   gfPlaRegeEssReg <- data.frame(NumForet=integer(),NumPlac=integer(),Strate=character(),Cycle=integer(),
#                       EssReg=character(),Recouv=numeric(),Classe1=numeric(),Classe2=numeric(),Classe3=numeric())
#   if (dim(Reges)[1] >0) {
#     gfPlaRegeEss <- summaryBy(Recouv + Classe1 + Classe2 + Classe3 ~
#                                 NumForet + NumPlac + Strate + Cycle + Essence + EssReg,
#                              data=Reges, FUN= sum, na.rm=T, keep.names=T)
#
#     gfPlaRegeEss <- merge(gfPlaRegeEss, Echantillonnages[,c("NumForet","Cycle","Strate","NbSousPlac")],
#                    by=c("NumForet","Cycle","Strate"), all.x = T, sort=F)
#     gfPlaRegeEss$Recouv <- gfPlaRegeEss$Recouv/gfPlaRegeEss$NbSousPlac
#     gfPlaRegeEss$NbSousPlac <- NULL
#
#     gfPlaRegeEss$Total <- gfPlaRegeEss$Recouv + gfPlaRegeEss$Classe1 + gfPlaRegeEss$Classe2 + gfPlaRegeEss$Classe3
#     gfPlaRegeEss <- gfPlaRegeEss[which(gfPlaRegeEss$Total >0),]
#     gfPlaRegeEss$Total <- NULL
#     # gfPlaRegeEss <- merge(gfPlaRegeEss, Placettes[,c(1:4)], by=c("NumForet","NumPlac","Strate"), all.x=T)
#     gfPlaRegeEss <- merge(gfPlaRegeEss, EssReg, by=c("NumForet","Essence"), all.x=T)
#     gfPlaRegeEss <- merge(gfPlaRegeEss, Echantillonnages[,c(1:3,14)], by=c("NumForet","Cycle","Strate"), all.x=T)
#     gfPlaRegeEss[,6:9] <- gfPlaRegeEss[,6:9]/gfPlaRegeEss$NbSousPlac
#     gfPlaRegeEssReg <- summaryBy(Recouv + Classe1 + Classe2 + Classe3 ~
#                                    NumForet + NumPlac + Strate + Cycle + EssReg,
#                              data=gfPlaRegeEss, FUN= sum, na.rm=T, keep.names=T)
#     gfPlaRege <- summaryBy(Recouv + Classe1 + Classe2 + Classe3 ~ NumForet + NumPlac + Strate + Cycle,
#                              data=gfPlaRegeEss, FUN= sum, na.rm=T, keep.names=T)
#   }
#
#   ############################## Sauvegarde ######################
#   gfDen <- den
#   save(Coords,Perches,IdPlacettes,Regroup,Placettes,Forets,
#     gfPla,gfPlaDendro,gfPlaEss,gfPlaEssDen,gfPlaEssReg,gfPlaEssRegDen,
#     gfPlaCat,gfPlaCatDen,gfPlaCatEssReg,gfPlaCatEssRegDen,gfPlaEssRegQual,gfPlaEssRegQualDen,
#     gfPlaClasse,gfPlaClasseQual1,gfPlaClasseQual1Den,gfPlaClasseQual2,gfPlaClasseQual2Den,
#     gfPlaClasseEssReg,gfPlaClasseEssRegDen,gfPlaCatQual1,gfPlaCatQual1Den,gfPlaCatQual2,
#     gfPlaCatQual2Den,gfPlaEssRegQual1,gfPlaEssRegQual1Den,gfPlaEssRegQual2,gfPlaEssRegQual2Den,
#     gfPlaPerches,gfPlaPerchesEss,gfPlaPerchesQual,
#     gfPlaTaillis, gfPlaTaillisEss, gfPlaTaillisEssReg,
#     gfPlaRege,gfPlaRegeEss,gfPlaRegeEssReg,
#     gfPlaClasseQualABC,gfPlaClasseQualD,
#     gfPlaGestion,gfPlaGestionCoupe,gfPlaGestionClasseEssReg,gfPlaPFClasseEssReg,
#     gfPlaBMS,gfPlaBMSClasse,gfPlaBMSStadeE,gfPlaBMSStadeD,
#     gfPlaBMP,gfPlaBMPClasse,gfPlaBMPStadeE,gfPlaBMPStadeD,
#     gfPlaEssPrinc,gfPlaEssPrincDen,gfPlaCatEssPrincDen,gfPlaCatEssPrinc,
#     gfPlaQual2,
#     file="Tables/gfTablesElaboreesPlac.RData")


  save(gfPlaClasse,gfPlaDendro,gfPlaEssCat,gfPlaEssClasse,gfPlaEssClasseQual,
       file="Tables/gfTablesElaboreesPlac.RData")
}





