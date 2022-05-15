#' Calcul des variables dendrométriques, économiques et écologiques.
#'
#' @description Cette fonction commence par calculer le poids de chaque arbre,
#' puis pour chacun d'eux, calcule les variables dendrométriques, économiques et écologiques ramenées à l'hectare.
#'
#' @return La fonction construit les tables suivantes.
#'
#' arbres, Reges, Taillis, Reperes, BMSLineaires, BMSsup30, BMP, Codes
#' Elles sont enregistrées dans le dossier Tables sous le nom : gfTablesBrutes.RData.
#'
#' @param TauxR = taux d'actualisation (0.03 par défaut)
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#' @export


gf_Calculs <- function(TauxR=0.03) {
  ########################### Préparation #####################
  #--------------- Chargement tables
  if (!("gfDonneesBrutes.Rdata" %in% list.files("Tables"))) {
    stop("Utiliser au préalable la fonction gf_Xls2Rdata")
  } else {load("Tables/gfDonneesBrutes.RData")}

  ########################### Création table arbre ###############################
  arbres <- IdArbres %>%
    right_join(ValArbres, by="IdArbre") %>%
    mutate(Diam = (Diam1 + Diam2)/2,
           Classe = floor(Diam/5+0.5)*5,
           Cat = cut(Diam, breaks = c(0, 17.5, 27.5, 47.5, 67.5,200),
                     labels = c("PER", "PB", "BM", "GB","TGB"), include.lowest = T, right = F)) %>%
    left_join(Placettes[,1:7], by=c("NumForet","NumPlac","Cycle")) %>%
    left_join(Echantillonnages[,1:13], by=c("NumForet","Cycle","Strate")) %>%
    filter(!is.na(NumArbre)) %>%
    left_join(Tarifs, by=c("NumForet","Strate","Cycle","Essence")) %>%
    left_join(Quals, by=c("Qual"="Nom")) %>%
    filter(Diam>=7.5) %>%
    left_join(Prix, by=c("Essence","Classe","Reg1"="Qual")) %>%
    arrange(NumForet,Cycle,NumPlac,Azimut)

  pos <- which(is.na(arbres$Qual))
  if (length(pos) > 0) arbres$Qual[pos] <- "C"

  ########################### Calcul du poids ###############################
  print("Calcul du poids")
  arbres$Nha <- NA
  # ------------ Cercles
  pos <- which(arbres$Diam1 < arbres$DiamLim1)
  if (length(pos) > 0) arbres$Nha[pos] <- 0

  pos <- which(is.na(arbres$Nha) &
                 arbres$Diam1 >= arbres$DiamLim1 &
                 arbres$Diam < arbres$DiamLim &
                 arbres$Dist <= arbres$Rayon1 * arbres$CoeffPente)
  if (length(pos) > 0) arbres$Nha[pos] <- 10000/pi/arbres$Rayon1[pos]^2

  # ------------ Angle fixe
  pos <- which(is.na(arbres$Nha) & arbres$Diam1 >= arbres$Dist * arbres$Coeff * 100)
  if (length(pos) > 0) arbres$Nha[pos] <- 10^8*arbres$Coeff[pos]^2/pi/arbres$Diam1[pos]^2

  # ------------ Arbres limites
  pos <- which(is.na(arbres$Nha))
  arbres[pos,"Nha"] <- 0
  rm(pos)

  ########################### Donnees /ha ############################
  arbres$Gha <- pi*arbres$Diam1^2/40000 * arbres$Nha
  # -------- Volume gestionnaire
  arbres$DiamSup <- arbres$Diam + 5
  arbres$ClasseSup <- arbres$Classe + 5

  print("Calcul du volume gestionnaire")
  arbres$Vha     <- NA
  arbres$VhaSup  <- NA
  pos <- which(arbres$TypeTarif=="SchR")
  if (length(pos) > 0) {
    arbres$Vha[pos] <-  5/70000*(8+arbres$NumTarif[pos])*(arbres$Diam[pos]-5)*
      (arbres$Diam[pos]-10)*arbres$Nha[pos]
    arbres$VhaSup[pos] <- 5/70000*(8+arbres$NumTarif[pos])*(arbres$DiamSup[pos]-5)*
      (arbres$DiamSup[pos]-10)*arbres$Nha[pos]}
  pos <- which(arbres$TypeTarif=="SchI")
  if (length(pos) > 0) {
    arbres$Vha[pos] <-  5/80000*(8+arbres$NumTarif[pos])*(arbres$Diam[pos]-2.5)*
      (arbres$Diam[pos]-7.5)*arbres$Nha[pos]
    arbres$VhaSup[pos] <- 5/80000*(8+arbres$NumTarif[pos])*(arbres$DiamSup[pos]-2.5)*
      (arbres$DiamSup[pos]-7.5)*arbres$Nha[pos]}
  pos <- which(arbres$TypeTarif=="SchL")
  if (length(pos) > 0) {
    arbres$Vha[pos] <-  5/90000*(8+arbres$NumTarif[pos])*(arbres$Diam[pos]-5)*
      arbres$Diam[pos]*arbres$Nha[pos]
    arbres$VhaSup[pos] <- 5/90000*(8+arbres$NumTarif[pos])*(arbres$DiamSup[pos]-5)*
      arbres$DiamSup[pos]*arbres$Nha[pos]}
  pos <- which(arbres$TypeTarif=="SchTL")
  if (length(pos) > 0) {
    arbres$Vha[pos] <-  5/101250*(8+arbres$NumTarif[pos])*arbres$Diam[pos]^2*arbres$Nha[pos]
    arbres$VhaSup[pos] <- 5/101250*(8+arbres$NumTarif[pos])*arbres$DiamSup[pos]^2*arbres$Nha[pos]}
  arbres$Vha[which(arbres$Vha<0)]       <- 0
  arbres$VhaSup[which(arbres$VhaSup<0)] <- 0
  rm(pos)

  # -------- Calcul du volume IFN
  print("Calcul du volume géométrique bois fort tige")
  arbres$VhaIFN <- NA
  pos <- which(arbres$TypeTarifIFN=="SchR")
  if (length(pos) > 0) {
    arbres$VhaIFN[pos] <-  5/70000*(8+arbres$NumTarifIFN[pos])*(arbres$Diam[pos]-5)*
      (arbres$Diam[pos]-10)*arbres$Nha[pos]}
  pos <- which(arbres$TypeTarifIFN=="SchI")
  if (length(pos) > 0) {
    arbres$VhaIFN[pos] <-  5/80000*(8+arbres$NumTarifIFN[pos])*(arbres$Diam[pos]-2.5)*
      (arbres$Diam[pos]-7.5)*arbres$Nha[pos]}
  pos <- which(arbres$TypeTarifIFN=="SchL")
  if (length(pos) > 0) {
    arbres$VhaIFN[pos] <-  5/90000*(8+arbres$NumTarifIFN[pos])*(arbres$Diam[pos]-5)*
      arbres$Diam[pos]*arbres$Nha[pos]}
  pos <- which(arbres$TypeTarifIFN=="SchTL")
  if (length(pos) > 0) {
    arbres$VhaIFN[pos] <-  5/101250*(8+arbres$NumTarifIFN[pos])*arbres$Diam[pos]^2*arbres$Nha[pos]}
  arbres$VhaIFN[which(arbres$VhaIFN<0)] <- 0

  # ------ Valeur consommation
  print("Calcul de la valeur de consommation")
  arbres$VcHa <- arbres$Vha*arbres$PU

  # ------ Taux
  arbres$TauxV <- 0
  pos <- which(arbres$Vha>0)
  arbres$TauxV[pos] <- log(arbres$VhaSup[pos]/arbres$Vha[pos])/5
  rm(pos)
  # ------ Valeur potentielle
  print("Calcul de la valeur potentielle")
  PrixSup <- Prix
  names(PrixSup)[4] <- "PUSup"
  arbres <- merge(arbres, PrixSup, by.x = c("Essence", "ClasseSup", "Reg1"),
                  by.y = c("Essence", "Classe", "Qual"), all.x = T)
  rm(PrixSup) # suppression de l'objet
  ########################### Accroissement sur le diamètre ################
  print("Calcul des accroissements en diamètre")
  if (max(Cycles$Cycle) > 1) {
    DispAnnee <- dcast(Cycles[,c(1:2,4)], NumForet ~ Cycle, value.var="Année")
    a <- subset(arbres, select= c(IdArbre,NumForet,Strate,NumPlac,NumArbre,Cycle,Essence,Diam))
    ListeCycles <- sort(unique(a$Cycle))
    NbCycle <- length(ListeCycles)
    b <- dcast(a, NumForet + NumPlac + NumArbre + Essence + Strate ~ Cycle, value.var=c("Diam"), mean)

    names(b)[6:(NbCycle+5)] <- paste0("Diam",1:NbCycle)
    if (dim(DispAnnee)[2] > 1+NbCycle) {DispAnnee[,(2+NbCycle):dim(DispAnnee)[2]] <- NULL} #supprime les cycles renseignés dans l'échantillonnage mais en trop dans les tables
    b <- merge(b, DispAnnee, by = c("NumForet"), all.x = T)
    names(b)[(6+NbCycle):(2*NbCycle+5)] <- paste0("An",1:NbCycle)

    for (i in 1:(NbCycle-1)) {
      b$temp <- (b[,6+i]-b[,5+i])/(b[,6+i+NbCycle]-b[,5+i+NbCycle])
      names(b)[2*NbCycle+5+i] <- i+1
    }

    b <- b[,!colnames(b) %in% paste0("An",1:NbCycle)] # Nettoyage
    AcctDBrut <- b

    pos <- c()
    for (i in 1:(NbCycle-1)) {
      #erreurs :
      pos <- c(pos,which(b[,5+NbCycle+i]<0))
      if (length(pos)>0) {stop("incohérences dans les diamètres")}
      b$temp <- b[,5+NbCycle+i]
      b$Classe <- floor(b[,6+i]/5+0.5)*5
      AcctDMoyen <- summaryBy(temp ~ NumForet + Essence + Classe, b, FUN = mean, na.rm=T)
      names(AcctDMoyen)[4] <-"AcctDmoy"
      AcctDMoyen <- AcctDMoyen[which(!is.na(AcctDMoyen$Classe)),]
      AcctDMoyenEss <- summaryBy(temp ~ NumForet + Essence, b, FUN = mean, na.rm=T)
      names(AcctDMoyenEss)[3] <-"AcctDEss"
      AcctDMoyenForet <- summaryBy(temp ~ Essence + Classe, b, FUN = mean, na.rm=T)
      names(AcctDMoyenForet)[3] <-"AcctDForet"
      AcctDMoyenForetEss <- summaryBy(temp ~ Essence, b, FUN = mean, na.rm=T)
      names(AcctDMoyenForetEss)[2] <-"AcctDForetEss"

      c <- merge(AcctDMoyen, AcctDMoyenEss, by = c("NumForet","Essence"), all.x=T)
      c <- merge(c, AcctDMoyenForet, by = c("Essence","Classe"), all.x=T)
      c <- merge(c, AcctDMoyenForetEss, by = c("Essence"), all.x=T)
      b <- merge(b, c, by = c("NumForet","Essence","Classe"), all.x=T)

      b$temp <- ifelse (is.na(b$temp),b$AcctDmoy, b$temp)
      b$temp <- ifelse (is.na(b$temp),b$AcctDEss, b$temp)
      b$temp <- ifelse (is.na(b$temp),b$AcctDForet, b$temp)
      b$temp <- ifelse (is.na(b$temp),b$AcctDForetEss, b$temp)
      b[,6+NbCycle+i] <- b$temp
      b <- b[,!colnames(b) %in% c("Classe","temp","AcctDmoy","AcctDEss","AcctDForet","AcctDForetEss","Cycle")] # Nettoyage

      if (i==1) {
        b$temp <- b[,5+NbCycle+i]
        b$Classe <- floor(b[,5+i]/5+0.5)*5
        b <- merge(b, c, by = c("NumForet","Essence","Classe"), all.x=T) #remplacer par AcctDmoy ou temp ?
        b <- b[order(-b$NumPlac,b$NumArbre),]
        b$temp <- ifelse (!is.na(b$Diam1), b$temp,NA) # suppression des valeurs inutiles
        # il faut envisager les cas où on n'a pas de valeur d'accroissement au cycle 2 => piocher les valeurs
        b$temp <- ifelse (is.na(b$temp),b$AcctDmoy, b$temp)
        b$temp <- ifelse (is.na(b$temp),b$AcctDEss, b$temp)
        b$temp <- ifelse (is.na(b$temp),b$AcctDForet, b$temp)
        b$temp <- ifelse (is.na(b$temp),b$AcctDForetEss, b$temp)
        b$"1" <- ifelse (!is.na(b$Diam1), b$temp,NA)
        b <- b[,!colnames(b) %in% c("Classe","temp","AcctDmoy","AcctDEss","AcctDForet","AcctDForetEss","Cycle")] # Nettoyage
      }
    }

    c <- subset(b,select=c(1,3,4,(6+NbCycle):dim(b)[2]))
    AcctD <- melt(c, id=1:3, na.rm =T,variable.name="Cycle",value.name="AccD")
    AcctD$Cycle <- as.numeric(as.character(AcctD$Cycle))

    arbres <- merge(arbres, AcctD, by = c("NumForet","NumPlac","NumArbre","Cycle"), all.x = T)
  } else {
    arbres <- merge(arbres, AcctD, by = c("NumForet","Strate","Cycle","Essence","Classe"), all.x = T)
  }
  pos <- which(is.na(arbres$AccD))
  arbres$AccD[pos] <- 0.3

  ########################### Accroissement en valeur et G ################
  print("Calcul des accroissements en valeur et G")
  pos <- which(arbres$Type=="M")
  if (length(pos)>0) {arbres[pos,"AccD"] <- 0}
  arbres$TauxPU  <- log(arbres$PUSup/arbres$PU)/5
  arbres$Taux    <- (arbres$TauxPU + arbres$TauxV) * arbres$AccD
  arbres$AcctV   <- arbres$TauxV * arbres$Vha * arbres$AccD
  arbres$Gain    <- arbres$Taux * arbres$VcHa
  arbres$VpHa    <- arbres$Gain/TauxR
  arbres$AcctG   <- pi/20000*arbres$AccD*arbres$Diam*arbres$Nha #changement2

  ########################### Accroissement en volume ################
  print("Calcul des accroissements en volume")
  if(max(arbres$Cycle) > 1){
    #---------- Calcul de l'accroissement volume par période
    t <- arbres[!is.na(arbres$Vha),c("NumForet","NumPlac","NumArbre","Vha","Cycle")]
    t <- merge(t, Cycles[,c("NumForet","Cycle","Année")], by=c("NumForet","Cycle"), all.x=T)
    #changement
    t.m <- melt(t, id=c("NumForet","NumPlac","NumArbre","Cycle"))
    t.c <- dcast(t.m, NumForet + NumPlac + NumArbre ~ Cycle + variable)
    NbCycles <- (dim(t.c)[2]-3)/2

    for (i in 2:NbCycles) {
      t.c$temp <- (t.c[,(2+i)+2]-t.c[,(2+i)]) / (t.c[,(3+i)+2]-t.c[,(3+i)])

      # Cas des arbres exploités
      posExpl <- which(is.na(t.c[,(2+i)+2]) & !is.na(t.c[,(2+i)]))
      t.c[posExpl,"temp"] <- 0

      # Cas des passages à la futaie :
      posPF <- which(!is.na(t.c[,(2+i)+2]) & is.na(t.c[,(2+i)]))
      t.c[posPF,3+i] <- Cycles$Année[Cycles$NumForet %in% t.c[posPF,"NumForet"] & # On retrouve l'année d'inventaire
                                       Cycles$Cycle %in% substr(names(t.c)[3+i],0,1)]
      t.c[posPF,"temp"] <- t.c[posPF,(2+i)+2] / (t.c[posPF,(3+i)+2]-t.c[posPF,(3+i)])

      names(t.c)[names(t.c) %in% "temp"] <- i
    }
    t.c <- t.c[,c(1:3,dim(t.c)[2]:(dim(t.c)[2]-(NbCycle-2)))]
    t2 <- melt(t.c, id=c("NumForet","NumPlac","NumArbre"), variable.name="Cycle", value.name="AcctVper")
    t2$Cycle <- as.numeric(as.character(t2$Cycle))

    arbres <- merge(arbres, t2, by=c("NumForet","NumPlac","NumArbre","Cycle"), all.x=T)
  }

  ########################### Regeneration ###########################
  if (dim(Reges)[1] > 0) {
    # ----- Donnees hectare
    print("Traitement des données de régénération")
    Reges <- Reges %>%
      left_join(Placettes[,1:10], by=c("NumForet","NumPlac","Cycle")) %>%
      left_join(Echantillonnages, by=c("NumForet","Cycle","Strate")) %>%
      mutate(Classe1 <- Class1* 10000/pi/RayonSousPlac^2,
             Classe2 <- Class2* 10000/pi/RayonSousPlac^2,
             Classe3 <- Class3* 10000/pi/RayonSousPlac^2,)

    # Reges <- merge(Reges, Placettes[,1:10], by=c("NumForet","NumPlac","Cycle"), all.x=T, sort=F)
    # # Reges <- merge(Reges, Placettes[,1:9,12], by=c("NumForet","NumPlac","Cycle"), all.x=T, sort=F)
    # Reges <- merge(Reges, Echantillonnages[,c("NumForet","Cycle","Strate","NbPlac","NbSousPlac","RayonSousPlac")],
    #                by=c("NumForet","Cycle","Strate"), all.x = T, sort=F)
    # Reges$Classe1 <- Reges$Class1* 10000/pi/Reges$RayonSousPlac^2
    # Reges$Classe2 <- Reges$Class2* 10000/pi/Reges$RayonSousPlac^2
    # Reges$Classe3 <- Reges$Class3* 10000/pi/Reges$RayonSousPlac^2

    Reges$EssValor <- 0
    if(dim(EssInd)[1] >0) {
      for (i in 1:dim(Forets)[1]) {
        EssEnTour <- subset(EssInd, NumForet==Forets$NumForet[i], select="Essence")
        pos <- which(Reges$NumForet==Forets$NumForet[i] & is.element(Reges$Essence, t(EssEnTour)))
        Reges$EssValor[pos] <- 1
      }
    }

    Reges <- Reges %>%
      mutate(Surf = ifelse(Class1 + Class2 + Class3 >= 5, 1, 0),
             EssValor = Surf*EssValor) %>%
      group_by(NumForet, Cycle, NumPlac, SsPlac, Essence) %>%
      summarise(Surf = sum(Surf, na.rm=T),
                EssValor = sum(EssValor, na.rm = T))

    # Reges$Surf <- ifelse(Reges$Class1 + Reges$Class2+ Reges$Class3 >=5, 1, 0)
    # Reges$Surf <- Reges$Surf/Reges$NbPlac
    # SurfRege <- summaryBy(Surf + Surf*EssValor ~ NumForet+ Cycle + NumPlac + SsPlac + Essence ,
    #                       data=Reges, FUN= sum, na.rm=T, keep.names=T)

    # Reges <- subset(Reges, select=c("NumForet","Strate","Cycle","NumPlac","SsPlac",
    #                                 "Parcelle","Groupe","Station",
    #                                 "Essence","EssValor","Recouv","Classe1","Classe2","Classe3","Abroutis"))
  } else {
    Reges <- data.frame(NumForet=integer(),Strate=integer(),Cycle=integer(),NumPlac=integer(),SsPlac=character(),
                        Parcelle=character(),Groupe=character(),Typologie=character(),Groupe1=character(),
                        Station=character(),Essence=character(),Essvalor=character(),Recouv=numeric(),
                        Classe1=numeric(),Classe2=numeric(),Classe3=numeric(),Abroutis=numeric())
  }


  # ########################### Taillis ################
  print("Traitement du taillis")
  # ---- PCQM
  if (dim(PCQM)[1] > 0) {
    Taillis <- PCQM %>%
      filter(Population=="Taillis") %>%
      dplyr::select(NumForet:Cycle,Quart:Diam)

    if (dim(Taillis)[1] >0) {
      Taillis <- Taillis %>%
        left_join(Placettes[,c(1:5)], by=c("NumForet","NumPlac","Cycle"))

      Corr <- data.frame(Coeff=table(Taillis$NumForet,Taillis$Cycle,Taillis$NumPlac))
      names(Corr) <- c("NumForet","Cycle","NumPlac","Nbre")
      Corr <- Corr %>%
        mutate(NumForet = as.numeric(as.character(NumForet)),
               Cycle = as.numeric(as.character(Cycle)),
               NumPlac = as.numeric(as.character(NumPlac)),
               Vides = 4 - Nbre,
               Surf = Vides*quantile(Taillis$Dist, probs=0.95)^2)

      Tab <- Taillis %>%
        mutate(Distance = Dist^2) %>%
        group_by(NumForet,NumPlac,Cycle) %>%
        summarise(Nha = sum(Distance)) %>%
        left_join(Corr, by=c("NumForet","Cycle","NumPlac")) %>%
        mutate(Nha = Nha + Surf,
               Nha = 10000*3/pi/Nha) %>%
        dplyr::select(-Surf)

      Taillis <- Taillis %>%
        left_join(Tab,  by=c("NumForet","NumPlac","Cycle")) %>%
        mutate(Gha = pi/40000*Diam^2 * Nha,
               Vha = Gha * 7) %>%
        dplyr::select(NumForet,NumPlac,Cycle,Strate,Essence,Diam,Nha,PoidsPlacette,Gha,Vha)


      # Tab <- summaryBy(Distance^2 ~ NumForet + NumPlac + Cycle,
      #                  data=Taillis, FUN= sum, na.rm=T, keep.names=T)
      # names(Tab)[4] <- "Nha"
      # Tab <- merge(Tab, Corr[,c("NumForet","Cycle","NumPlac","Surf")], by=c("NumForet","Cycle","NumPlac"), all.x=T)
      # Tab$Nha <- Tab$Nha + Tab$Surf
      # Tab$Nha <- 10000*3/pi/Tab$Nha
      # Tab$Surf <- NULL
      # Taillis <- merge(Taillis, Tab, by=c("NumForet","NumPlac","Cycle"))
      #
      # Taillis$Gha <- pi/40000*Taillis$Diam^2 * Taillis$Nha
      # Taillis$Vha <- Taillis$Gha * 7
      # Taillis <- subset(Taillis, select=c("NumForet","NumPlac","Cycle","Strate","Essence","Diam",
      #                                     "Nha","PoidsPlacette","Gha","Vha"))
    }
  }
  # # ---- Cercles
  # if (dim(Cercles)[1] > 0) {
  #   Cercles <- merge(Cercles, Placettes[,c(1:5)], by=c("NumForet","NumPlac","Cycle"), all.x=T, sort=F)
  #   Taillis <- merge(Cercles, Echantillonnages[,c("NumForet","Cycle","Strate","Taillis")],
  #                    by=c("NumForet","Strate","Cycle"), all.x=T, sort=F)
  #   Taillis$Nha <- 10000/pi/Taillis$Taillis/Taillis$Taillis
  #   Taillis$Gha <- pi/40000*Taillis$Diam^2 * Taillis$Nha
  #   Taillis$Vha <- Taillis$Gha * 7
  #   Taillis <- subset(Taillis, select=c("NumForet","NumPlac","Cycle","Essence","Diam",
  #                                       "PoidsPlacette","Nha","Gha","Vha"))
  # }
  # if(dim(Cercles)[1] == 0 & dim(PCQM)[1] == 0) {
  #   Taillis <- data.frame(NumForet=integer(), NumPlac=integer(), Cycle=integer(), Essence=character(),
  #                         Diam=numeric(), Nha=numeric(), Gha=numeric(), Vha=numeric())
  # }
  #
  # ########################### Bois mort au sol ###########
  # print("Traitement des données de bois mort")
  #
  # # lineaire
  # if (dim(BMSLineaires)[1] > 0) {
  #   LongLig <- Echantillonnages[1,21]
  #   BMSLineaires$Vha <- pi^2*BMSLineaires$Diam^2/8/LongLig/cos(BMSLineaires$Angle*pi/180)
  # } else {
  #   BMSLineaires <- data.frame()
  # }
  #
  # # Cercle
  # if (dim(BMSCercles)[1] > 0) {
  #   Rayon <- Echantillonnages$Cercle
  #   t <- BMSCercles
  #   t$DiamIni[is.na(t$DiamIni)] <- 0
  #   t$DiamFin[is.na(t$DiamFin)] <- 0
  #   t$DiamMed[is.na(t$DiamMed)] <- 0
  #   t$Vha <- 0
  #   t$Classe <- 0
  #   # ---- formule de Huber
  #   pos <- which((t$DiamIni + t$DiamFin)==0)
  #   t$Vha[pos] <- pi/40000*t$DiamMed[pos]^2*t$Longueur[pos] * 10000/pi/20^2
  #   t$Classe[pos] <- floor(t$DiamMed[pos]/5+0.5)*5
  #   # ---- formule de Smalian
  #   pos <- which((t$DiamIni+ t$DiamFin)!=0 & t$DiamMed==0)
  #   t$Vha[pos] <- pi/80000*(t$DiamIni[pos]^2+t$DiamFin[pos]^2)*t$length[pos] * 10000/pi/20^2
  #   t$Classe[pos] <- floor((t$DiamIni[pos]+t$DiamFin[pos])/2/5+0.5)*5
  #   # ---- formule de Newton
  #   pos <- which((t$DiamIni+ t$DiamFin)!=0 & t$DiamMed!=0)
  #   t$Vha[pos] <- pi/240000*(t$DiamIni[pos]^2+t$DiamFin[pos]^2 + 4*t$DiamMed[pos]^2)*t$Longueur[pos] * 10000/pi/20^2
  #   t$Classe[pos] <- floor((t$DiamIni[pos]+t$DiamFin[pos]+t$DiamIni[pos])/3/5+0.5)*5
  #   BMSsup30 <- t
  # } else {
  #   BMSsup30 <- data.frame()
  # }
  #
  #
  # ########################### Bois mort sur pied ###########
  # if (dim(PCQM)[1] > 0) {
  #   BMP <- subset(PCQM, Population=="BMP")
  #   if (dim(BMP)[1] > 0) {
  #     BMP <- merge(BMP, Placettes[,c(1:5)], by=c("NumForet","NumPlac","Cycle"), all.x=T)
  #     Corr <- data.frame(Coeff=table(BMP$NumForet,BMP$Cycle,BMP$NumPlac))
  #     names(Corr) <- c("NumForet","Cycle","NumPlac","Nbre")
  #     Corr$Vides <- 4-Corr$Nbre
  #     Corr$Surf <- Corr$Vides*quantile(BMP$Distance, probs=0.95)^2
  #     Tab <- summaryBy(Distance^2 ~ NumForet + NumPlac + Cycle,
  #                      data=BMP, FUN= sum, na.rm=T, keep.names=T)
  #     names(Tab)[4] <- "Nha"
  #     Tab <- merge(Tab, Corr[,c("NumForet","Cycle","NumPlac","Surf")], by=c("NumForet","Cycle","NumPlac"), all.x=T)
  #     Tab$Nha <- Tab$Nha + Tab$Surf
  #     Tab$Nha <- 10000*3/pi/Tab$Nha
  #     Tab$Surf <- NULL
  #     BMP <- merge(BMP, Tab, by=c("NumForet","NumPlac","Cycle"), all.x=T, sort=F)
  #     BMP$Gha <- 0
  #     pos <- which(BMP$Type != "S")
  #     BMP$Gha[pos] <- pi/40000*BMP$Diam[pos]^2 * BMP$Nha[pos]
  #
  #     pos <- which(BMP$Type=="A")
  #     BMP$Vha <- 0
  #     BMP$Vha[pos] <- 5/90000*(8+6)*BMP$Diam[pos]*(BMP$Diam[pos]-5)*BMP$Nha[pos]
  #     pos <- which(BMP$Type=="V")
  #     BMP$Vha[pos] <- pi/40000*(BMP$Diam[pos]-BMP$Haut[pos]/2)^2*BMP$Haut[pos]*BMP$Nha[pos]
  #     BMP <- subset(BMP, select=c("NumForet","NumPlac","Cycle","Essence","Diam","Stade","Gha","Vha"))
  #     rm(Tab,pos,Corr)
  #   } else {
  #     BMP <- data.frame()
  #   }
  # } else {
  #   BMP <- data.frame()
  # }
  # ########################### Note ecologique ######
  # print("Traitement des données écologiques")
  #
  # Codes <- subset(arbres, NoteEcolo != "", c(NumForet,Strate,Cycle,NumPlac,NumArbre,Essence,Diam,NoteEcolo,Nha))
  # # ---- Liste des niveaux
  # if (dim(Codes)[1] > 0) {
  #   Niveaux <- c("g1","g2","g3","h1","h2","h3","f1","f2","f3","a1","a2","a3","p1","p2","p3","i1","i2","i3","c1",
  #                "c2","c3","e1","e2","e3","b1","b2","b3","l1","l2","l3","r1","r2","r3","k","ts","tc","tn","tx","d")
  #   # ---- Decomposition
  #   NbCodes <-length(Niveaux)
  #   for (i in 1:NbCodes) {
  #     Codes$Temp <- ifelse (str_detect(Codes$NoteEcolo, Niveaux[i]),Codes$Nha,0)
  #     if (sum(Codes$Temp, na.rm=T) == 0) {
  #       Codes$Temp <- NULL
  #     } else {
  #       names(Codes)[dim(Codes)[2]] <- Niveaux[i]
  #     }
  #   }
  #   Codes$NoteEcolo <- NULL
  #   Codes$Nha <- NULL
  #   Codes$Classe <- floor(Codes$Diam/5+0.5)*5
  # } else {
  #   Codes <- data.frame()
  # }

  ########################### Nettoyage et sauvegarde ################
  if (max(arbres$Cycle) == 1){ #changement
    arbres <- arbres %>%
      dplyr::select(NumForet,NumPlac,NumArbre,Cycle,Strate,Coupe,IdArbre,Azimut,Dist,
                    Essence,Qual,Type,Reg1,Reg2,Haut,Stade,Limite,Diam1,Diam2,Diam,Classe,Cat,
                    PU,PoidsPlacette,Nha,Gha,Vha,VhaIFN,VcHa,VpHa,Gain,AcctV,AcctG,Taux,TauxPU,TauxV,AccD
      )
    # arbres <- subset(arbres, select=c("NumForet","NumPlac","NumArbre","Cycle","Strate","Coupe",
    #                                   "IdArbre","Azimut","Dist",
    #                                   "Essence","Qual","Type","Reg1", "Reg2","Haut","Stade","Limite",
    #                                   "Diam1","Diam2","Diam","Classe","Cat","NoteEcolo","Vitalité","PU",
    #                                   "PoidsPlacette","Nha","Gha","Vha","VhaIFN","VcHa","VpHa","Gain","AcctV",
    #                                   "AcctG","Taux","TauxPU","TauxV","AccD"))
    save(TauxR,arbres,Reges,Taillis,EssReg,Echantillonnages,
         Coords,Placettes,Forets,
         file="Tables/gfTablesBrutes.RData")

    # save(TauxR,arbres,Reges,Taillis,Reperes,BMSLineaires,BMSsup30,BMP,Codes,EssReg,Echantillonnages,
    #      Coords,IdPlacettes,Regroup,Placettes,Forets,
    #      file="Tables/gfTablesBrutes.RData")
    # arbres <- subset(arbres, select=c("NumForet","NumPlac","NumArbre","Cycle","Strate","Coupe",
    #                                   "IdArbre","Azimut","Dist",
    #                                   "Essence","Qual","Type","Reg1", "Reg2","Haut","Stade","Limite",
    #                                   "Diam1","Diam2","Diam","Classe","Cat","PU",
    #                                   "PoidsPlacette","Nha","Gha","Vha","VhaIFN"))
    # save(arbres, file="Tables/gfTablesBrutes.RData")
    }


  if (max(arbres$Cycle) > 1){
    arbres <- subset(arbres, select=c("NumForet","NumPlac","NumArbre","Cycle","Strate","Coupe",
                                      "IdArbre","Azimut","Dist","Observation",
                                      "Essence","Qual","Type","Reg1", "Reg2","Haut","Stade","Limite",
                                      "Diam1","Diam2","Diam","Classe","Cat","NoteEcolo","Vitalité","PU",
                                      "PoidsPlacette","Gha","Vha","VhaIFN","VcHa","VpHa","Gain","AcctV",
                                      "AcctVper", "AcctG","Taux","TauxPU","TauxV","AccD"))
    save(TauxR,arbres,Reges,Taillis,Reperes,BMSLineaires,BMSsup30,BMP,Codes,EssReg,Echantillonnages,
         Coords,IdPlacettes,Regroup,Placettes,Forets,
         AcctDBrut, file="Tables/gfTablesBrutes.RData")
    }
}
