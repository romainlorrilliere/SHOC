


prepa_data <- function() {

                                        # Data directory  #
    input = "data/" #
    output = "output/"
################# Tables  ################

    obs=read.table(paste(input,"data_observation_sept_2019.txt",sep=""), sep = "\t", h=T, dec = ",", fileEncoding = "UTF-8")
    carre_coord=read.table(paste(input,"data_carre_coord_sept_2019.txt",sep=""), sep = "\t", h=T, dec = ",")
    transect_coord=read.table(paste(input,"data_carre_transect_coord_sept_2019.txt",sep=""), sep = "\t", h=T, dec = ",")

    carre_coord$Longitude <- as.numeric(as.character(carre_coord$Longitude))
    carre_coord$Latitude <- as.numeric(as.character(carre_coord$Latitude))
    transect_coord$Longitude <- as.numeric(as.character(transect_coord$Longitude))
    transect_coord$Latitude <- as.numeric(as.character(transect_coord$Latitude))

    data_espece=read.csv(paste(input,"teespeces.csv",sep=""), sep = ",", h=T, fileEncoding = "UTF-8")
    colnames(data_espece)[1]<-"espece"

####################################################################################
####----------------------- Preparation   ----------

                                        # on suprime les especes ou nombre Null ou ""

    obs<-subset(obs, !is.na(sum_nombre) )
    obs<-subset(obs, espece != "")

    library(data.table)
    setdiff(unique(obs$espece), unique(data_espece$espece)) #  The elements of setdiff(x,y) are those elements in x but not in y.

    liste_espece_shoc<-unique(obs$espece)
    liste_espece_shoc<-subset(data_espece, espece %in% liste_espece_shoc)
    liste_SPE<-unique(liste_espece_shoc[liste_espece_shoc$espece %like% "SPE", ]$espece)
    liste_espece_shoc<-subset(liste_espece_shoc, !(espece %in% liste_SPE))

    data_shoc_transect<-merge(obs,liste_espece_shoc[,c("espece", "NomF")], by=c("espece"), all.y = TRUE )
    data_shoc_transect$espece<-as.factor(substr(data_shoc_transect$espece,1,6))
    data_shoc_transect$id_obs<-paste(data_shoc_transect$carre,data_shoc_transect$hiver,data_shoc_transect$passage, sep="_")
    data_shoc_transect$annee<-substr(data_shoc_transect$hiver,1,4)
    data_shoc_transect$annee<-as.numeric(as.character(data_shoc_transect$annee))

    head(data_shoc_transect)





########################################################################
########    Tendances temporelles des abondances par espece   ########
########################################################################

###############################
                                        # Données SHOC :
                                        # on a enlevé les espèces indeterminee ou au genre ESP
                                        # Données au carré, Nom francais, sans les ESPESP, seuil de 50 par point
                                        # cf script 1

    str(data_shoc_transect)

                                        # Preparation :

    data_tempo<-data_shoc_transect

                                        # selection des espece frequente
    ## espece100<-subset(oisx_freq, frequence > 5)
    ## espece100<-unique(espece100$espece)
    ## data_tempo<-subset(data_tempo, espece %in% espece100)

                                        # on exclu les carre suivis qu'une annee  ???
                                        #carre_plus_annee<-aggregate(annee~id_carre, data = data_stoc_Carre, function(x) length(unique(x) ))
                                        #carre_plus_annee<-subset(carre_plus_annee, annee > 1)
                                        #liste_carre_plus_annee<-unique(carre_plus_annee$id_carre)
                                        #data_tempo<-subset(data_tempo, carre %in% liste_carre_plus_annee)
                                        # Seuil d'abondance a été plafonné au transect (pas au carré) cf script 1

    str(data_tempo)
    head(data_tempo)

### Formatage du tableau pour avoir les 0 et les especes en lignes :

    toto <- reshape(data_tempo[c("hiver","carre","point","date","heure","espece","sum_nombre")]
                   ,v.names="sum_nombre"
                   ,idvar=c("hiver","carre","point","date","heure")
                   ,timevar="espece"
                   ,direction="wide"
                   ,new.row.names = NULL)
    str(toto)
    colnames(toto)[6:length(toto)] <- substr(colnames(toto)[6:length(toto)],12,nchar(colnames(toto)[6:length(toto)]))
    for(j in 6:length(toto)) toto[,j] <- ifelse(is.na(toto[,j]),0,toto[,j])

                                        #	for(j in 6:length(toto)) toto[,j] <- ifelse(toto[,j]>0,1,toto[,j])

    data_tempo <- reshape(toto
                         ,v.names="sum_nombre"
                         ,varying = colnames(toto)[6:length(toto)]
                         ,times = colnames(toto)[6:length(toto)]
                         ,idvar=c("hiver","carre","point","date","heure")
                         ,timevar="espece"
                         ,direction="long"
                         ,new.row.names = NULL)
    row.names(data_tempo) <- NULL
                                        #

    ## Recuperation du jour julien de l'hiver
    data_tempo$date<- as.Date(data_tempo$date, format="%d.%m.%Y")
    library(lubridate)
    data_tempo$julian_date <- yday(data_tempo$date)
    data_tempo$month <- month(data_tempo$date)
    data_tempo$julian_winter <- ifelse(data_tempo$month>6,data_tempo$julian_date - 305,data_tempo$julian_date + 60)
    data_tempo<-subset(data_tempo,julian_winter > 0)
    data_tempo<-merge(data_tempo, transect_coord, by = c("carre","point"), all.x = T)
    data_tempo$annee<-as.numeric(substr(data_tempo$hiver,1,4))
    data_tempo<-subset(data_tempo, annee > 2014)

    data_tempo$pa<-ifelse(data_tempo$sum_nombre>0,1,0)

    colnames(data_tempo)<-c("carre","transect","hiver","date","heure","espece","abond","julian_date","month","julian_winter","Longitude","Latitude","annee","pa")

    write.csv(data_tempo,"output/data_tempo.csv",row.names=FALSE)


    ## Recupe donnees meteo
    ##donnees<-data_tempo[c("hiver","carre","point","date","heure","espece","sum_nombre","julian_winter","Longitude","Latitude")]
    ##colnames(donnees)<-c("hiver","carre","transect","date","heure","espece","pa","julian_winter","Longitude","Latitude")
    ##donnees_site<-unique(donnees[c("carre","transect","date","Longitude","Latitude")])  # long, mais l'aggregate est tout aussi long
    ##donnees_site$site <- paste(donnees_site$carre, donnees_site$transect, sep="_")
    ##donnees_site$carre<-NULL
    ##donnees_site$transect<-NULL
    ##source("C:/CAMILA/SHOC/Script_DATA/get_sample_weather.r")
    ##data_meteo<-get_sample_weather(dsample=donnees_site)
    ##
    ##
    ##
    ##
    ##
    ##
    ##
    ##

}


main.glm <- function(id = "glmmTMB_shoc_2019-11-27", seuilSignif  = 0.05, filtre_transect = NULL, input = "data/" ,output = "output/") {

                                        # Data directory  #

    source("f_Sp_GLM_short_RomaiLo.r")
    library(glmmTMB)
    library(ggplot2)

                                        # Faut les packages glmmTMB
    cat("Import: output/data_tempo.csv ...")
    data_tempo <- read.csv("output/data_tempo.csv")
    cat("    DONE !\n")

    if(!is.null(filtre_transect)) if(filtre_transect == "pair") data_tempo <- subset(data_tempo,transect %% 2 == 0) else  data_tempo <- subset(data_tempo,transect %% 2 == 1)
    tabSp <- read.csv("data/espece.csv",encoding="UTF-8",stringsAsFactors=FALSE)



    repout <- paste0(output,id)


    data_tempo <-subset(data_tempo, !is.na(Longitude) )


    dir.create(repout,showWarnings=FALSE)
    repoutResult <- paste0(repout,"/result")
    dir.create(repoutResult,showWarnings=FALSE)
    dir.create(paste0(repoutResult,"/Incertain"),showWarnings=FALSE)


    file_trend <- paste0(repoutResult,"/",id,"_tendances.csv")
    file_An <- paste0(repoutResult,"/",id,"_variations.csv")
    file_gg <- paste0(repoutResult,"/",id,"_ggTable.csv")



    dAn <- NULL
    dgg <- NULL
    dTrend <- NULL

    dagg <- aggregate(pa~espece,data_tempo,sum)
    dagg <- dagg[order(dagg$pa,decreasing=TRUE),]
    listSp <- dagg$espece

    for(sp in listSp) {

	nomSp <- tabSp$french_name[tabSp$pk_species == sp]
	cat("\n\n==========================\n",sp,nomSp,"\n===============================\n\n")
	flush.console()

	data_sp <- subset(data_tempo,espece == sp)

	theYears <- sort(unique(data_sp$annee))

	myListEffect <- c("annee","julian_winter","Longitude","Latitude")

	cat("\nModele variations temporelles\n=======================\n")
	md_f <- Sp_GLM_short(dataFile=id,varInterest="pa",listEffects=myListEffect,	asfactor="annee",interactions=NA,formulaRandom="+(1|carre/transect)",selSample=1e10,tagModel=paste0("GLMalphatest_VarAnFY",id,"_",sp),family="binomial",data=data_sp,repout=repout,checkRepout=TRUE,saveFig=TRUE,output=TRUE,doBeep=FALSE,printFormula=TRUE)
	smd_f <- md_f[[2]]
	print(smd_f)

	theta_f <- sigma(md_f[[1]])
	coefan <- c(1,smd_f$coef[2:length(theYears)])
	erreuran <- smd_f[2:length(theYears),3]

	erreurannee1 <- c(0,erreuran *smd_f$coef[2:length(theYears)])
	pval <- c(1,smd_f[2:length(theYears),5])
	ic_inf_sim <-  c(1,smd_f$ICinf[2:length(theYears)])
	ic_sup_sim <-  c(1,smd_f$ICsup[2:length(theYears)])


	tab1 <- data.table(id = id,espece = sp, nom_espece = nomSp,
                           year = theYears,
                           val = coefan,
                           LL = ic_inf_sim, UL = ic_sup_sim,
                           catPoint = ifelse(pval < seuilSignif,"significatif",NA),pval)


	tabAn1 <- data.table(espece = sp, nom_espece = nomSp ,
                             year=theYears,
                             abondance_relative = round(coefan,3),
                             IC_inferieur = round(ic_inf_sim,3), IC_superieur = round(ic_sup_sim,3),
                             erreur_standard = round(erreurannee1,4),
                             p_value = round(pval,3),significatif = ifelse(pval < seuilSignif,"significatif",NA),theta=theta_f)


        cat("\nModele tendance\n=======================\n")
	md_c <- Sp_GLM_short(dataFile=id,varInterest="pa",listEffects=myListEffect,asfactor=NA,interactions=NA,formulaRandom="+(1|carre/transect)",selSample=1e10,tagModel=paste0("GLMalphatest_VarAnFY",id,"_",sp),family="binomial",data=data_sp,repout=repout,checkRepout=TRUE,saveFig=TRUE,output=TRUE,doBeep=FALSE,printFormula=TRUE)

	smd_c <- md_c[[2]]
	print(smd_c)

	vif_c_mean <- mean(smd_c$VIF)
	vif_c_max <- max(smd_c$VIF)
	theta_c <- sigma(md_c[[1]])
	smd_c <- smd_c[smd_c$term=="annee",]
	coefan <- smd_c$coef
	trend <- round(coefan,3)
	## pourcentage de variation sur la periode
	estimate <- smd_c$Estimate

	pasdetemps <- length(unique(data_sp$annee))-1
	pourcentage <- round((exp(estimate*pasdetemps)-1)*100,3)
	pval <- smd_c[,5]
	erreuran <- smd_c[,3]
	## erreur standard
	erreurannee_c <- erreuran*coefan
	vif_c <- smd_c$VIF
	ic_inf_sim <-  round(smd_c$ICinf,3)
	ic_sup_sim <-  round(smd_c$ICsup,3)
	## tab1t table utile pour la realisation des figures
	tab1t <- data.frame(Est=trend,
                            LL=ic_inf_sim, UL=ic_sup_sim,
                            pourcent=pourcentage,signif=pval<seuilSignif,pval,
                            vif=vif_c,vif_mean=vif_c_mean,vif_max=vif_c_max)
	trendsignif <- tab1t$signif

	## surdispersion
	## affectation des tendence EBCC
	catEBCC <- NA
	catEBCC <- affectCatEBCC(trend = tab1t$Est,pVal = tab1t$pval,ICinf=as.vector(tab1t$LL),ICsup=as.vector(tab1t$UL))
	## table complete de resultats
	vecLib <-  NULL
	if(is.na(vif_c_mean)) {
            catIncert <- "Incertain"
            if(is.na(vif_c_mean)) vecLib <- paste(vecLib,"VIF tendance non calculable")
	} else { # ELSE  if(is.na(vif_c_mean))
            if( vif_c_mean > 2 | vif_c_max > 5 | theta_c
               < .1 | theta_f > 10 | theta_c < .1 | theta_c > 10) {
                catIncert <- "Incertain"
                if(vif_c_mean > 2) vecLib <- c(vecLib,"moyenne vif tendance sup à 2")
                if(vif_c_max > 5) vecLib <- c(vecLib,"max vif tendance sup à 5")
                if(theta_f < 0.1) vecLib <- c(vecLib," theta variation inf à 0.1")
                if(theta_c < 0.1) vecLib <- c(vecLib," theta tendance inf à 0.1")
                if(theta_f > 10) vecLib <- c(vecLib," theta variation sup à 10")
                if(theta_c > 10) vecLib <- c(vecLib," theta tendance sup à 10")
            } else {
                catIncert <-"bon"
            }
	} # END ELSE  if(is.na(vif_c_mean))

	raisonIncert <-  paste(vecLib,collapse=" et ")

	firstY <- min(theYears)
	lastY <- max(theYears)


	tabTrend1 <- data.frame(
            id,espece=sp,nom_espece = nomSp ,
            nombre_annees = pasdetemps+1,premiere_annee = firstY,derniere_annee = lastY,
            tendance = trend ,  IC_inferieur=ic_inf_sim , IC_superieur = ic_sup_sim ,
            pourcentage_variation= pourcentage,
            erreur_standard = erreurannee_c, p_value = pval,
            vif = vif_c,vif_mean=vif_c_mean,vif_max=vif_c_max,
            significatif = trendsignif,categorie_tendance_EBCC=catEBCC,
            theta_variation = theta_f,theta_tendance = theta_c,
            valide = catIncert,raison_incertitude = raisonIncert)


	dTrend <- rbind(dTrend,tabTrend1)
	dAn <- rbind(dAn,tabAn1)
	dgg <- rbind(dgg,tab1)#,tab2)

	cat("\nFigure\n=======================\n")
        titre <- paste(tab1$nom_espece[1]," (",tab1$espece[1],")\n",min(tab1$year) ," - ",max(tab1$year),sep="")

        txtPente <- paste(tabTrend1$tendance,
                          ifelse(tabTrend1$significatif," *",""),"  [",tabTrend1$IC_inf," , ",tabTrend1$IC_sup,"]",
                          ifelse(tabTrend1$significatif,paste("\n",ifelse(tabTrend1$pourcentage_variation>0,"+ ","- "),
                                                              abs(tabTrend1$pourcentage_variation)," % en ",pasdetemps," ans",sep=""),""),sep="")



        ## table du texte de la tendence

	tabTextPent <- data.table(x=ifelse(tabTrend1$pourcentage_variation>0,-Inf,Inf),
                                  text_hjust= ifelse(tabTrend1$pourcentage_variation>0,-0.1,1.1),
                                  txt=txtPente)


	figname<- paste0(repoutResult,"/",ifelse(tabTrend1$valide=="Incertain","Incertain/",""),
                         sp,"_",id,".png")


	gg <- ggplot(data=tab1,mapping=aes(x=year,y=val))
	gg <- gg + labs(y="",x="Année",title=titre)
	gg <- gg +  theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())
	gg <- gg + geom_hline(yintercept=1,colour="white",size=2)
	gg <- gg + geom_ribbon(aes(ymin=LL,ymax=UL),colour=NA,alpha=.2,fill="#3c47e0")
	gg <- gg + geom_pointrange(aes(y=val,ymin=LL,ymax=UL),alpha=.5,colour="#3c47e0")
	gg <- gg + geom_line(size = 1.2,alpha=.8,colour="#3c47e0")
	gg <- gg + geom_point(size = 2,colour="#3c47e0")
	gg <- gg + geom_text(data=tabTextPent, mapping=aes(x=x,y=Inf,label=txt,hjust=text_hjust),vjust=1.1,colour="black",parse=FALSE,fontface=2, size=3)

	cat("\n  [PNG]",figname,"\n")
	flush.console()
	ggsave(figname,gg,width=6,height=4)
	cat("\n  [CSV]",file_trend,"\n")
	flush.console()
	write.csv(dTrend,file_trend,row.names=FALSE)
	cat("\n  [CSV]",file_An,"\n")
	flush.console()
	write.csv(dAn,file_An,row.names=FALSE)
	cat("\n  [CSV]",file_gg,"\n")
	flush.console()
	write.csv(dgg,file_gg,row.names=FALSE)

    }

    cat("\n  [CSV]",file_trend,"\n")
    flush.console()
    write.csv(dTrend,file_trend,row.names=FALSE)
    cat("\n  [CSV]",file_An,"\n")
    flush.console()
    write.csv(dAn,file_An,row.names=FALSE)
    cat("\n  [CSV]",file_gg,"\n")
    flush.console()
    write.csv(dgg,file_gg,row.names=FALSE)

}

bin <- function() {

    if(class(md1)[1] != "try-error") {
        ## test the robustness of the fit
        smd1 <- md1[[2]]
        smd1 <- smd1[grep("year",row.names(smd1)),]
        sd_good <- all(smd1[,3]<3)
    } else { cat("ERROR !!!\n")}



###################################################################################################
###################################################################################################
################################  A FAIRE    Boucle pour variation d'abondance par année       ################################



    donnees<-toto
    names=colnames(donnees)
    result=list()
    pasdetemps=max(as.numeric(donnees$annee)-min(as.numeric(donnees$annee)))
    nbans=pasdetemps + 1

    setwd("C:/CAMILA/SHOC/Script_DATA/output_R/tendance_tempo/")

    for (i in 8:length(donnees)) {

     ##   pa ~ annee + hiver_julien + hiver_julien^2 + heure + (1|point<carre), family = binomial
                                        # pa ~ annee + hiver_julien + hiver_julien^2 + heure + varMeteo + (1|point<carre), family = binomial


        coefdata=coefficients(glm)
        coefdata=matrix(coefdata)
        coefdata=tail(coefdata,pasdetemps)
        coefanneebis=exp(coefdata)
        coefannee=rbind(1,coefanneebis)
        erreurmatrix=matrix(summary(glm)$coefficients[,2])
        erreur=tail(erreurmatrix,pasdetemps)
        erreur=as.data.frame(erreur)
        erreur=erreur*coefanneebis
        erreurcoef=rbind(0,erreur)
        figure <- paste(names[i], ".png", sep = "") #
        png(figure)
        plot(coefannee,pch=16,col="red",ylim=c(min(coefannee-(erreurcoef)),max(coefannee+(erreurcoef))),xaxt="n",ylab="",xlab="",main=names[i])
        axis(1,1:nbans,labels = c(min(as.numeric(donnees$annee)):max(as.numeric(donnees$annee))))
        lines(coefannee,col="red")
        lines(coefannee-(erreurcoef),col="blue",lty="dashed")
        lines(coefannee+(erreurcoef),col="blue",lty="dashed")
        legend("topright",legend=c("indice annuel de variation d'effectifs","Indice +/- erreur"),col=c("red","blue"),lty=c(1,2),bty="o",pt.cex=1,cex=0.8,text.col="black",horiz=F,inset=c(0.03,0.03))
        abline(h=1,col="grey")
        dev.off() #
        resume=cbind(coefannee,erreurcoef)
        colnames(resume)<-c("estimate","erreur standard")
        resume
        row.names(resume)<-paste(row.names(resume),names[i],sep="")
        result[[i]]=resume
    }

    lala=do.call(rbind,result)
    write.table(lala,"C:/CAMILA/SHOC/Script_DATA/output_R/tendance_tempo/", row.names=T)
### Valeurs par annee pour tracer les graph




###################################################################################################
###################################################################################################
################################  GRAPHIQUE    ################################

                                        # dans main.glm


    ggplot.espece(dgg,tab1t,id,serie=NULL,sp,valide=catIncert,nomSp,description,tendanceSurFigure,seuilOccu=14,vpan = vpan,assessIC=assessIC)



############################################################################################################ fonction graphique appelée par main.glm / function called by main.glm for graphical output
    ggplot.espece <- function(dgg,tab1t,id,serie=NULL,sp,valide,nomSp=NULL,description=TRUE,
                              tendanceSurFigure=TRUE,seuilOccu=14, vpan,assessIC=TRUE) {

                                        #  serie=NULL;nomSp=NULL;description=TRUE;valide=catIncert
                                        #  tendanceSurFigure=TRUE;seuilOccu=14
        require(ggplot2)

        figname<- paste("Output/",id,"/",ifelse(valide=="Incertain","Incertain/",""),
                        sp,"_",id,serie, ".png",
                        sep = "")
        ## coordonnee des ligne horizontal de seuil pour les abondances et les occurences
        hline.data1 <- data.frame(z = c(1), panel = c(vpan[1]),couleur = "variation abondance",type="variation abondance")
        hline.data2 <- data.frame(z = c(0,seuilOccu), panel = c(vpan[2],vpan[2]),couleur = "seuil",type="seuil")
        hline.data3 <- data.frame(z = 0, panel = vpan[3] ,couleur = "seuil",type="seuil")
        hline.data <- rbind(hline.data1,hline.data2,hline.data3)
        titre <- paste(nomSp)#,"\n",min(annee)," - ",max(annee),sep="")

        ## texte de la tendance / text for the population evolution trend
        tab1 <- subset(dgg,panel =="Variation abondance")
        pasdetemps <- max(dgg$annee) - min(dgg$annee) + 1
        if(assessIC){
            txtPente1 <- paste(tab1t$Est,
                               ifelse(tab1t$signif," *",""),"  [",tab1t$LL," , ",tab1t$UL,"]",
                               ifelse(tab1t$signif,paste("\n",ifelse(tab1t$pourcent>0,"+ ","- "),
                                                         abs(tab1t$pourcent)," % en ",pasdetemps," ans",sep=""),""),sep="")
        }else{
            txtPente1 <- ifelse(tab1t$signif,paste("\n",ifelse(tab1t$pourcent>0,"+ ","- "),
                                                   abs(tab1t$pourcent)," % en ",pasdetemps," ans",sep=""),"")

        }
        ## table du texte de la tendance / table of the text for the population evolution trend
        tabTextPent <- data.frame(y=c(max(c(tab1$val,tab1$UL),na.rm=TRUE)*.9),
                                  x=median(tab1$annee),
                                  txt=ifelse(tendanceSurFigure,c(txtPente1),""),
                                  courbe=c(vpan[1]),panel=c(vpan[1]))
        ## les couleurs / the colors
        vecColPoint <- c("#ffffff","#eeb40f","#ee0f59")
        names(vecColPoint) <- c("significatif","infSeuil","0")
        vecColCourbe <- c("#3c47e0","#5b754d","#55bb1d","#973ce0")
        names(vecColCourbe) <- c(vpan[1],"carre","presence",vpan[3])
        vecColHline <- c("#ffffff","#e76060")
        names(vecColHline) <- c("variation abondance","seuil")

        col <- c(vecColPoint,vecColCourbe,vecColHline)
        names(col) <- c(names(vecColPoint),names(vecColCourbe),names(vecColHline))

        ## si description graphique en 3 panels
        if(description) {
            p <- ggplot(data = dgg, mapping = aes(x = annee, y = val))
            ## Titre, axes ...
            p <- p + facet_grid(panel ~ ., scale = "free") +
                theme(legend.position="none",
                      panel.grid.minor=element_blank(),
                      panel.grid.major.y=element_blank())  +
                ylab("") + xlab("Annee")+ ggtitle(titre) +
                scale_colour_manual(values=col, name = "" ,
                                    breaks = names(col))+
                scale_x_continuous(breaks=min(dgg$annee):max(dgg$annee))
            p <- p + geom_hline(data =hline.data,mapping = aes(yintercept=z, colour = couleur,linetype=type ),
                                alpha=1,size=1.2)
            if(assessIC){ ############# ONLY FOR THE CONFIDENCE INTERVAL
                p <- p + geom_ribbon(mapping=aes(ymin=LL,ymax=UL),fill=col[vpan[1]],alpha=.2)
                p <- p + geom_pointrange(mapping= aes(y=val,ymin=LL,ymax=UL),fill=col[vpan[1]],alpha=.2)
            }
            p <- p + geom_line(mapping=aes(colour=courbe),size = 1.5)
            p <- p + geom_point(mapping=aes(colour=courbe),size = 3)
            p <- p + geom_point(mapping=aes(colour=catPoint,alpha=ifelse(!is.na(catPoint),1,0)),size = 2)
            p <-  p + geom_text(data=tabTextPent, mapping=aes(x,y,label=txt),parse=FALSE,color=col[vpan[1]],fontface=2, size=4)
            ggsave(figname, p,width=16,height=21, units="cm")
            print (figname)  ##### CAN BE REMOVED IF YOU DO NOT WANT THE GRAPH TO BE PLOTTED
        } else {

            p <- ggplot(data = subset(dgg,panel=="Variation abondance"), mapping = aes(x = annee, y = val))
            ## Titre, axes ...
            p <- p + facet_grid(panel ~ ., scale = "free") +
                theme(legend.position="none",
                      panel.grid.minor=element_blank(),
                      panel.grid.major.y=element_blank())  +
                ylab("") + xlab("Annee")+ ggtitle(titre) +
                scale_colour_manual(values=col, name = "" ,
                                    breaks = names(col))+
                scale_x_continuous(breaks=min(dgg$annee):max(dgg$annee))
            p <- p + geom_hline(data =subset(hline.data,panel=="Variation abondance"),mapping = aes(yintercept=z, colour = couleur,linetype=type ),
                                alpha=1,size=1.2)

            if(assessIC){ ############# ONLY FOR THE CONFIDENCE INTERVAL
                p <- p + geom_ribbon(mapping=aes(ymin=LL,ymax=UL),fill=col[vpan[1]],alpha=.2)
                p <- p + geom_pointrange(mapping= aes(y=val,ymin=LL,ymax=UL),fill=col[vpan[1]],alpha=.2)
            }
            p <- p + geom_line(mapping=aes(colour=courbe),size = 1.5)
            p <- p + geom_point(mapping=aes(colour=courbe),size = 3)
            p <- p + geom_point(mapping=aes(colour=catPoint,alpha=ifelse(!is.na(catPoint),1,0)),size = 2)
            p <-  p + geom_text(data=tabTextPent, mapping=aes(x,y,label=txt),parse=FALSE,color=col[vpan[1]],fontface=2, size=4)
            ggsave(figname, p,width=15,height=9,units="cm")
            print (figname) ##### CAN BE REMOVED IF YOU DO NOT WANT THE GRAPH TO BE PLOTTED
        }
    }
############################################################################################################ fin fonction graphique / end of function for graphical output









}
