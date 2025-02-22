#######
## Analysis SHOC data
######

vecPackage=c("ade4","akima","data.table","date","devtools","dismo","doBy","dplyr","factoextra","gam","ggmap","ggplot2","gstat","lme4","lubridate","mapdata","mapproj","maps","maptools","MASS","mgcv","mgcViz","MuMIn","nlme","partykit","party","plyr","rattle","reshape","reshape2","rgdal","RMySQL","RODBC","rpart","rpart.plot","RPostgreSQL","siland","snow","sp","StreamMetabolism","stringr","survMisc","vegan")
ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)

    require(reshape2)
    require(ggplot2)
    require(data.table)





habitat_prevalence_winter <- function(dataBrut=FALSE,fileAnalysis="data/winter_analysisHabiat.csv") {
    require(reshape2)
    require(ggplot2)
    require(data.table)

    dataBrut=FALSE ;    fileAnalysis="data/winter_analysisHabiat.csv"
    if(dataBrut) {
        d <- read.delim("data/export_shoc_04072018_155413.txt",stringsAsFactors=FALSE,header=TRUE,encoding="UTF-8")
        colnames(d) <- c("Code.inventaire","Etude","Site","Pays","Departement","INSEE","Commune","N..Carre.EPS","Date","Heure","Heure.fin","N..Passage","Observateur","Email","EXPORT_STOC_TEXT_EPS_POINT","Altitude","Classe","Espece","Nombre","Distance.de.contact","Longitude","Latitude","Type.de.coordonn�es","Type.de.coordonn�es.lambert","EPS.Nuage","EPS.Pluie","EPS.Vent","EPS.Visibilite","EPS.Neige","EPS.Transport","EPS.P.Milieu","EPS.P.Type","EPS.P.Cat1","EPS.P.Cat2","EPS.P.Sous.Cat1","EPS.P.Sous.Cat2","EPS.S.Milieu","EPS.S.Type","EPS.S.Cat1","EPS.S.Cat2","EPS.S.Sous.Cat1","EPS.S.Sous.Cat2")
        dd <- subset(d,d$Distance.de.contact %in% c("LESS25","LESS100","LESS200","U")& N..Passage >0 & substr(Date,4,5)=="12",select=c("Code.inventaire","N..Carre.EPS","Date","N..Passage","EXPORT_STOC_TEXT_EPS_POINT","Espece","Nombre","EPS.P.Milieu","EPS.P.Type"))

        dd$Espece[dd$Espece==""] <- "OTHERS"
        dd$Espece <- substr(dd$Espece,1,6)

        ddinv <- aggregate(Nombre~Code.inventaire+N..Carre.EPS+Date+N..Passage+EXPORT_STOC_TEXT_EPS_POINT+Espece+EPS.P.Milieu,,dd,sum)

        ddinv$an <- as.numeric(substr(ddinv$Date,7,10))


        ddan <- aggregate(Nombre~N..Carre.EPS+EXPORT_STOC_TEXT_EPS_POINT+Espece+an,ddinv,max)

        ddan_w <- dcast(ddan,N..Carre.EPS+EXPORT_STOC_TEXT_EPS_POINT+an ~Espece,value.var="Nombre")
        ddan_w[is.na(ddan_w)] <- 0

        ddan2 <- melt(ddan_w,id.vars=c("N..Carre.EPS","EXPORT_STOC_TEXT_EPS_POINT","an"))
        colnames(ddan2) <- c("carre","point","an","sp","ab")

        ddcarre <- aggregate(Nombre~N..Carre.EPS+an+Espece,ddinv,max)
        colnames(ddcarre)<- c("carre","an","sp","ab_carre")

        ddpoint <- merge(ddan2,ddcarre,by=c("carre","an","sp"))
        ddpoint$carrepoint <- paste(ddpoint$carre,ddpoint$point)

        ddhab <- unique(dd[,c("N..Carre.EPS","EXPORT_STOC_TEXT_EPS_POINT","EPS.P.Milieu")])
        colnames(ddhab) <- c("carre","point","hab")
        ddhab$carrepoint <- paste(ddhab$carre,ddhab$point)

        habdupli <- which(duplicated(ddhab$carrepoint))

        ddhab <- ddhab[!(1:nrow(ddhab)%in%habdupli),]

        ddhab <- ddhab[,c("carrepoint","hab")]

        ddpoint <- merge(ddpoint,ddhab,by="carrepoint")

        write.csv(ddpoint,fileAnalysis,row.names=FALSE)
    } else {

        ddpoint <- read.csv(fileAnalysis)
    }

    thab <- read.csv("data/habitat.csv")

    dodo <-  unique(ddpoint[,c("carrepoint","hab")])
    dodo <- merge(dodo,thab,by="hab")

    gghab <- ggplot(dodo,aes(x=factor(1),fill=habitat_name))+ geom_bar(width = 1)+
  coord_polar("y")

    ggsave("output/habitatSHOC.png",gghab)


    ## Prevalence indicator
    ##    Andrade, C., Chiron, F., Julliard, R., 2012. Improving the selection of focal species exposed to pesticides to support ecological risk assessments. Ecotoxicology 21, 2430�2440. https://doi.org/10.1007/s10646-012-0982-4


    ddpoint$pr <- as.numeric(ddpoint$ab>0)

    sp_pr <- aggregate(pr~sp,ddpoint,sum)
    sp_pr <- subset(sp_pr, pr>100)

    ddpoint <- subset(ddpoint,sp %in% sp_pr$sp)

    ddhab <- as.data.frame(table(ddpoint$hab))
    colnames(ddhab) <- c("hab","hab_nb")

    ddsp <- aggregate(ab~sp,ddpoint,sum)
    colnames(ddsp) <- c("sp","ab_tot")

    ddsp_hab <- aggregate(cbind(ddpoint$ab,ddpoint$pr)~hab+sp,ddpoint,sum)
    colnames(ddsp_hab) <- c("hab","sp","ab","pr")

    ddsp_hab <- merge(ddsp_hab,ddhab,by="hab")
    ddsp_hab <- merge(ddsp_hab,ddsp,by="sp")
    ddsp_hab <- merge(ddsp_hab,thab,by="hab")

    ddsp_hab$prevalence <- (ddsp_hab$ab/ddsp_hab$ab_tot)*(ddsp_hab$pr/ddsp_hab$hab_nb)


    dprevalence <- dcast(sp ~habitat_name,data=ddsp_hab,value.var="prevalence")
    colnames(dprevalence)[c(4,7,8)] <- c("woodland","urban","water")

    tsp <- read.csv2("data/espece_list_indicateur.csv",encoding="UTF-8")
    tsp <- tsp[,c("pk_species","french_name","english_name","birdlab","birdlab_shoc_prevalence_farmland","birdlab_shoc_prevalence_woodland","birdlab_shoc_prevalence_urban")]

    dprevalence <- merge(dprevalence,tsp,by.x="sp",by.y="pk_species")

    ## require(ggtern)
    ## gg <- ggtern(subset(dprevalence,birdlab),aes(farmland,woodland,urban,label=sp,color=birdlab_shoc_prevalence_farmland))+geom_text(size=2)
    ## gg

### resampling 300 by main habitat
    dodo$grHab <- ifelse(dodo$habitat_name %in% c("farmland","forest","urban area"),as.character(dodo$habitat_name),"other")

        dddprev <- NULL

    for(i in 1:400){

        dodo_<- dodo[sample(1:nrow(dodo)),]
        dodo.dt <- data.table(dodo_)
        dodo.dt <- dodo.dt[,order_gr := 1:.N, by=grHab]
        dodo2 <- as.data.frame(dodo.dt)

        dodo2 <- subset(dodo2,order_gr <= 300)
        ## gghab <- ggplot(dodo2,aes(x=factor(1),fill=habitat_name))+ geom_bar(width = 1)+
        ## coord_polar("y")
        ##  ggsave("output/habitatSHOC_resampled.png",gghab)


        ddpoint2 <- subset(ddpoint,carrepoint %in% dodo2$carrepoint)

        ddsp <- aggregate(ab~sp,ddpoint2,sum)
        colnames(ddsp) <- c("sp","ab_tot")

        ddsp_hab <- aggregate(ab ~ hab+sp,ddpoint2,sum)
        colnames(ddsp_hab) <- c("hab","sp","ab")

        ddsp_hab <- merge(ddsp_hab,ddsp,by="sp")

        ddsp_hab$prevalence <- (ddsp_hab$ab/ddsp_hab$ab_tot)
        ddsp_hab <- merge(ddsp_hab,thab,by="hab")

        dprevalence <- dcast(sp ~habitat_name,data=ddsp_hab,value.var="prevalence")
        colnames(dprevalence)[c(4,7,8)] <- c("woodland","urban","water")
        dprevalence[is.na(dprevalence)] <- 0

        dprevalence <- subset(dprevalence,(dprevalence$farmland + dprevalence$woodland + dprevalence$urban)>0)
        dprevalence$sum3hab <- (dprevalence$farmland + dprevalence$woodland + dprevalence$urban)
     #   dprevalence <- subset(dprevalence,sum3hab>.75)
        dprev <- dprevalence[,c("sp","farmland","woodland","urban")]

        ddprev <- melt(id.vars="sp",data=dprev)
        colnames(ddprev)[2:3] <- c("habitat_name","preval")
        ddprev$preval <- ddprev$preval - 0.3
        ddprev$idset <- i

        dddprev <- rbind(dddprev,ddprev)
    }



    write.csv(dddprev,"output/winter_habitat_prev_bootstrap.csv",row.names=FALSE)


    ddSpe.nb <- aggregate(preval~sp+habitat_name,dddprev,length)

    ddSpe.agg <- aggregate(preval~sp+habitat_name,dddprev,quantile,c(0.025,0.5,0.975))

    ddSpe.agg <- data.frame(ddSpe.agg[,1:2],ddSpe.agg[,3])
    colnames(ddSpe.agg)[3:5] <- c("ICinf","med","ICsup")

    ddSpe.agg.mean <- aggregate(preval~sp+habitat_name,dddprev,mean)
    colnames(ddSpe.agg.mean)[3] <- c("mean")

    ddSpe.agg <- merge(ddSpe.agg,ddSpe.agg.mean,by = c("sp","habitat_name"))


    ddSpe.agg <- merge(ddSpe.agg,tsp,by.x="sp",by.y="pk_species")


    ddSpe.agg <- subset(ddSpe.agg,habitat_name%in% c("farmland","woodland","urban"))

    write.csv(ddSpe.agg,"output/winter_prevalence_CI.csv",row.names=FALSE)



    dres.bl <- subset(ddSpe.agg,birdlab)



    vecCol <- c("farmland"="#ffb319","woodland"="#1a9129","urban"="#ff0404")


    ttsp <- read.csv("data/espece.csv",encoding="UTF-8")
    ttsp <- subset(ttsp,select=c("pk_species","euring","french_name","english_name"))
    colnames(ttsp)[1] <- "sp"


    orderSp <- subset(ttsp,sp %in% dres.bl$sp)
    dres.bl$english_name <- factor(dres.bl$english_name, levels = orderSp$english_name)



    gg <- ggplot(dres.bl,aes(x=english_name,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + geom_errorbar(aes(ymin=med,ymax=med),position = position_dodge(),width=.7 )
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))+ labs(x="",y="Habitat prevalence",fill="Habitat",colour="Habitat")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    ## gg
   ggsave("output/winter_birdlab_prevalence_habitat.png",gg,width=8,height=6)



    gg <- ggplot(subset(dres.bl,mean>0),aes(x=english_name,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))+ labs(x="",y="Habitat prevalence",fill="Habitat",colour="Habitat")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    ## gg
    ggsave("output/winter_birdlab_prevalence_habitat_index.png",gg,width=8,height=6)




    ddSpe.agg <- read.csv("output/winter_prevalence_CI.csv")

    dprev_wide <- dcast(ddSpe.agg,sp~habitat_name,value.var="mean")
    dprev_wide <- dprev_wide[,c("sp","farmland","woodland","urban")]
    colnames(dprev_wide) <- c("pk_species","shoc_farmland_prev_index","shoc_woodland_prev_index","shoc_urban_prev_index")

    tindic <- read.csv("../Birdlab/generic_data/espece_list_indicateur.csv",encoding="UTF-8")
    dim(tindic)
    tindic <- merge(tindic,dprev_wide,by="pk_species",all.x=TRUE)
    dim(tindic)
    tindic$shoc_farmland_prev <- ifelse(is.na(tindic$shoc_farmland_prev_index),FALSE,tindic$shoc_farmland_prev_index > 0)
    tindic$shoc_woodland_prev <- ifelse(is.na(tindic$shoc_woodland_prev_index),FALSE,tindic$shoc_woodland_prev_index > 0)
    tindic$shoc_urban_prev <- ifelse(is.na(tindic$shoc_urban_prev_index),FALSE,tindic$shoc_urban_prev_index > 0)

    tindic <- tindic[,sort(setdiff(1:ncol(tindic),grep("index",colnames(tindic))))]

    write.csv(tindic,"../Birdlab/generic_data/espece_list_indicateur.csv",na="",row.names=FALSE,fileEncoding="UTF-8")

    tindex <- read.csv("../Birdlab/generic_data/espece_indicateur_fonctionel.csv",encoding="UTF-8")
    tindex <-  tindex[,sort(setdiff(1:ncol(tindex),intersect(grep("index",colnames(tindex)),grep("shoc",colnames(tindex)))))]

    tindex <- merge(tindex,dprev_wide,by="pk_species",all=TRUE)

    colindex <- c("pk_species","ssi","sti","sti_europe","thermal_niche_mean","thermal_niche_range","thermal_niche_max","thermal_niche_min","stri","exp_stri","trophic_vegetation","trophic_invertebrate","trophic_vertebrate","shoc_farmland_prev_index","shoc_woodland_prev_index","shoc_urban_prev_index")
    colindex <- c(colindex,setdiff(colnames(tindex),colindex))
    tindex <- tindex[,colindex]

    write.csv(tindex,"../Birdlab/generic_data/espece_indicateur_fonctionel.csv",na="",row.names=FALSE,fileEncoding="UTF-8")


}

habitat_prevalence_breeding <- function(dataBrut=FALSE,fileAnalysis="data/breeding_analysisHabiat.csv") {
    require(reshape2)
    require(ggplot2)
    require(data.table)

   # dataBrut=TRUE;    fileAnalysis="data/breeding_analysisHabiat.csv"
    if(dataBrut) {
        d <- read.csv2("data/data_FrenchBBS_point_romain_allSp_2014_2017fr.csv",stringsAsFactors=FALSE)

        dd <- subset(d,select=c("point","carre","annee","id_point_annee","code_sp","abondance","habitat_principal"))

        dd$code_sp[dd$code_sp==""] <- "OTHERS"
        dd$code_sp<- substr(dd$code_sp,1,6)


        ddan_w <- dcast(dd,point + carre + annee + id_point_annee + habitat_principal ~ code_sp,value.var="abondance")

        ddan_w[is.na(ddan_w)] <- 0




        ddan2 <- melt(ddan_w,id.vars=c("point","carre","annee","id_point_annee","habitat_principal"))
        colnames(ddan2)[6:7] <- c("sp","ab")




        ddcarre <- aggregate(ab~carre +annee + sp,ddan2,sum)
        colnames(ddcarre)<- c("carre","annee","sp","ab_carre")
        ddcarre <- subset(ddcarre,ab_carre>0)


        ddpoint <- merge(ddan2,ddcarre,by=c("carre","annee","sp"))
        ddpoint$log_ab <- log(ddpoint$ab+1)
        ddpoint$carrepoint <- paste(ddpoint$carre,ddpoint$point)


        ddhab <- unique(dd[,c("carre","point","habitat_principal")])
        colnames(ddhab) <- c("carre","point","hab")
        ddhab$carrepoint <- paste(ddhab$carre,ddhab$point)

        habdupli <- which(duplicated(ddhab$carrepoint))

        ddhab <- ddhab[!(1:nrow(ddhab)%in%habdupli),]

        ddhab <- ddhab[,c("carrepoint","hab")]

        ddpoint <- merge(ddpoint,ddhab,by="carrepoint")

        write.csv(ddpoint,fileAnalysis,row.names=FALSE)
    } else {

        ddpoint <- read.csv(fileAnalysis)
    }

    thab <- read.csv("data/habitat.csv")

    dodo <-  unique(ddpoint[,c("carrepoint","hab")])
    dodo <- merge(dodo,thab,by="hab")

    gghab <- ggplot(dodo,aes(x=factor(1),fill=habitat_name))+ geom_bar(width = 1)+
  coord_polar("y")

    ggsave("output/habitatSTOC.png",gghab)


    ## Prevalence indicator
    ##    Andrade, C., Chiron, F., Julliard, R., 2012. Improving the selection of focal species exposed to pesticides to support ecological risk assessments. Ecotoxicology 21, 2430�2440. https://doi.org/10.1007/s10646-012-0982-4


    ddpoint$pr <- as.numeric(ddpoint$ab>0)

    sp_pr <- aggregate(pr~sp,ddpoint,sum)
    sp_pr <- subset(sp_pr, pr>100)

    ddpoint <- subset(ddpoint,sp %in% sp_pr$sp)

    ddhab <- as.data.frame(table(ddpoint$hab))
    colnames(ddhab) <- c("hab","hab_nb")

    ddsp <- aggregate(ab~sp,ddpoint,sum)
    colnames(ddsp) <- c("sp","ab_tot")

    ddsp_hab <- aggregate(cbind(ddpoint$ab,ddpoint$pr)~hab+sp,ddpoint,sum)
    colnames(ddsp_hab) <- c("hab","sp","ab","pr")

    ddsp_hab <- merge(ddsp_hab,ddhab,by="hab")
    ddsp_hab <- merge(ddsp_hab,ddsp,by="sp")
    ddsp_hab <- merge(ddsp_hab,thab,by="hab")

    ddsp_hab$prevalence <- (ddsp_hab$ab/ddsp_hab$ab_tot)*(ddsp_hab$pr/ddsp_hab$hab_nb)


    dprevalence <- dcast(sp ~habitat_name,data=ddsp_hab,value.var="prevalence")
    colnames(dprevalence)[c(4,7,8)] <- c("woodland","urban","water")

    tsp <- read.csv2("data/espece_list_indicateur.csv",encoding="UTF-8")
    tsp <- tsp[,c("pk_species","french_name","english_name","birdlab","birdlab_shoc_prevalence_farmland","birdlab_shoc_prevalence_woodland","birdlab_shoc_prevalence_urban")]

    dprevalence <- merge(dprevalence,tsp,by.x="sp",by.y="pk_species")
    ##require(ggtern)

##    gg <- ggtern(subset(dprevalence,birdlab),aes(farmland,woodland,urban,label=sp,color=birdlab_shoc_prevalence_farmland))+geom_text(size=2)
##gg

### resampling 300 by main habitat
    dodo$grHab <- ifelse(dodo$habitat_name %in% c("farmland","forest","urban area"),as.character(dodo$habitat_name),"other")

        dddprev <- NULL


    for(i in 1:400){

        dodo_<- dodo[sample(1:nrow(dodo)),]
        dodo.dt <- data.table(dodo_)
        dodo.dt <- dodo.dt[,order_gr := 1:.N, by=grHab]
        dodo2 <- as.data.frame(dodo.dt)

        dodo2 <- subset(dodo2,order_gr <= 300)
        ## gghab <- ggplot(dodo2,aes(x=factor(1),fill=habitat_name))+ geom_bar(width = 1)+
        ## coord_polar("y")
        ##  ggsave("output/habitatSHOC_resampled.png",gghab)


        ddpoint2 <- subset(ddpoint,carrepoint %in% dodo2$carrepoint)

        ddsp <- aggregate(ab~sp,ddpoint2,sum)
        colnames(ddsp) <- c("sp","ab_tot")

        ddsp_hab <- aggregate(ab ~ hab+sp,ddpoint2,sum)
        colnames(ddsp_hab) <- c("hab","sp","ab")

        ddsp_hab <- merge(ddsp_hab,ddsp,by="sp")

        ddsp_hab$prevalence <- (ddsp_hab$ab/ddsp_hab$ab_tot)
        ddsp_hab <- merge(ddsp_hab,thab,by="hab")

        dprevalence <- dcast(sp ~habitat_name,data=ddsp_hab,value.var="prevalence")
        colnames(dprevalence)[c(4,7,8)] <- c("woodland","urban","water")
        dprevalence[is.na(dprevalence)] <- 0

        dprevalence <- subset(dprevalence,(dprevalence$farmland + dprevalence$woodland + dprevalence$urban)>0)
        dprevalence$sum3hab <- (dprevalence$farmland + dprevalence$woodland + dprevalence$urban)
        dprev <- dprevalence[,c("sp","farmland","woodland","urban")]

        ddprev <- melt(id.vars="sp",data=dprev)
        colnames(ddprev)[2:3] <- c("habitat_name","preval")
        ddprev$preval <- ddprev$preval - 0.3
        ddprev$idset <- i

        dddprev <- rbind(dddprev,ddprev)
    }

        write.csv(dddprev,"output/breeding_habitat_prev_bootstrap.csv",row.names=FALSE)

    ddSpe.nb <- aggregate(preval~sp+habitat_name,dddprev,length)

    ddSpe.agg <- aggregate(preval~sp+habitat_name,dddprev,quantile,c(0.025,0.5,0.975))

    ddSpe.agg <- data.frame(ddSpe.agg[,1:2],ddSpe.agg[,3])
    colnames(ddSpe.agg)[3:5] <- c("ICinf","med","ICsup")

    ddSpe.agg.mean <- aggregate(preval~sp+habitat_name,dddprev,mean)
    colnames(ddSpe.agg.mean)[3] <- c("mean")

    ddSpe.agg <- merge(ddSpe.agg,ddSpe.agg.mean,by = c("sp","habitat_name"))



    ddSpe.agg <- merge(ddSpe.agg,tsp,by.x="sp",by.y="pk_species")

  ddSpe.agg <- subset(ddSpe.agg,habitat_name%in% c("farmland","woodland","urban"))

    write.csv(ddSpe.agg,"output/breeding_prevalence_CI.csv",row.names=FALSE)


     dres.bl <- subset(ddSpe.agg,birdlab)
    vecCol <- c("farmland"="#ffb319","woodland"="#1a9129","urban"="#ff0404")


     ttsp <- read.csv("data/espece.csv",encoding="UTF-8")
    ttsp <- subset(ttsp,select=c("pk_species","euring","french_name","english_name"))
    colnames(ttsp)[1] <- "sp"


    orderSp <- subset(ttsp,sp %in% dres.bl$sp)
    dres.bl$english_name <- factor(dres.bl$english_name, levels = orderSp$english_name)



    gg <- ggplot(dres.bl,aes(x=english_name,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + geom_errorbar(aes(ymin=med,ymax=med),position = position_dodge(),width=.7 )
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))+ labs(x="",y="Habitat prevalence",fill="Habitat",colour="Habitat")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    gg
    ggsave("output/breeding_birdlab_prevalence_habitat.png",gg,width=8,height=6)



    gg <- ggplot(subset(dres.bl,mean>0),aes(x=english_name,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))+ labs(x="",y="Habitat prevalence",fill="Habitat",colour="Habitat")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    gg
   ggsave("output/breeding_birdlab_prevalence_habitat_index.png",gg,width=8,height=6)


    ddSpe.agg <- ddSpe.agg[,1:8]

    write.csv(ddSpe.agg,"output/stoc_prevalence.csv",row.names=FALSE)

    ddSpe.agg2 <- subset(ddSpe.agg,habitat_name %in% c("farmland","woodland","urban"))
    dprev_wide <- dcast(ddSpe.agg2,sp~habitat_name,value.var="mean")
    colnames(dprev_wide) <- c("pk_species","stoc_farmland_prev_index","stoc_woodland_prev_index","stoc_urban_prev_index")

    tindic <- read.csv("../Birdlab/generic_data/espece_list_indicateur.csv",encoding="UTF-8")
   # tindic <- tindic[,c(1:15,19:21)]
   # tindic$habitat_specialisation_f <- gsub("â","�",tindic$habitat_specialisation_f)
   # colnames(tindic)[16:18] <- c("shoc_farmland_prev","shoc_woodland_prev","shoc_urban_prev")
    dim(tindic)
    tindic <- merge(tindic,dprev_wide,by="pk_species",all.x=TRUE)
    dim(tindic)
    tindic$stoc_farmland_prev <- ifelse(is.na(tindic$stoc_farmland_prev_index),FALSE,tindic$stoc_farmland_prev_index > 0)
    tindic$stoc_woodland_prev <- ifelse(is.na(tindic$stoc_woodland_prev_index),FALSE,tindic$stoc_woodland_prev_index > 0)
    tindic$stoc_urban_prev <- ifelse(is.na(tindic$stoc_urban_prev_index),FALSE,tindic$stoc_urban_prev_index > 0)

    tindic <- tindic[,sort(setdiff(1:ncol(tindic),grep("index",colnames(tindic))))]

    write.csv(tindic,"../Birdlab/generic_data/espece_list_indicateur.csv",na="",row.names=FALSE,fileEncoding="UTF-8")

    tindex <- read.csv("../Birdlab/generic_data/espece_indicateur_fonctionel.csv",encoding="UTF-8")


   tindex <-  tindex[,sort(setdiff(1:ncol(tindex),intersect(grep("index",colnames(tindex)),grep("stoc",colnames(tindex)))))]

    tindex <- merge(tindex,dprev_wide,by="pk_species",all=TRUE)

    colindex <- c("pk_species","ssi","sti","sti_europe","thermal_niche_mean","thermal_niche_range","thermal_niche_max","thermal_niche_min","stri","exp_stri","trophic_vegetation","trophic_invertebrate","trophic_vertebrate","shoc_farmland_prev_index","shoc_woodland_prev_index","shoc_urban_prev_index","stoc_farmland_prev_index","stoc_woodland_prev_index","stoc_urban_prev_index")
    tindex <- tindex[,colindex]

   write.csv(tindex,"../Birdlab/generic_data/espece_indicateur_fonctionel.csv",na="",row.names=FALSE,fileEncoding="UTF-8")


}




analysis_modif_prev <- function() {
    library(reshape2)
    tbreedingCI <- read.csv("output/breeding_prevalence_CI.csv",stringsAsFactors=FALSE)
    tbreedingCI$season <- "breeding"

    twinterCI <- read.csv("output/winter_prevalence_CI.csv",stringsAsFactors=FALSE)
    twinterCI$season <- "winter"

    vecSp <- intersect(tbreedingCI$sp,twinterCI$sp)
    tbreedingCI <- subset(tbreedingCI,sp  %in% vecSp)
    twinterCI <- subset(twinterCI,sp  %in% vecSp)

    tCI <- rbind(twinterCI,tbreedingCI)



    tbreedingBT <- read.csv("output/breeding_habitat_prev_bootstrap.csv",stringsAsFactors=FALSE)
    colnames(tbreedingBT)[3] <- "breeding_preval"

    twinterBT <- read.csv("output/winter_habitat_prev_bootstrap.csv",stringsAsFactors=FALSE)
    colnames(twinterBT)[3] <- "winter_preval"

    tbreedingBT <- subset(tbreedingBT,sp  %in% vecSp)
    twinterBT <- subset(twinterBT,sp  %in% vecSp)

    ibreeding <- sample(unique(tbreedingBT$idset),500,replace=TRUE)
    iwinter <- sample(unique(twinterBT$idset),500,replace=TRUE)

    tibreeding <- data.frame(idiff=1:length(ibreeding),idset=ibreeding)
    tiwinter <- data.frame(idiff=1:length(iwinter),idset=iwinter)

    ttbreedingBT <- merge(tbreedingBT,tibreeding,by="idset")
    ttwinterBT <- merge(twinterBT,tiwinter,by="idset")

    colnames(ttbreedingBT)[1] <- "breeding_idset"
    colnames(ttwinterBT)[1] <- "winter_idset"

    tdiff <- merge(ttbreedingBT,ttwinterBT,by=c("sp","habitat_name","idiff"))
    tdiff$winter_breeding <- tdiff$winter_preval - tdiff$breeding_preval


    ddSpe.agg <- aggregate(winter_breeding~sp+habitat_name,tdiff,quantile,c(0.025,0.5,0.975))

    ddSpe.agg <- data.frame(ddSpe.agg[,1:2],ddSpe.agg[,3])
    colnames(ddSpe.agg)[3:5] <- c("ICinf","med","ICsup")

    ddSpe.agg.mean <- aggregate(winter_breeding~sp+habitat_name,tdiff,mean)
    colnames(ddSpe.agg.mean)[3] <- c("mean")

    ddSpe.agg <- merge(ddSpe.agg,ddSpe.agg.mean,by = c("sp","habitat_name"))


    tsp <- read.csv2("data/espece_list_indicateur.csv",encoding="UTF-8")
    tsp <- tsp[,c("pk_species","french_name","english_name","birdlab","birdlab_shoc_prevalence_farmland","birdlab_shoc_prevalence_woodland","birdlab_shoc_prevalence_urban")]

    ddSpe.agg <- merge(ddSpe.agg,tsp,by.x="sp",by.y="pk_species")
    ddSpe.agg$season <- "winter - breeding"

    tCI <- rbind(tCI,ddSpe.agg)

    colnames(tCI)[1] <- "pk_species"


    tindic <- read.csv("../Birdlab/generic_data/espece_list_indicateur.csv",encoding="UTF-8",stringsAsFactors=FALSE)
    tindic <- tindic[,c("pk_species","habitat_specialisation")]
    tindic$habitat_specialisation[tindic$habitat_specialisation==""] <- "any"
    tCI <- merge(tCI,tindic,by="pk_species")




    ttsp <- read.csv("data/espece.csv",encoding="UTF-8")
    ttsp <- subset(ttsp,select=c("pk_species","euring","french_name","english_name"))
    ttsp$name_id <- paste(ttsp$english_name,"|",ttsp$pk_species)

    tCI$name_id <- paste(tCI$english_name,"|",tCI$pk_species)
    orderSp <- subset(ttsp,pk_species %in% tCI$pk_species)
    tCI$name_id<- factor(tCI$name_id, levels = orderSp$name_id)



    vecCol <- c("farmland"="#ffb319","woodland"="#1a9129","urban"="#ff0404","generalist"="#1200ff","any"="#7a7a7a")

    tCol <- as.data.frame(vecCol)
    colnames(tCol)[1] <- "colour"
    tCol$habitat_name <- rownames(tCol)

    tCI <- merge(tCI,tCol,by.x="habitat_specialisation",by.y="habitat_name")

    ttCol <- unique(tCI[,c("name_id","colour")])
    rownames(ttCol) <- ttCol$name_id
    ttCol$colour <- as.character(ttCol$colour)

    vec_vline <- levels(tCI$name_id)[1]

    gg <- ggplot(tCI,aes(x=name_id,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))+facet_grid(season~.)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
  # gg <- gg + geom_vline(aes(xintercept = vec_vline))#,colour=ttCol[as.character(orderSp$name_id),"colour"]),alpha=.5)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + geom_errorbar(aes(ymin=med,ymax=med),position = position_dodge(),width=.7 )

    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5,colour=ttCol[as.character(orderSp$name_id),"colour"]))+ labs(x="",y="Difference between  winter and breeding habitat prevalence",fill="Habitat",colour="Habitat")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    gg
    ggsave("output/diff_prevalence_habitat.png",gg,width=14,height=14)


 gg <- ggplot(subset(tCI,habitat_specialisation=="farmland"),aes(x=name_id,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))+facet_grid(season~.)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + geom_errorbar(aes(ymin=med,ymax=med),position = position_dodge(),width=.7 )

    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5,colour=vecCol["farmland"]))+ labs(x="",y="Difference between  winter and breeding habitat prevalence",fill="Habitat",colour="Habitat",title="Farmland birds")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
##    gg
    ggsave("output/diff_prevalence_habitat_spe_farmland.png",gg,width=6,height=8)




 gg <- ggplot(subset(tCI,habitat_specialisation=="woodland"),aes(x=name_id,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))+facet_grid(season~.)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + geom_errorbar(aes(ymin=med,ymax=med),position = position_dodge(),width=.7 )

    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5,colour=vecCol["woodland"]))+ labs(x="",y="Difference between  winter and breeding habitat prevalence",fill="Habitat",colour="Habitat",title="Woodland birds")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    ##gg
    ggsave("output/diff_prevalence_habitat_spe_woodland.png",gg,width=8,height=8)


 gg <- ggplot(subset(tCI,habitat_specialisation=="urban"),aes(x=name_id,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))+facet_grid(season~.)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + geom_errorbar(aes(ymin=med,ymax=med),position = position_dodge(),width=.7 )

    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5,colour=vecCol["urban"]))+ labs(x="",y="Difference between  winter and breeding habitat prevalence",fill="Habitat",colour="Habitat",title="Urban birds")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
##    gg
    ggsave("output/diff_prevalence_habitat_spe_urban.png",gg,width=6,height=8)


 gg <- ggplot(subset(tCI,habitat_specialisation=="generalist"),aes(x=name_id,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))+facet_grid(season~.)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + geom_errorbar(aes(ymin=med,ymax=med),position = position_dodge(),width=.7 )

    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5,colour=vecCol["generalist"]))+ labs(x="",y="Difference between  winter and breeding habitat prevalence",fill="Habitat",colour="Habitat",title="Generalist birds")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    ##gg
    ggsave("output/diff_prevalence_habitat_spe_generalist.png",gg,width=6,height=8)



 gg <- ggplot(subset(tCI,habitat_specialisation=="any"),aes(x=name_id,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))+facet_grid(season~.)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + geom_errorbar(aes(ymin=med,ymax=med),position = position_dodge(),width=.7 )

    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5,colour=vecCol["any"]))+ labs(x="",y="Difference between  winter and breeding habitat prevalence",fill="Habitat",colour="Habitat",title="Birds of any groups")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
##    gg
    ggsave("output/diff_prevalence_habitat_spe_any.png",gg,width=6,height=8)











    ttind <- tindex[,c(1,grep("prev",colnames(tindex)))]
    ttind <- na.omit(ttind)

    tindic <- read.csv("../Birdlab/generic_data/espece_list_indicateur.csv",encoding="UTF-8",stringsAsFactors=FALSE)
    tindic <- tindic[,c("pk_species","habitat_specialisation")]
    tindic$habitat_specialisation[tindic$habitat_specialisation==""] <- "any"
    ttind <- merge(ttind,tindic,by="pk_species")


    ttsp <- read.csv("data/espece.csv",encoding="UTF-8",stringsAsFactors=FALSE)
    ttsp <- subset(ttsp,select=c("pk_species","euring","french_name","english_name"))

    ttind <- merge(ttind,ttsp,by="pk_species")

    orderSp <- subset(ttsp,pk_species %in% ttind$pk_species)
    ttind$english_name <- factor(ttind$english_name, levels = orderSp$english_name)

    ggfarmland <- melt(ttind[,c("pk_species","english_name","stoc_farmland_prev_index","shoc_farmland_prev_index")],id.vars=c("pk_species","english_name"))
    colnames(ggfarmland)[4] <- "prevalence_value"
    ggfarmland$season <- "winter"
    ggfarmland$season[grep("stoc",ggfarmland$variable)] <- "breeding"
    ggfarmland <- merge(ggfarmland,tindic,by="pk_species")
    ggfarmland$prevalence_name <- "farmland"

ggwoodland <- melt(ttind[,c("pk_species","english_name","stoc_woodland_prev_index","shoc_woodland_prev_index")],id.vars=c("pk_species","english_name"))
    colnames(ggwoodland)[4] <- "prevalence_value"
    ggwoodland$season <- "winter"
    ggwoodland$season[grep("stoc",ggwoodland$variable)] <- "breeding"
    ggwoodland <- merge(ggwoodland,tindic,by="pk_species")
    ggwoodland$prevalence_name<- "woodland"

ggurban <- melt(ttind[,c("pk_species","english_name","stoc_urban_prev_index","shoc_urban_prev_index")],id.vars=c("pk_species","english_name"))
    colnames(ggurban)[4] <- "prevalence_value"
    ggurban$season <- "winter"
    ggurban$season[grep("stoc",ggurban$variable)] <- "breeding"
    ggurban <- merge(ggurban,tindic,by="pk_species")
    ggurban$prevalence_name<- "urban"



    vecCol <- c("farmland"="#ffb319","woodland"="#1a9129","urban"="#ff0404","generalist"="#1200ff","any"="#7a7a7a")

    ggtab <- rbind(rbind(ggfarmland,ggwoodland),ggurban)

    tsp_dec <- data.frame(pk_species=unique(ggtab$pk_species),
                          sp_decal=runif(length(unique(ggtab$pk_species)),0,4))
    ggtab <- merge(ggtab,tsp_dec,by="pk_species")
    ggtab$text_hjust <- ifelse(ggtab$season=="breeding",1.5 + ggtab$sp_dec ,-.5 - ggtab$sp_dec)

    gg <- ggplot(ggtab,aes(x=season,y=prevalence_value,color=habitat_specialisation,group=pk_species,label=pk_species)) + facet_grid(prevalence_name~.)
    gg <- gg + geom_hline(yintercept=0,size=2,color="white")
    gg <- gg + geom_point()+geom_line()
    gg <- gg + geom_hline(yintercept=0,size=2,color="white",alpha=.25)
    gg <- gg + geom_text(aes(hjust=text_hjust),size=2.5)
    gg <- gg + labs(x="season",y="Prevalence",color="Historical\nindicator\ngroups")
    gg <- gg + scale_colour_manual(values=vecCol)
##    gg
    ggsave("output/prevalence_breeding_winter.png",gg,height=12,width=12)


    ttind$farmland_prev_diff <- ttind$shoc_farmland_prev_index - ttind$stoc_farmland_prev_index
    ttind$woodland_prev_diff <- ttind$shoc_woodland_prev_index - ttind$stoc_woodland_prev_index
    ttind$urban_prev_diff <- ttind$shoc_urban_prev_index - ttind$stoc_urban_prev_index


    ggdiff <-  melt(ttind[,c("pk_species","english_name","farmland_prev_diff","woodland_prev_diff","urban_prev_diff")],id.vars=c("pk_species","english_name"))
    colnames(ggdiff)[3:4] <- c("habitat_name","diff_winter_breeding")
    ggdiff$habitat_name <- as.character(ggdiff$habitat_name)
    ggdiff$habitat_name[grep("farmland",ggdiff$habitat_name)] <- "farmland"
    ggdiff$habitat_name[grep("woodland",ggdiff$habitat_name)] <- "woodland"
    ggdiff$habitat_name[grep("urban",ggdiff$habitat_name)] <- "urban"

    ggdiff <- merge(ggdiff,tindic,by="pk_species")



    ttsp <- read.csv("data/espece.csv",encoding="UTF-8")
    ttsp <- subset(ttsp,select=c("pk_species","euring","french_name","english_name"))
    ttsp$name_id <- paste(ttsp$english_name,"|",ttsp$pk_species)

    ggdiff$name_id <- paste(ggdiff$english_name,"|",ggdiff$pk_species)
    orderSp <- subset(ttsp,pk_species %in% ggdiff$pk_species)
    ggdiff$name_id<- factor(ggdiff$name_id, levels = orderSp$name_id)


    tCol <- as.data.frame(vecCol)
    colnames(tCol)[1] <- "colour"
    tCol$habitat_name <- rownames(tCol)

    ggdiff <- merge(ggdiff,tCol,by.x="habitat_specialisation",by.y="habitat_name")

    ttCol <- unique(ggdiff[,c("name_id","colour")])
    rownames(ttCol) <- ttCol$name_id
    ttCol$colour <- as.character(ttCol$colour)

    gg <- ggplot(ggdiff,aes(x=name_id,y=diff_winter_breeding,group=habitat_name,fill=habitat_name,colour=habitat_name))
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
##    gg <- gg + geom_vline(aes(xintercept=ggdiff$name_id))
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5,colour=ttCol[as.character(orderSp$name_id),"colour"]))+ labs(x="",y="Difference between  winter and breeding habitat prevalence",fill="Habitat",colour="Habitat")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    gg
    ggsave("output/diff_prevalence_habitat.png",gg,width=16,height=8)

}




habitat_specilisation2 <- function(dataBrut=FALSE,fileAnalysis="data/analysisHabiat.csv") {
    require(reshape2)
    require(ggplot2)
    require(data.table)

    dataBrut=TRUE

    fileAnalysis="data/data_analysisHabiat.csv"

    if(dataBrut) {
        d <- read.delim("data/export_stoc04072018_155413.txt",header=TRUE,encoding="UTF-8",stringsAsFactors=FALSE)

        dd <- subset(d,d$Distance.de.contact %in% c("LESS25","LESS100")& N..Passage > 1,select=c("Code.inventaire","N..Carr�.EPS","Date","N..Passage","EXPORT_STOC_TEXT_EPS_POINT","Esp�ce","Nombre","EPS.P.Milieu"))

        dd$Esp�ce[dd$Esp�ce==""] <- "OTHERS"
        dd$Esp�ce <- substr(dd$Esp�ce,1,6)

        ddinv <- aggregate(Nombre~Code.inventaire+N..Carr�.EPS+Date+N..Passage+EXPORT_STOC_TEXT_EPS_POINT+Esp�ce+EPS.P.Milieu,dd,sum)

        ddinv$an <- as.numeric(substr(ddinv$Date,7,10))


        ddan <- aggregate(Nombre~N..Carr�.EPS+EXPORT_STOC_TEXT_EPS_POINT+Esp�ce+an,ddinv,max)


        ddan_w <- dcast(ddan,N..Carr�.EPS+EXPORT_STOC_TEXT_EPS_POINT+an ~Esp�ce,value.var="Nombre")

        ddan_w[is.na(ddan_w)] <- 0




        ddan2 <- melt(ddan_w,id.vars=c("N..Carr�.EPS","EXPORT_STOC_TEXT_EPS_POINT","an"))
        colnames(ddan2) <- c("carre","point","an","sp","ab")

        dcarre <- aggregate(ab~carre+an+sp,ddan2,sum)
        colnames(dcarre)[4] <- "sum_ab_carre"
        dcarre <- subset(dcarre,sum_ab_carre>0)



        ddpoint <- merge(ddan2,dcarre,by=c("carre","an","sp"))

      #  ddpoint <- aggregate(ab~carre+point+sp,ddan2,median)
        ddpoint$carrepoint <- paste(ddpoint$carre,ddpoint$point)



        ddhab <- unique(dd[,c("N..Carr�.EPS","EXPORT_STOC_TEXT_EPS_POINT","EPS.P.Milieu")])
        colnames(ddhab) <- c("carre","point","hab")
        ddhab$carrepoint <- paste(ddhab$carre,ddhab$point)

        habdupli <- which(duplicated(ddhab$carrepoint))

        ddhab <- ddhab[!(1:nrow(ddhab)%in%habdupli),]

        ddhab <- ddhab[,c("carrepoint","hab")]

        ddpoint <- merge(ddpoint,ddhab,by="carrepoint")
        thab <- read.csv("data/habitat.csv")
        ddpoint <- merge(ddpoint,thab,by="hab")
        ddpoint$an.factor <- as.factor(ddpoint$an)

        tsp <- read.csv("data/espece.csv")
        tbird <- read.csv2("data/species.csv")

        tsp <- subset(tsp,select=c("pk_species","french_name","english_name"))
        colnames(tsp)[1] <- "sp"
        tsp$sp_birdlab <- tsp$sp %in% tbird$id_species

        ddpoint <- merge(ddpoint,tsp,by="sp")

        write.csv(ddpoint,fileAnalysis,row.names=FALSE)
    } else {

        ddpoint <- read.csv(fileAnalysis)
    }


    require(ggplot2)

    ddpoint2 <- subset(ddpoint,sp_birdlab)
    ddpoint2 <- ddpoint2[order(ddpoint2$sp,ddpoint2$an,ddpoint2$carre,ddpoint2$point),]
    lim <- aggregate(ab~sp,ddpoint2,quantile,.95)
    colnames(lim)[2] <- "lim"
    lim$lim <- lim$lim * 1.5
    ddpoint2 <- merge(ddpoint2,lim,by="sp")
    ddpoint2$ab_lim <- ifelse(ddpoint2$ab < ddpoint2$lim,ddpoint2$ab,ddpoint2$lim)
    ddpoint2$pa <- ifelse(ddpoint2$ab==0,0,1)

    ddpoint2$sp <-  droplevels(ddpoint2$sp)
    gg <- ggplot(data=ddpoint2,aes(x=hab,y=pa,fill=habitat_name))+geom_violin()+facet_wrap(~english_name,scales="free")
    #gg <- gg + coord_trans(y = "log10")
    gg


    ddagg.pa <- aggregate(pa~hab+sp,ddpoint2,sum)
    ddagg.tot <- aggregate(pa~hab+sp,ddpoint2,length)
    colnames(ddagg.tot)[3] <- "tot"


    ddagg <- merge(ddagg.pa,ddagg.tot,by=c("hab","sp"))
    ddagg$prop <- ddagg$pa/ddagg$tot

    ddagg <- merge(ddagg,tsp,by="sp")
    ddagg <- merge(ddagg,thab,by="hab")


    gg <- ggplot(ddagg,aes(y=prop,x=hab,fill=habitat_name))+geom_bar(stat="identity")+facet_wrap(~english_name)
    gg

    ggsave("output/habitat_prop.png",gg)

   require(MASS)
   md <- glmmPQL(pa~habitat_name+0,data= subset(ddpoint2,sp=="FRIMON"),random= ~ 1| carre,family=binomial)#,correlation=corExp(form=~feederpair_agg_latitude_l93+feederpair_agg_longitude_l93,nugget = TRUE))
   smd <- summary(md)
    print(smd)




      gg <- ggplot(data=ddpoint2,aes(x=hab,y=ab_lim,fill=habitat_name))+geom_boxplot()+facet_wrap(~english_name,scales="free")
    #gg <- gg + coord_trans(y = "log10")

      gg <- ggplot(data=ddpoint2,aes(y=sum_ab_carre,x=sp))+geom_boxplot()+facet_wrap(~english_name,scales="free")
    gg

    hist(subset(ddpoint,sp=="CARCHL")$ab,100)

        require(lme4)

    md <- glmer(ab~habitat_name + (1|an)+(1|carre)+1,data = subset(ddpoint,sp=="FRICOE"),family=poisson)

    smd <- summary(md)
    print(smd)


    require(MASS)
   md <- glmmPQL(ab~habitat_name  + an.factor,data= subset(ddpoint,sp=="COCCOC"),random= ~ 1| carre,family=quasipoisson)#,correlation=corExp(form=~feederpair_agg_latitude_l93+feederpair_agg_longitude_l93,nugget = TRUE))
   smd <- summary(md)
    print(smd)


md <- glm(ab~habitat_name  + an.factor + carre + 0 ,data= subset(ddpoint,sp=="COCCOC"),family=quasipoisson)#,correlation=corExp(form=~feederpair_agg_latitude_l93+feederpair_agg_longitude_l93,nugget = TRUE))
   smd <- summary(md)
    print(smd)






  md <- glm(ab~ habitat_name,data= subset(ddpoint,sp=="FRIMON"),family=negbin)
   smd <- summary(md)
    print(smd)

hist(md$res)


 md.ab <- glmer(abundance_indicator~ feeder_kind + level + julian_winter + I(julian_winter^2) +year_factor + feederpair_agg_longitude_l93 * feederpair_agg_latitude_l93 + (1| id_feederpair_agg),data=d4,family=poisson)


    dodo <-  unique(ddpoint[,c("carrepoint","hab")])
    dodo <- merge(dodo,thab,by="hab")

    gghab <- ggplot(dodo,aes(x=factor(1),fill=habitat_name))+ geom_bar(width = 1)+
  coord_polar("y")

    ggsave("output/habitatSHOC.png",gghab)

### resampling 300 by main habitat
    dodo$grHab <- ifelse(dodo$habitat_name %in% c("farmland","forest","urban area"),dodo$habitat_name,"other")

    ddSpe <- NULL

    for(i in 1:200){

    dodo <- dodo[sample(1:nrow(dodo)),]
    dodo.dt <- data.table(dodo)
    dodo.dt <- dodo.dt[,order_gr := 1:.N, by=grHab]
    dodo2 <- as.data.frame(dodo.dt)

    dodo2 <- subset(dodo2,order_gr <= 300)
 # gghab <- ggplot(dodo2,aes(x=factor(1),fill=habitat_name))+ geom_bar(width = 1)+
 # coord_polar("y")

  #  ggsave("output/habitatSHOC_resampled.png",gghab)


    ddpoint2 <- subset(ddpoint,carrepoint %in% dodo2$carrepoint)


    ddAgghab <- aggregate(ab~sp+hab,ddpoint2,sum)
    ddAggtot <- aggregate(ab~sp,ddpoint2,sum)


    ddAgghab <- merge(ddAgghab,ddAggtot,by="sp")
    colnames(ddAgghab)[3:4] <- c("sum","tot")


        ddAgghab$prop <- ddAgghab$sum/ddAgghab$tot

    ddAgghab$set <- i

    ddSpe <- rbind(ddSpe,ddAgghab)

}

    ddSpe <- subset(ddSpe,sum>10)
    ddSpe.agg <- aggregate(prop~sp+hab,ddSpe,quantile,c(0.025,0.5,0.975))

    ddSpe.agg <- data.frame(ddSpe.agg[,1:2],ddSpe.agg[,3])
    colnames(ddSpe.agg)[3:5] <- c("ICinf","med","ICsup")


    ddSpe.agg <- merge(ddSpe.agg,thab,by="hab")

    tsp <- read.csv("data/espece.csv",encoding="UTF-8")
    tsp <- subset(tsp,select=c("pk_species","euring","french_name","english_name"))
    colnames(tsp)[1] <- "sp"

    ddSpe.agg <- merge(ddSpe.agg,tsp,by="sp")

    dres <- ddSpe.agg[order(ddSpe.agg$hab,ddSpe.agg$sp),]
    dres.main <- subset(dres,hab %in% c("A","E","D")& med > .3)
    dres.main <- dres.main[order(dres.main$sp,dres.main$med),]

    tsp.bl <- read.csv("data/species.csv",sep=";")

    dres.bl <- subset(dres,sp %in% tsp.bl$id_species & hab %in% c("A","E","D"))
    vecCol <- c("farmland"="#ffb319","forest"="#1a9129","urban area"="#9439c4")

    orderSp <- subset(tsp,sp %in% tsp.bl$id_species)
    dres.bl$english_name <- factor(dres.bl$english_name, levels = orderSp$english_name)

    gg <- ggplot(dres.bl,aes(x=english_name,y=med,group=habitat_name,fill=habitat_name,colour=habitat_name))
    gg <- gg + geom_hline(yintercept=.25,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_hline(yintercept=.25,colour="white",size=2,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ labs(x="",y="Habitat prevalence",fill="Habitat",colour="Habitat")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    ggsave("output/prevalence_habitat.png",gg)

}









habitat_specilisation_3 <- function(dataBrut=FALSE,fileAnalysis="data/analysisHabiat.csv") {
    require(reshape2)
    require(ggplot2)
    require(data.table)

   ## dataBrut=FALSE
   ## fileAnalysis="data/analysisHabiat.csv"
    if(dataBrut) {
        d <- read.delim("data/export_stoc04072018_155413.txt",header=TRUE,encoding="UTF-8")

        dd <- subset(d,d$Distance.de.contact %in% c("LESS25","LESS100")& N..Passage > 1,select=c("Code.inventaire","N..Carr�.EPS","Date","N..Passage","EXPORT_STOC_TEXT_EPS_POINT","Esp�ce","Nombre","EPS.P.Milieu"))

        dd$Esp�ce[dd$Esp�ce==""] <- "OTHERS"
        dd$Esp�ce <- substr(dd$Esp�ce,1,6)

        ddinv <- aggregate(Nombre~Code.inventaire+N..Carr�.EPS+Date+N..Passage+EXPORT_STOC_TEXT_EPS_POINT+Esp�ce+EPS.P.Milieu,dd,sum)

        ddinv$an <- as.numeric(substr(ddinv$Date,7,10))


        ddan <- aggregate(Nombre~N..Carr�.EPS+EXPORT_STOC_TEXT_EPS_POINT+Esp�ce+an,ddinv,max)


        ddan_w <- dcast(ddan,N..Carr�.EPS+EXPORT_STOC_TEXT_EPS_POINT+an ~Esp�ce,value.var="Nombre")

        ddan_w[is.na(ddan_w)] <- 0




        ddan2 <- melt(ddan_w,id.vars=c("N..Carr�.EPS","EXPORT_STOC_TEXT_EPS_POINT","an"))
        colnames(ddan2) <- c("carre","point","an","sp","ab")


        ddpoint <- aggregate(ab~carre+point+sp,ddan2,median)
        ddpoint$carrepoint <- paste(ddpoint$carre,ddpoint$point)



        ddhab <- unique(dd1[,c("N..Carr�.EPS","EXPORT_STOC_TEXT_EPS_POINT","EPS.P.Milieu")])
        colnames(ddhab) <- c("carre","point","hab")
        ddhab$carrepoint <- paste(ddhab$carre,ddhab$point)

        habdupli <- which(duplicated(ddhab$carrepoint))

        ddhab <- ddhab[!(1:nrow(ddhab)%in%habdupli),]

        ddhab <- ddhab[,c("carrepoint","hab")]

        ddpoint <- merge(ddpoint,ddhab,by="carrepoint")

        write.csv(ddpoint,fileAnalysis,row.names=FALSE)
    } else {

        ddpoint <- read.csv(fileAnalysis)
    }

    thab <- read.csv("data/habitat.csv")

    dodo <-  unique(ddpoint[,c("carrepoint","hab")])
    dodo <- merge(dodo,thab,by="hab")

    gghab <- ggplot(dodo,aes(x=factor(1),fill=habitat_name))+ geom_bar(width = 1)+
  coord_polar("y")

    ggsave("output/habitatSHOC.png",gghab)

### resampling 300 by main habitat
    dodo$grHab <- ifelse(dodo$habitat_name %in% c("farmland","forest","urban area"),dodo$habitat_name,"other")

    ddSpe <- NULL

    for(i in 1:200){

    dodo <- dodo[sample(1:nrow(dodo)),]
    dodo.dt <- data.table(dodo)
    dodo.dt <- dodo.dt[,order_gr := 1:.N, by=grHab]
    dodo2 <- as.data.frame(dodo.dt)

    dodo2 <- subset(dodo2,order_gr <= 300)
 # gghab <- ggplot(dodo2,aes(x=factor(1),fill=habitat_name))+ geom_bar(width = 1)+
 # coord_polar("y")

  #  ggsave("output/habitatSHOC_resampled.png",gghab)


    ddpoint2 <- subset(ddpoint,carrepoint %in% dodo2$carrepoint)


    ddAgghab <- aggregate(ab~sp+hab,ddpoint2,sum)
    ddAggtot <- aggregate(ab~sp,ddpoint2,sum)


    ddAgghab <- merge(ddAgghab,ddAggtot,by="sp")
    colnames(ddAgghab)[3:4] <- c("sum","tot")


        ddAgghab$prop <- ddAgghab$sum/ddAgghab$tot

    ddAgghab$set <- i

    ddSpe <- rbind(ddSpe,ddAgghab)

}

    ddSpe <- subset(ddSpe,sum>10)
    ddSpe.agg <- aggregate(prop~sp+hab,ddSpe,quantile,c(0.025,0.5,0.975))

    ddSpe.agg <- data.frame(ddSpe.agg[,1:2],ddSpe.agg[,3])
    colnames(ddSpe.agg)[3:5] <- c("ICinf","med","ICsup")


    ddSpe.agg <- merge(ddSpe.agg,thab,by="hab")

    tsp <- read.csv("data/espece.csv",encoding="UTF-8")
    tsp <- subset(tsp,select=c("pk_species","euring","french_name","english_name"))
    colnames(tsp)[1] <- "sp"

    ddSpe.agg <- merge(ddSpe.agg,tsp,by="sp")

    dres <- ddSpe.agg[order(ddSpe.agg$hab,ddSpe.agg$sp),]
    dres.main <- subset(dres,hab %in% c("A","E","D")& med > .3)
    dres.main <- dres.main[order(dres.main$sp,dres.main$med),]

    tsp.bl <- read.csv("data/species.csv",sep=";")

    dres.bl <- subset(dres,sp %in% tsp.bl$id_species & hab %in% c("A","E","D"))
    vecCol <- c("farmland"="#ffb319","forest"="#1a9129","urban area"="#9439c4")

    orderSp <- subset(tsp,sp %in% tsp.bl$id_species)
    dres.bl$english_name <- factor(dres.bl$english_name, levels = orderSp$english_name)

    gg <- ggplot(dres.bl,aes(x=english_name,y=med,group=habitat_name,fill=habitat_name,colour=habitat_name))
    gg <- gg + geom_hline(yintercept=.25,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_hline(yintercept=.25,colour="white",size=2,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ labs(x="",y="Habitat prevalence",fill="Habitat",colour="Habitat")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    ggsave("output/prevalence_habitat.png",gg)

}



habitat_Indval_winter <- function(fileAnalysis="data/winter_analysisHabiat.csv") {
    require(reshape2)
    require(ggplot2)
    require(data.table)
    require(dplyr)

    fileAnalysis="data/winter_analysisHabiat.csv"


    dpoint <- read.csv(fileAnalysis)


    thab <- read.csv("data/habitat.csv")

    dpoint <- inner_join(dpoint,thab)


    ## Prevalence indicator
    ##    Andrade, C., Chiron, F., Julliard, R., 2012. Improving the selection of focal species exposed to pesticides to support ecological risk assessments. Ecotoxicology 21, 2430�2440. https://doi.org/10.1007/s10646-012-0982-4

   dpoint$pr <- as.numeric(dpoint$ab>0)

    ddpoint <- subset(dpoint,an == 2014)

    ddhab <- as.data.frame(table(ddpoint$habitat_name))
    colnames(ddhab) <- c("hab","hab_nb")

    ddsp <- aggregate(ab~sp,ddpoint,sum)
    colnames(ddsp) <- c("sp","ab_tot")

    ddsp_hab <- aggregate(cbind(ddpoint$ab,ddpoint$pr)~hab+sp,ddpoint,sum)
    colnames(ddsp_hab) <- c("hab","sp","ab","pr")

    ddsp_hab <- merge(ddsp_hab,ddhab,by="hab")
    ddsp_hab <- merge(ddsp_hab,ddsp,by="sp")
    ddsp_hab <- merge(ddsp_hab,thab,by="hab")

    ddsp_hab$prevalence <- (ddsp_hab$ab/ddsp_hab$ab_tot)*(ddsp_hab$pr/ddsp_hab$hab_nb)


    dprevalence <- dcast(sp ~habitat_name,data=ddsp_hab,value.var="prevalence")
    colnames(dprevalence)[c(4,7,8)] <- c("woodland","urban","water")

    tsp <- read.csv2("data/espece_list_indicateur.csv",encoding="UTF-8")
    tsp <- tsp[,c("pk_species","french_name","english_name","birdlab","birdlab_shoc_prevalence_farmland","birdlab_shoc_prevalence_woodland","birdlab_shoc_prevalence_urban")]

    dprevalence <- merge(dprevalence,tsp,by.x="sp",by.y="pk_species")

    ## require(ggtern)
    ## gg <- ggtern(subset(dprevalence,birdlab),aes(farmland,woodland,urban,label=sp,color=birdlab_shoc_prevalence_farmland))+geom_text(size=2)
    ## gg

### resampling 300 by main habitat
    dodo$grHab <- ifelse(dodo$habitat_name %in% c("farmland","forest","urban area"),as.character(dodo$habitat_name),"other")

        dddprev <- NULL

    for(i in 1:400){

        dodo_<- dodo[sample(1:nrow(dodo)),]
        dodo.dt <- data.table(dodo_)
        dodo.dt <- dodo.dt[,order_gr := 1:.N, by=grHab]
        dodo2 <- as.data.frame(dodo.dt)

        dodo2 <- subset(dodo2,order_gr <= 300)
        ## gghab <- ggplot(dodo2,aes(x=factor(1),fill=habitat_name))+ geom_bar(width = 1)+
        ## coord_polar("y")
        ##  ggsave("output/habitatSHOC_resampled.png",gghab)


        ddpoint2 <- subset(ddpoint,carrepoint %in% dodo2$carrepoint)

        ddsp <- aggregate(ab~sp,ddpoint2,sum)
        colnames(ddsp) <- c("sp","ab_tot")

        ddsp_hab <- aggregate(ab ~ hab+sp,ddpoint2,sum)
        colnames(ddsp_hab) <- c("hab","sp","ab")

        ddsp_hab <- merge(ddsp_hab,ddsp,by="sp")

        ddsp_hab$prevalence <- (ddsp_hab$ab/ddsp_hab$ab_tot)
        ddsp_hab <- merge(ddsp_hab,thab,by="hab")

        dprevalence <- dcast(sp ~habitat_name,data=ddsp_hab,value.var="prevalence")
        colnames(dprevalence)[c(4,7,8)] <- c("woodland","urban","water")
        dprevalence[is.na(dprevalence)] <- 0

        dprevalence <- subset(dprevalence,(dprevalence$farmland + dprevalence$woodland + dprevalence$urban)>0)
        dprevalence$sum3hab <- (dprevalence$farmland + dprevalence$woodland + dprevalence$urban)
     #   dprevalence <- subset(dprevalence,sum3hab>.75)
        dprev <- dprevalence[,c("sp","farmland","woodland","urban")]

        ddprev <- melt(id.vars="sp",data=dprev)
        colnames(ddprev)[2:3] <- c("habitat_name","preval")
        ddprev$preval <- ddprev$preval - 0.3
        ddprev$idset <- i

        dddprev <- rbind(dddprev,ddprev)
    }



    write.csv(dddprev,"output/winter_habitat_prev_bootstrap.csv",row.names=FALSE)


    ddSpe.nb <- aggregate(preval~sp+habitat_name,dddprev,length)

    ddSpe.agg <- aggregate(preval~sp+habitat_name,dddprev,quantile,c(0.025,0.5,0.975))

    ddSpe.agg <- data.frame(ddSpe.agg[,1:2],ddSpe.agg[,3])
    colnames(ddSpe.agg)[3:5] <- c("ICinf","med","ICsup")

    ddSpe.agg.mean <- aggregate(preval~sp+habitat_name,dddprev,mean)
    colnames(ddSpe.agg.mean)[3] <- c("mean")

    ddSpe.agg <- merge(ddSpe.agg,ddSpe.agg.mean,by = c("sp","habitat_name"))


    ddSpe.agg <- merge(ddSpe.agg,tsp,by.x="sp",by.y="pk_species")


    ddSpe.agg <- subset(ddSpe.agg,habitat_name%in% c("farmland","woodland","urban"))

    write.csv(ddSpe.agg,"output/winter_prevalence_CI.csv",row.names=FALSE)



    dres.bl <- subset(ddSpe.agg,birdlab)



    vecCol <- c("farmland"="#ffb319","woodland"="#1a9129","urban"="#ff0404")


    ttsp <- read.csv("data/espece.csv",encoding="UTF-8")
    ttsp <- subset(ttsp,select=c("pk_species","euring","french_name","english_name"))
    colnames(ttsp)[1] <- "sp"


    orderSp <- subset(ttsp,sp %in% dres.bl$sp)
    dres.bl$english_name <- factor(dres.bl$english_name, levels = orderSp$english_name)



    gg <- ggplot(dres.bl,aes(x=english_name,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + geom_errorbar(aes(ymin=ICinf,ymax=ICsup),position = position_dodge(),width=.7 )
    gg <- gg + geom_errorbar(aes(ymin=med,ymax=med),position = position_dodge(),width=.7 )
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))+ labs(x="",y="Habitat prevalence",fill="Habitat",colour="Habitat")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    ## gg
   ggsave("output/winter_birdlab_prevalence_habitat.png",gg,width=8,height=6)



    gg <- ggplot(subset(dres.bl,mean>0),aes(x=english_name,y=mean,group=habitat_name,fill=habitat_name,colour=habitat_name))
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2)
    gg <- gg + geom_bar(stat="identity", position="dodge",width=.7,alpha=.5)
    gg <- gg + geom_hline(yintercept=0,colour="white",size=2,alpha=.5)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))+ labs(x="",y="Habitat prevalence",fill="Habitat",colour="Habitat")
    gg <- gg + scale_fill_manual(values=vecCol)+scale_colour_manual(values=vecCol)
    ## gg
    ggsave("output/winter_birdlab_prevalence_habitat_index.png",gg,width=8,height=6)




    ddSpe.agg <- read.csv("output/winter_prevalence_CI.csv")

    dprev_wide <- dcast(ddSpe.agg,sp~habitat_name,value.var="mean")
    dprev_wide <- dprev_wide[,c("sp","farmland","woodland","urban")]
    colnames(dprev_wide) <- c("pk_species","shoc_farmland_prev_index","shoc_woodland_prev_index","shoc_urban_prev_index")

    tindic <- read.csv("../Birdlab/generic_data/espece_list_indicateur.csv",encoding="UTF-8")
    dim(tindic)
    tindic <- merge(tindic,dprev_wide,by="pk_species",all.x=TRUE)
    dim(tindic)
    tindic$shoc_farmland_prev <- ifelse(is.na(tindic$shoc_farmland_prev_index),FALSE,tindic$shoc_farmland_prev_index > 0)
    tindic$shoc_woodland_prev <- ifelse(is.na(tindic$shoc_woodland_prev_index),FALSE,tindic$shoc_woodland_prev_index > 0)
    tindic$shoc_urban_prev <- ifelse(is.na(tindic$shoc_urban_prev_index),FALSE,tindic$shoc_urban_prev_index > 0)

    tindic <- tindic[,sort(setdiff(1:ncol(tindic),grep("index",colnames(tindic))))]

    write.csv(tindic,"../Birdlab/generic_data/espece_list_indicateur.csv",na="",row.names=FALSE,fileEncoding="UTF-8")

    tindex <- read.csv("../Birdlab/generic_data/espece_indicateur_fonctionel.csv",encoding="UTF-8")
    tindex <-  tindex[,sort(setdiff(1:ncol(tindex),intersect(grep("index",colnames(tindex)),grep("shoc",colnames(tindex)))))]

    tindex <- merge(tindex,dprev_wide,by="pk_species",all=TRUE)

    colindex <- c("pk_species","ssi","sti","sti_europe","thermal_niche_mean","thermal_niche_range","thermal_niche_max","thermal_niche_min","stri","exp_stri","trophic_vegetation","trophic_invertebrate","trophic_vertebrate","shoc_farmland_prev_index","shoc_woodland_prev_index","shoc_urban_prev_index")
    colindex <- c(colindex,setdiff(colnames(tindex),colindex))
    tindex <- tindex[,colindex]

    write.csv(tindex,"../Birdlab/generic_data/espece_indicateur_fonctionel.csv",na="",row.names=FALSE,fileEncoding="UTF-8")


}
