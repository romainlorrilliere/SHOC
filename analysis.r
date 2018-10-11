#######
## Analysis SHOC data
######




habitat_specilisation<- function(dataBrut=FALSE,fileAnalysis="data/analysisHabiat.csv") {
    require(reshape2)
    require(ggplot2)
    require(data.table)

    dataBrut=FALSE
    fileAnalysis="data/analysisHabiat.csv"
    if(dataBrut) {
        d <- read.delim("data/export_shoc_04072018_155413.txt",stringsAsFactors=FALSE,header=TRUE,encoding="UTF-8")

        dd <- subset(d,d$Distance.de.contact %in% c("LESS25","LESS100","LESS200","U")& N..Passage > 0,select=c("Code.inventaire","N..Carré.EPS","Date","N..Passage","EXPORT_STOC_TEXT_EPS_POINT","Espèce","Nombre","EPS.P.Milieu"))

        dd$Espèce[dd$Espèce==""] <- "OTHERS"
        dd$Espèce <- substr(dd$Espèce,1,6)

        ddinv <- aggregate(Nombre~Code.inventaire+N..Carré.EPS+Date+N..Passage+EXPORT_STOC_TEXT_EPS_POINT+Espèce+EPS.P.Milieu,dd,sum)

        ddinv$an <- as.numeric(substr(ddinv$Date,7,10))


        ddan <- aggregate(Nombre~N..Carré.EPS+EXPORT_STOC_TEXT_EPS_POINT+Espèce+an,ddinv,max)



        ddan_w <- dcast(ddan,N..Carré.EPS+EXPORT_STOC_TEXT_EPS_POINT+an ~Espèce,value.var="Nombre")

        ddan_w[is.na(ddan_w)] <- 0




        ddan2 <- melt(ddan_w,id.vars=c("N..Carré.EPS","EXPORT_STOC_TEXT_EPS_POINT","an"))
        colnames(ddan2) <- c("carre","point","an","sp","ab")




        ddcarre <- aggregate(Nombre~N..Carré.EPS+an+Espèce,ddinv,max)
        colnames(ddcarre)<- c("carre","an","sp","ab_carre")


        ddpoint <- merge(ddan2,ddcarre,by=c("carre","an","sp"))
        ddpoint$log_ab <- log(ddan3$ab+1)
        ddpoint$carrepoint <- paste(ddpoint$carre,ddpoint$point)


        ddhab <- unique(dd[,c("N..Carré.EPS","EXPORT_STOC_TEXT_EPS_POINT","EPS.P.Milieu")])
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
    ##    Andrade, C., Chiron, F., Julliard, R., 2012. Improving the selection of focal species exposed to pesticides to support ecological risk assessments. Ecotoxicology 21, 2430–2440. https://doi.org/10.1007/s10646-012-0982-4


    ddpoint$pr <- as.numeric(ddpoint$ab>0)

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
    require(ggtern)

    gg <- ggtern(subset(dprevalence,birdlab),aes(farmland,woodland,urban,label=sp,color=birdlab_shoc_prevalence_farmland))+geom_text(size=2)
gg

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







habitat_specilisation2 <- function(dataBrut=FALSE,fileAnalysis="data/analysisHabiat.csv") {
    require(reshape2)
    require(ggplot2)
    require(data.table)

    dataBrut=TRUE

    fileAnalysis="data/data_analysisHabiat.csv"

    if(dataBrut) {
        d <- read.delim("data/export_stoc04072018_155413.txt",header=TRUE,encoding="UTF-8",stringsAsFactors=FALSE)

        dd <- subset(d,d$Distance.de.contact %in% c("LESS25","LESS100")& N..Passage > 1,select=c("Code.inventaire","N..Carré.EPS","Date","N..Passage","EXPORT_STOC_TEXT_EPS_POINT","Espèce","Nombre","EPS.P.Milieu"))

        dd$Espèce[dd$Espèce==""] <- "OTHERS"
        dd$Espèce <- substr(dd$Espèce,1,6)

        ddinv <- aggregate(Nombre~Code.inventaire+N..Carré.EPS+Date+N..Passage+EXPORT_STOC_TEXT_EPS_POINT+Espèce+EPS.P.Milieu,dd,sum)

        ddinv$an <- as.numeric(substr(ddinv$Date,7,10))


        ddan <- aggregate(Nombre~N..Carré.EPS+EXPORT_STOC_TEXT_EPS_POINT+Espèce+an,ddinv,max)


        ddan_w <- dcast(ddan,N..Carré.EPS+EXPORT_STOC_TEXT_EPS_POINT+an ~Espèce,value.var="Nombre")

        ddan_w[is.na(ddan_w)] <- 0




        ddan2 <- melt(ddan_w,id.vars=c("N..Carré.EPS","EXPORT_STOC_TEXT_EPS_POINT","an"))
        colnames(ddan2) <- c("carre","point","an","sp","ab")

        dcarre <- aggregate(ab~carre+an+sp,ddan2,sum)
        colnames(dcarre)[4] <- "sum_ab_carre"
        dcarre <- subset(dcarre,sum_ab_carre>0)



        ddpoint <- merge(ddan2,dcarre,by=c("carre","an","sp"))

      #  ddpoint <- aggregate(ab~carre+point+sp,ddan2,median)
        ddpoint$carrepoint <- paste(ddpoint$carre,ddpoint$point)



        ddhab <- unique(dd[,c("N..Carré.EPS","EXPORT_STOC_TEXT_EPS_POINT","EPS.P.Milieu")])
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

        dd <- subset(d,d$Distance.de.contact %in% c("LESS25","LESS100")& N..Passage > 1,select=c("Code.inventaire","N..Carré.EPS","Date","N..Passage","EXPORT_STOC_TEXT_EPS_POINT","Espèce","Nombre","EPS.P.Milieu"))

        dd$Espèce[dd$Espèce==""] <- "OTHERS"
        dd$Espèce <- substr(dd$Espèce,1,6)

        ddinv <- aggregate(Nombre~Code.inventaire+N..Carré.EPS+Date+N..Passage+EXPORT_STOC_TEXT_EPS_POINT+Espèce+EPS.P.Milieu,dd,sum)

        ddinv$an <- as.numeric(substr(ddinv$Date,7,10))


        ddan <- aggregate(Nombre~N..Carré.EPS+EXPORT_STOC_TEXT_EPS_POINT+Espèce+an,ddinv,max)


        ddan_w <- dcast(ddan,N..Carré.EPS+EXPORT_STOC_TEXT_EPS_POINT+an ~Espèce,value.var="Nombre")

        ddan_w[is.na(ddan_w)] <- 0




        ddan2 <- melt(ddan_w,id.vars=c("N..Carré.EPS","EXPORT_STOC_TEXT_EPS_POINT","an"))
        colnames(ddan2) <- c("carre","point","an","sp","ab")


        ddpoint <- aggregate(ab~carre+point+sp,ddan2,median)
        ddpoint$carrepoint <- paste(ddpoint$carre,ddpoint$point)



        ddhab <- unique(dd1[,c("N..Carré.EPS","EXPORT_STOC_TEXT_EPS_POINT","EPS.P.Milieu")])
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


