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
