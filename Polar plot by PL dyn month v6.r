#setwd("C:\\Users\\glaboure\\Documents\\R\\Test_DPM2")
#setwd(".././data")
setwd("S:\\Procurement\\Commun\\SIOP\\Central org SIOP\\N. R and shiny\\data")
source(".././program/polar plot.R")
###
######
##### detemine 6 last months and last 24 months
date<-Sys.Date()
# date character string containing POSIXct date
date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
mon<-list()
year<-list()
six_months<-list()
mon[1] <- date.lt$mon+1
year[1] <- date.lt$year
for (i in 1:6){
  mon[i+1]<-as.integer(mon[i])-1
  year[i+1]<-as.integer(year[i])-as.integer(mon[i+1]==0)# if month is December remove a year
  mon[i+1]=as.integer(mon[i+1])+as.integer(mon[i+1]==0)*12
  six_months[i]<-format(ISOdate(as.integer(year[i+1])+1900,mon[i+1],1),"%Y%m")
}
six_months<-unlist(rev(six_months))

mon<-list()
year<-list()
last_24m<-list()
mon[1] <- date.lt$mon+1
year[1] <- date.lt$year
for (i in 1:24){
  mon[i+1]<-as.integer(mon[i])-1
  year[i+1]<-as.integer(year[i])-as.integer(mon[i+1]==0)# if month is December remove a year
  mon[i+1]=as.integer(mon[i+1])+as.integer(mon[i+1]==0)*12
  last_24m[i]<-format(ISOdate(as.integer(year[i+1])+1900,mon[i+1],1),"%Y%m")
}
last_24m<-unlist(rev(last_24m))

######

#setwd("C:\\Users\\glaboure\\Documents\\R\\Test_DPM2")
library(plyr)
library(data.table)
library(reshape2)
library(dplyr)
library(stringr)

DivName<-read.csv2("DivName.csv")
DivName<-data.table(DivName, key='Div')

lasttable<-NULL

###### Start of function to remove leading 0

remove_leading_0<-function(ordered_vector)
{
  aaa<-0
  bbb<-0
  indiceStart<-0
  while(aaa==0 & indiceStart<length(ordered_vector)[1])
  {
    indiceStart<-indiceStart+1
    if(substr(ordered_vector[indiceStart],1,1)>=0)
    {aaa<-1}    
  }
  
  indiceEnd<-indiceStart
  
  while(bbb==0)
  {
    if(substr(ordered_vector[indiceEnd+1],1,1)==0)
    {indiceEnd<-indiceEnd+1} 
    else 
    {bbb<-1}
  }
  ordered_vector[indiceStart:indiceEnd]<-sapply(ordered_vector[indiceStart:indiceEnd], function(x) {sub("^[0]+", "",x)})
  return(ordered_vector)
}

###### End of function to remove leading 0

###### Start of function to clean for VST
#### try to load 2 times of the data, first w/o VST, 2nd with VST, then split the 2nd into TR01 and DB01

cleanvst <- function(x){
  
  x1 <- x[x$Div != "D10",]
  
  t <- select(DivName, c(1,3))
  
  x1 <- left_join(x1, t, by = "Div")
  
  x1 <- data.table(x1,key='Material,Div,CustGr')
  
  x2 <- x[x$Div == "D10",]
  
  x2 <- data.table(x2,key='Material,Div,CustGr')
  
  x2 <- left_join(x2, t, by = "Div")
  
  x2 <- data.table(x2,key='Material,Div,CustGr')
  
  tr01 <- x2[str_sub(x2$Material,-5,-1) == "_TR01",]
  
  tr01$Material <-  str_replace(tr01$Material,"_TR01","")
  
  db01 <- x2[str_sub(x2$Material,-5,-1) == "_DB01",]
  
  db01$Platform <- str_replace(db01$Platform, "ISL","DBI")
  db01$Material <-  str_replace(db01$Material,"_DB01","")
  
  x <- unique(x)
  
  x <- rbind(x1,tr01,db01)
  
  return(x)
}


cleanvst2 <- function(x)
{
  x <- as.data.frame(x)
  
  x1 <- x[x$Platform != "ISL",]
  
  x2 <- x[x$Platform == "ISL",]
  
  
  tr01 <- x2[str_sub(x2$Material,-5,-1) == "_TR01",]
  
  tr01$Material <-  str_replace(tr01$Material,"_TR01","")
  
  db01 <- x2[str_sub(x2$Material,-5,-1) == "_DB01",]
  
  db01$Platform <- str_replace(db01$Platform, "ISL","DBI")
  
  db01$Material <-  str_replace(db01$Material,"_DB01","")
  
  x <- unique(x)
  
  x <- rbind(x1,tr01,db01)
}
###### End of function clean

#########Import DP M-2
######


DPM2<-subset(
  read.table("DP_M-2_6m.txt",sep=";",fileEncoding = "UTF-16",col.names=c("DS","Div","Material","CustGr","Month","DPM2"),colClasses=c("Material"="character"))[,-1]
  ,Div %in% c("D02","D03","D05","D06","D07","D08","D09","D10","D13","D20"))
DPM2<-data.table(DPM2, key='Material,Div,CustGr,Month')
DPM2$Material<-remove_leading_0(DPM2$Material)  ###Remove leading 0
DPM2<-data.table(DPM2, key='Material,Div,CustGr,Month')
DPM2<-dcast.data.table(DPM2,Material+Div+CustGr~Month,
                       fill=as.integer(0),value.var='DPM2')
for (Months in names(DPM2)[!is.na(as.numeric(names(DPM2)))])
{setnames(DPM2,Months,paste("DP_",Months,sep=""))}  ##### Add demand type before month in column labels
DPM2 <- cleanvst(DPM2)
DPM2<-data.table(DPM2, key='Material,Div,CustGr,Platform')
######


#########Import Demand
######

#########Import Demand
######

demandN<-subset(
  read.table("HISTatDFU2.txt",sep=";",fileEncoding = "UTF-16",colClasses=c("Material"="character"),col.names=c("Div","Material","CustGr",paste("HDN_",last_24m,sep="")))
  ,CustGr %in% c("NORMAL","OSCARO","G_HORSBUR","G_PLUE","SOUTH","BALKANS","NORDIC","AT_CH","C_ZFTRAD","DIRECTSWF","MIDDLEAST","BELARUS","ISTANBUL","DUBAI"))
demandN <- cleanvst(demandN)
demandN<-data.table(demandN, key='Material,Div,CustGr,Platform')
demandN$Material<-remove_leading_0(demandN$Material)  ###Remove leading 0
demandN<-data.table(demandN, key='Material,Div,CustGr,Platform')

demandD<-subset(
  read.table("HISTDDatDFU2.txt",sep=";",fileEncoding = "UTF-16",colClasses=c("Material"="character"),col.names=c("Div","Material","CustGr",paste("HDD_",last_24m,sep="")))
  ,CustGr %in% c("NORMAL","OSCARO","G_HORSBUR","G_PLUE","SOUTH","BALKANS","NORDIC","AT_CH","C_ZFTRAD","DIRECTSWF","MIDDLEAST","BELARUS","ISTANBUL","DUBAI"))
######correction for SWF demand recording specificity 
demandD <- cleanvst(demandD)
demandSWF<-subset(demandN,CustGr=="DIRECTSWF")
demandD<-rbind(demandD,demandSWF,use.names=FALSE)
demandSWF<-NULL
demandD<-data.table(demandD, key='Material,Div,CustGr,Platform')
demandD$Material<-remove_leading_0(demandD$Material)  ###Remove leading 0
demandD<-data.table(demandD, key='Material,Div,CustGr,Platform')

demandZ<-subset(
  read.table("HISTZEatDFU2.txt",sep=";",fileEncoding = "UTF-16",colClasses=c("Material"="character"),col.names=c("Div","Material","CustGr",paste("HDE_",last_24m,sep="")))
  ,CustGr %in% c("NORMAL","OSCARO","G_HORSBUR","G_PLUE","SOUTH","BALKANS","NORDIC","AT_CH","C_ZFTRAD","DIRECTSWF","MIDDLEAST","BELARUS","ISTANBUL","DUBAI"))
demandZ <- cleanvst(demandZ)
demandZ<-data.table(demandZ, key='Material,Div,CustGr,Platform')
demandZ$Material<-remove_leading_0(demandZ$Material)  ###Remove leading 0
demandZ<-data.table(demandZ, key='Material,Div,CustGr,Platform')

######keep only last 6 months
demandN<-demandN[,c(1:3,22:28),with=FALSE]
demandD<-demandD[,c(1:3,22:28),with=FALSE]
demandZ<-demandZ[,c(1:3,22:28),with=FALSE]

######


#########Unique keys
######
listkey<-rbind(DPM2[,c("Material","Div","CustGr","Platform"),with=FALSE],demandN[,c("Material","Div","CustGr","Platform"),with=FALSE],demandD[,c("Material","Div","CustGr","Platform"),with=FALSE],demandZ[,c("Material","Div","CustGr","Platform"),with=FALSE])
listkey<-data.table(unique(listkey, key='Material,Div,CustGr,Platform'))

# Corresp<-data.table(Div=c("D02","D03","D05","D06","D07","D08","D09","D10","D10","D13","D20"),Platform=c("BRT","BRT","GEE","SAA","HED","REH","HED","ISL","DBI","OCW","MOW"),key='Div')
# listkey<-data.table(listkey, key='Div,Material,CustGr')
# listkey<-Corresp[listkey, allow.cartesian = TRUE]
# listkey<-data.table(listkey, key='Material,Div,CustGr')
######

#########Import DP families
######
DPfamily<-read.csv2("DPfamPL.txt",colClasses=c("Material"="character","Product.Line"="character"))
DPfamily<-data.table(DPfamily, key='Material')
DPfamily$Material<-remove_leading_0(DPfamily$Material)  ###Remove leading 0
DPfamily<-data.table(DPfamily, key='Material')
######

#########Import PAP
######
Forex_rate<-subset(
  read.table("Forex rate.txt",sep=";",fileEncoding = "UTF-16",col.names=c("currency","rate"),colClasses=c("currency"="character","rate"="numeric"))
  ,currency %in% c("GBP","PLZ","TRY","TRY1","RUB"))
Forex_rate<-cbind(Forex_rate,data.frame(Platform=c("REH","OCW","ISL","DBI","MOW")))
Forex_rate<-rbind(Forex_rate,data.frame(currency=rep("EUR",4),rate=rep(1,4),Platform=c("BRT","GEE","SAA","HED")))
Forex_rate<-data.table(Forex_rate, key='Platform')

PAP<-subset(
  read.table("PAP.txt",sep=";",fileEncoding = "UTF-16",col.names=c("Platform","Material","PAP"),colClasses=c("PAP"="numeric","Material"="character"),dec=".")
  ,Platform %in% c("BRT","GEE","SAA","HED","REH","ISL","DBI","OCW","MOW"))
PAP<-cleanvst2(PAP)
PAP<-data.table(PAP, key='Material,Platform')
PAP$Material<-remove_leading_0(PAP$Material)  ###Remove leading 0
PAP<-PAP[eval(dim(PAP)[1]:1),]  ###Reverse order as leading 0 are the one we suppose to be removed by unique()
PAP<-data.table(PAP, key='Material,Platform')

PAP<-unique(PAP,key='Material,Platform')
PAP<-data.table(PAP, key='Platform,Material')
PAP<-Forex_rate[PAP][,list(Platform,Material,PAP_EUR=PAP*rate,PAP_LC=PAP)]
PAP<-data.table(PAP, key='Material,Platform')
######

#########Import RP families
######
RPfamily<-read.table("RP_Family2.txt",sep=";",fileEncoding = "UTF-16",col.names=c("Platform","Material","RPfamily","Supplier","empty"),colClasses=c("Material"="character","RPfamily"="character"))[,1:4]
RPfamily$Supplier <-  str_replace(RPfamily$Supplier,"803967","915206")
RPfamily<-cleanvst2(RPfamily)
RPfamily<-data.table(RPfamily, key='Material,Platform')
RPfamily$Material<-remove_leading_0(RPfamily$Material)  ###Remove leading 0
RPfamily<-data.table(RPfamily, key='Material,Platform')
RPfamily<-unique(RPfamily) ### Remove duplicate (can exist because of leading 0 making 2 keys...)
######

##### Import suppliers list for thr plots 
vendorlist<-fread("SupplierName_v2.txt",colClasses =c(rep("character",3)))
#####

lasttable<-data.table(demandZ[listkey],key='Material,Div,CustGr,Platform')
lasttable<-data.table(demandN[lasttable],key='Material,Div,CustGr,Platform')
lasttable<-data.table(DPM2[lasttable],key='Material,Div,CustGr,Platform')
lasttable<-data.table(DPfamily[lasttable],key='Material,Div,CustGr,Platform')
lasttable<-data.table(lasttable, key='Material,Platform,Div,CustGr,Platform')
lasttable<-data.table(PAP[lasttable],key='Material,Platform,Div,CustGr,Platform')
lasttable<-data.table(lasttable, key='Material,Platform,Div,CustGr,Platform')
lasttable<-RPfamily[lasttable]
lasttable_wo_vsao <- lasttable[lasttable$Div != "D03",]


setwd(".././output")
write.table(lasttable,file = "ouputTOT.csv",na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)
platf<-c("BRT","HED","GEE","SAA","REH","ISL","DBI","OCW","MOW")
for (i in 1:9){
  write.table(subset(lasttable,Platform == platf[i])
              ,file = paste("Data",platf[i],".csv",sep="")
              ,na = "0",dec = ",",quote=FALSE,sep=";",col.names=NA)
  }


#### per Div / PL ####

lasttable<-data.table(lasttable, key='Product.Line,Div,CustGr,Material')
initdf1<-subset(lasttable[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Product.Line,Div)],Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))

initdf1tot<-subset(lasttable[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Product.Line)],Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
initdf1tot<-cbind(initdf1tot,data.frame("Div"=rep("TOT",13)))

initdf1<-rbind(initdf1,initdf1tot,use.names=TRUE)

setnames(initdf1,c("Product.Line","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
initdf1<-data.frame(initdf1)
initdf1[mapply(is.infinite, initdf1)] <- 0
initdf1[mapply(is.na, initdf1)] <- 0


pdf(paste("./pdf files/Div ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()

div<-c("TOT","D02","D06","D08","D03","D07","D09","D13","D10","D05","D20")
divname<-c("TOT","VSF","VSI","VSUK","VSAO","VSG","VSBE","VSEE","VST","VSE","VSR")

for (i in 1:11){
  dfi<-initdf1[initdf1$Div==div[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=divname[i])
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],ncol=3)
dev.off()



#### per PL / Platform ####

lasttable_wo_vsao<-data.table(lasttable_wo_vsao, key='Product.Line,Platform')
initdf2<-subset(lasttable_wo_vsao[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Platform,Product.Line)],Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS") & Platform %in% c("BRT","GEE","SAA","HED","REH","ISL","DBI","OCW","MOW"))


##Add total (with containTotal option activated)
initdf2tot<-subset(lasttable_wo_vsao[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Product.Line)],Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
initdf2tot<-cbind(initdf2tot,data.frame("Platform"=rep("TOT",13)))

initdf2<-rbind(initdf2,initdf2tot,use.names=TRUE)


setnames(initdf2,c("Platform","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
initdf2<-data.frame(initdf2)
initdf2[mapply(is.infinite, initdf2)] <- 0
initdf2[mapply(is.na, initdf2)] <- 0

###use initdf1tot to determine order
PLordered<-as.character(unlist(initdf1tot[order(-initdf1tot$demand),][,1,with=FALSE],2))

pdf(paste("./pdf files/PL-Platform ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()
#div<-c("VTR","VWS","VLS","VEC","VES","BRA","VCC","ELE","IGN","VSD","VSS","POP","FLT")
div<-PLordered

for (i in 1:13){
  dfi<-initdf2[initdf2$Product.Line==div[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, ContainTotal=TRUE, division=div[i]) #Note that ContainTotal = TRUE
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],ncol=3)
dev.off()


#### Per DP family / PL ####

lasttable_wo_vsao<-data.table(lasttable_wo_vsao, key='Product.Line,DP.Family')
lasttable3<-subset(lasttable_wo_vsao,Platform %in% c("BRT","GEE","SAA","HED","REH","ISL","DBI","OCW","MOW"))
lasttable3<-subset(lasttable_wo_vsao[,list("demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
)
            ,by=list(DP.Family,Product.Line)]
            ,Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
lasttable3<-lasttable3[order(-lasttable3$demand)]

lasttable3<-ddply(lasttable3,.(Product.Line),transform,DPbis=1)
lasttable3<-ddply(lasttable3,.(Product.Line),transform,DPbis=cumsum(DPbis))
lasttable3<-ddply(lasttable3,.(Product.Line),transform,DPbis=ifelse(DPbis<10,as.character(DP.Family),paste("other",Product.Line)))
lasttable3<-data.table(lasttable3, key='Product.Line,DP.Family')
initdf3<-lasttable3[,c(1,2,4),with=FALSE][lasttable_wo_vsao]
initdf3<-subset(initdf3[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                             +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                             +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(DPbis,Product.Line)],Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
setnames(initdf3,c("DPbis","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))

initdf3<-data.frame(initdf3)
initdf3[mapply(is.infinite, initdf3)] <- 0
initdf3[mapply(is.na, initdf3)] <- 0

divs <- list()

#div<-c("D02","D03","D05","D06","D07","D08","D09","D10","D13","D20")

pdf(paste("./pdf files/PL_DPfam ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)

div<-c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS")
for (i in 1:13){
  dfi<-initdf3[initdf3$Product.Line==div[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=div[i],sizePL=3)
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],ncol=3)
dev.off()


#### Per RP family / Supplier ####

lasttable_wo_vsao<-data.table(lasttable_wo_vsao, key='RPfamily')
lasttable4<-cbind(data.frame(Supplier=substr(lasttable_wo_vsao$RPfamily,1,4),stringsAsFactors=FALSE),lasttable_wo_vsao)
lasttable4<-data.table(lasttable4, key='Supplier,RPfamily')
lasttable4<-subset(lasttable4[,list("demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
)
,by=list(Supplier,RPfamily)])
lasttable4<-lasttable4[order(-lasttable4$demand)]

lasttable4<-ddply(lasttable4,.(Supplier),transform,DPbis=1)
lasttable4<-ddply(lasttable4,.(Supplier),transform,DPbis=cumsum(DPbis))
lasttable4<-ddply(lasttable4,.(Supplier),transform,DPbis=ifelse(DPbis<10,as.character(RPfamily),paste("other",Supplier)))
lasttable4<-data.table(lasttable4, key='RPfamily')
initdf4<-lasttable4[,c(1,2,4),with=FALSE][lasttable_wo_vsao]
initdf4<-subset(initdf4[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                             +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                             +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(DPbis,Supplier)])
setnames(initdf4,c("DPbis","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
initdf4<-data.frame(initdf4)
initdf4[mapply(is.infinite, initdf4)] <- 0
initdf4[mapply(is.na, initdf4)] <- 0

supplier_select<-"VW02"
dfi<-initdf4[initdf4$Supplier==supplier_select,c(1,3:9)]
dfi<-melt(dfi,c("PL","demand"))
polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=supplier_select,sizePL=3)




#### per Div / PL / CustGr ####

initdf5<-subset(lasttable_wo_vsao[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Product.Line,Div,CustGr)],Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))


setnames(initdf5,c("Product.Line","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
initdf5<-data.frame(initdf5)
initdf5[mapply(is.infinite, initdf5)] <- 0
initdf5[mapply(is.na, initdf5)] <- 0
initdf5[initdf5$CustGr=="C_ZFTRAD",c(1,3)]<-c("VT_ZF","NORMAL")


pdf(paste("./pdf files/CustGr ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()

div<-c("D07","D07","D02","D02","D06","D06","D10","D10")
custgr<-c("NORMAL","DIRECTSWF","OSCARO","NORMAL","BALKANS","NORMAL","MIDDLEAST","ISTANBUL")
divname<-c("VSGN","VSG_DD","VSFO","VSFN","BALKANS","VSIN","VSTM","VSTN")

for (i in 1:8){
  dfi<-initdf5[initdf5$Div==div[i] & initdf5$CustGr==custgr[i],c(1,4:10)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=divname[i])
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],ncol=3)
dev.off()



#### Per DP family / PL / filter one div ####

division<-c("D02","D03","D05","D06","D07","D08","D09","D10","D13","D20")
divname<-c("VSF","VSAO","VSE","VSI","VSG","VSUK","VSBE","VST","VSEE","VSR")
for (j in 1:10){

###define order (use initdf1)  
PLordered<-subset(initdf1[order(-initdf1$demand),],Div==division[j])[,1]
  
  
lasttable<-data.table(lasttable, key='Product.Line,DP.Family')
lasttable6<-subset(lasttable,Div == division[j])
lasttable6<-subset(lasttable6[,list("demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
)
,by=list(DP.Family,Product.Line)]
,Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
lasttable6<-lasttable6[order(-lasttable6$demand)]

lasttable6<-ddply(lasttable6,.(Product.Line),transform,DPbis=1)
lasttable6<-ddply(lasttable6,.(Product.Line),transform,DPbis=cumsum(DPbis))
lasttable6<-ddply(lasttable6,.(Product.Line),transform,DPbis=ifelse(DPbis<10,as.character(DP.Family),paste("other",Product.Line)))
lasttable6<-data.table(lasttable6, key='Product.Line,DP.Family')
initdf6<-lasttable6[,c(1,2,4),with=FALSE][subset(lasttable,Div == division[j])]
initdf6<-subset(initdf6[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                             +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                             +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(DPbis,Product.Line)],Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
setnames(initdf6,c("DPbis","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
initdf6<-data.frame(initdf6)
initdf6[mapply(is.infinite, initdf6)] <- 0
initdf6[mapply(is.na, initdf6)] <- 0

pdf(paste("./pdf files/",division[j]," ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()
div<-PLordered

#plot global graph
dfi<-initdf1[initdf1$Div==division[j],c(1,3:9)]
dfi<-melt(dfi,c("PL","demand"))
divs[[1]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=divname[j])

#plot PL graphes
k<-1
for (i in 1:length(PLordered)){
  dfi<-initdf6[initdf6$Product.Line==div[i],c(1,3:9)]
  if (nrow(dfi)!=0)
    {
    dfi<-melt(dfi,c("PL","demand"))
    k<-k+1
    divs[[k]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=div[i],sizePL=3)
    }
  }

if (k==14){
grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],divs[[14]],ncol=3)
}
if (k==13){
grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],ncol=3)
}
dev.off()
}

#### Per DP family / PL / CustGr filter one div ####

division<-c("D02","D02","D06","D06","D10","D10")
customergr<-c("OSCARO","NORMAL","BALKANS","NORMAL","MIDDLEAST","ISTANBUL")
for (j in 1:6){

  ###define order (use initdf1)  
  PLordered<-subset(initdf5[order(-initdf5$demand),],Div==division[j] & CustGr==customergr[j])[,1]
  
  lasttable<-data.table(lasttable, key='Product.Line,DP.Family')
  lasttable7<-subset(lasttable,Div == division[j] & CustGr == customergr[j])
  lasttable7<-subset(lasttable7[,list("demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                     +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                     +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
  )
  ,by=list(DP.Family,Product.Line)]
  ,Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
  lasttable7<-lasttable7[order(-lasttable7$demand)]
  
  lasttable7<-ddply(lasttable7,.(Product.Line),transform,DPbis=1)
  lasttable7<-ddply(lasttable7,.(Product.Line),transform,DPbis=cumsum(DPbis))
  lasttable7<-ddply(lasttable7,.(Product.Line),transform,DPbis=ifelse(DPbis<10,as.character(DP.Family),paste("other",Product.Line)))
  lasttable7<-data.table(lasttable7, key='Product.Line,DP.Family')
  initdf7<-lasttable7[,c(1,2,4),with=FALSE][subset(lasttable,Div == division[j] & CustGr == customergr[j])]
  initdf7<-subset(initdf7[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
  ),by=list(DPbis,Product.Line)],Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
  setnames(initdf7,c("DPbis","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
  initdf7<-data.frame(initdf7)
  initdf7[mapply(is.infinite, initdf7)] <- 0
  initdf7[mapply(is.na, initdf7)] <- 0
  
  pdf(paste("./pdf files/",division[j]," - ",customergr[j]," ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
  divs <- list()
  div<-PLordered
  
  
  #plot global graph
  dfi<-initdf5[initdf5$Div==division[j] & initdf5$CustGr==customergr[j],c(1,4:10)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[1]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=customergr[j])
  
  
  #plot PL graphes
  k<-1
  for (i in 1:length(PLordered)){
    dfi<-initdf7[initdf7$Product.Line==div[i],c(1,3:9)]
    if (nrow(dfi)!=0)
    {
      dfi<-melt(dfi,c("PL","demand"))
      k<-k+1
      divs[[k]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=div[i],sizePL=3)
    }
  }
  
  if (k==14){
    grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],divs[[14]],ncol=3)
  }
  if (k==13){
    grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],ncol=3)
  }
  dev.off()
}


#### Per Supplier / Div 

# Supplier0<-"490001"
# initdf8<-subset(lasttable[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
#                                 "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
#                                 "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
#                                 "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
#                                 "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
#                                 "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
#                                 "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
#                                   +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
#                                   +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
#   ),by=list(Div,Supplier)],Supplier==Supplier0)
# setnames(initdf8,c("Div","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
# initdf8<-data.frame(initdf8)
# initdf8[mapply(is.infinite, initdf8)] <- 0
# initdf8[mapply(is.na, initdf8)] <- 0
# 
# division<-c("D02","D03","D05","D06","D07","D08","D09","D10","D13","D20")
# divname<-c("VSF","VSAO","VSE","VSI","VSG","VSUK","VSBE","VST","VSEE","VSR")
# conversion<-data.frame(division=division,divname=divname)
# 
# dfi<-data.table(conversion,key='division')[data.table(initdf8,key='PL')][,c(-1,-3),with=F]
# setnames(dfi,"divname","PL")
# dfi<-melt(dfi,c("PL","demand"))
# polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=Supplier0)

#### per Supplier / Div ####

lasttable9<-data.table(lasttable_wo_vsao, key='Supplier,Div')

initdf9<-subset(lasttable9[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Div,Supplier)],Div %in% c("D02","D05","D06","D07","D08","D09","D10","D13","D20"))


initdf9tot<-subset(lasttable9[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Supplier)],Supplier %in% c("490001","913070","800741","917668","914943","191459","803676","906916","921453","800153","191674","191675","911641","490005","803782","916822","804178"))

initdf9tot<-cbind(initdf9tot,data.frame("Div"=rep("TOT",17)))

initdf9<-rbind(initdf9,initdf9tot,use.names=TRUE)

setnames(initdf9,c("Div","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))

##change the name of division

initdf9$PL <- as.character(initdf9$PL)
initdf9$PL[initdf9$PL == "D02"] <- "VSF"
#initdf9$PL[initdf9$PL == "D03"] <- "VSAO"
initdf9$PL[initdf9$PL == "D05"] <- "VSE"
initdf9$PL[initdf9$PL == "D06"] <- "VSI"
initdf9$PL[initdf9$PL == "D07"] <- "VSG"
initdf9$PL[initdf9$PL == "D08"] <- "VSUK"
initdf9$PL[initdf9$PL == "D09"] <- "VSBE"
initdf9$PL[initdf9$PL == "D10"] <- "VST"
initdf9$PL[initdf9$PL == "D13"] <- "VSEE"
initdf9$PL[initdf9$PL == "D20"] <- "VSR"


### Clean table
initdf9<-data.frame(initdf9)
initdf9[mapply(is.infinite, initdf9)] <- 0
### Delete all the lines with NA
initdf9 <- initdf9[complete.cases(initdf9),]
initdf9[mapply(is.na, initdf9)] <- 0

## Save the pdf file for Visibility
pdf(paste("./pdf files/Visibility of ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()

sup<-c("490001","913070","800741","917668","914943","191459","803676","906916")
supname<-c("Issoire","Skawina","Bietigheim","Batam","Timisoara","Martos","Winhere","CHRZANOW")

for (i in 1:8){
  dfi<-initdf9[initdf9$Supplier==sup[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE,ContainTotal=TRUE,division=supname[i])
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],ncol=3)
dev.off()

## Save the pdf file for Powertrain
pdf(paste("./pdf files/Powertrain of ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()

sup<-c("921453","800153","191674","191675","911641")
supname<-c("Fuenlabrada","Bursa","Amiens PC","Amiens VI","VPH")

for (i in 1:5){
  dfi<-initdf9[initdf9$Supplier==sup[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE,ContainTotal=TRUE,division=supname[i])
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],ncol=3)
dev.off()

## Save the pdf file for Thermal
pdf(paste("./pdf files/Thermal of ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()

sup<-c("490005","803782","916822","804178")
supname<-c("Saragosse","Saint Florine","Xin Yue","Dolz")

for (i in 1:4){
  dfi<-initdf9[initdf9$Supplier==sup[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE,ContainTotal=TRUE,division=supname[i])
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],ncol=3)
dev.off()

### Per PL / Supplier ###

lasttable10<-data.table(lasttable_wo_vsao, key="Supplier,Product.Line")

initdf10<-subset(lasttable10[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Supplier,Product.Line)], Supplier %in% vendorlist$Supplier)

initdf10 <- as.data.frame(initdf10)
initdf10$Product.Line <- as.character(initdf10$Product.Line)

##### re-select the data
tt <- initdf10[initdf10$Product.Line != 'NA',]
initdf10_new <- tt[tt$demand >= 100,]
initdf10 <- initdf10_new[complete.cases(initdf10_new),]


###### Change the name of product line (in order to put the 3 pls into a one plots)
initdf10$Product.Line <-  str_replace(initdf10$Product.Line,"ELE","PTS")
initdf10$Product.Line <-  str_replace(initdf10$Product.Line,"VSD","PTS")
initdf10$Product.Line <-  str_replace(initdf10$Product.Line,"VES","PTS")

vendorlist <- as.data.frame(vendorlist)
vendorlist$Supplier <- as.character(vendorlist$Supplier)
initdf10$Supplier <- as.character(initdf10$Supplier)

initdf10 <- data.table(initdf10, key = 'Supplier,Product.Line')
vendorlist <- data.table(vendorlist, key = 'Supplier,Product.Line')
aaa <- data.table(initdf10[vendorlist],key = 'Supplier,Product.Line')

initdf10 <- aaa

setnames(initdf10,c("SupplierName","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))



initdf10[mapply(is.infinite, initdf10)] <- 0
initdf10[mapply(is.na, initdf10)] <- 0
initdf10 <- initdf10[complete.cases(initdf10),]
initdf10 <- as.data.frame(initdf10)
initdf10$Supplier <- as.character(initdf10$Supplier)
initdf10 <- initdf10[c("PL","Product.Line",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6],"demand")]

pdf(paste("./pdf files/PL_Suppliers ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()

pls<-c("VTR","PTS","VWS","BRA","VLS","POP","VSS","VEC","IGN","VCC")

for (i in 1:10){
  dfi<-initdf10[initdf10$Product.Line==pls[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE,ContainTotal=TRUE,division=pls[i])
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],ncol=3)
dev.off()


### Top 10 Ref / PL

lasttable11<-data.table(lasttable_wo_vsao, key="Material,Product.Line")

PL_list <- c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS")

initdf11<-subset(lasttable11[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Material,Product.Line)], Product.Line %in% PL_list)


initdf11[mapply(is.infinite, initdf11)] <- 0
initdf11[mapply(is.na, initdf11)] <- 0

###Creat the table with Top 10 Ref (by Demand)

bbb <- initdf11[initdf11$Product.Line == "BRA",]

bbb <- bbb[order(bbb$demand, decreasing = TRUE),]

ccc <- head(bbb, n=10)

for(i in 2:13){

bbb <- initdf11[initdf11$Product.Line == PL_list[i],]

bbb <- bbb[order(bbb$demand, decreasing = TRUE),]

ccc1 <- head(bbb, n=10)

ccc <- rbind(ccc,ccc1,use.names=TRUE)

}

initdf11 <- ccc

setnames(initdf11,c("Material","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
initdf11 <- as.data.frame(initdf11)

pdf(paste("./pdf files/PL_Top_10_Ref ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()

for (i in 1:13){
  dfi<-initdf11[initdf11$Product.Line==PL_list[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE,division=PL_list[i])
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],ncol=3)
dev.off()

### Top 10 Ref / Div

lasttable12<-data.table(lasttable, key="Material,Div")

Div_list <- c("D02","D03","D05","D06","D07","D08","D09","D10","D13","D20")

initdf12<-subset(lasttable12[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Material,Div)], Div %in% Div_list)


initdf12[mapply(is.infinite, initdf12)] <- 0
initdf12[mapply(is.na, initdf12)] <- 0

###Creat the table with Top 10 Ref (by Demand)

bb <- initdf12[initdf12$Div == "D02",]

bb <- bb[order(bb$demand, decreasing = TRUE),]

cc <- head(bb, n=10)

for(i in 2:10){
  
  bb <- initdf12[initdf12$Div == Div_list[i],]
  
  bb <- bb[order(bb$demand, decreasing = TRUE),]
  
  cc1 <- head(bb, n=10)
  
  cc <- rbind(cc,cc1,use.names=TRUE)
  
}

initdf12 <- cc

setnames(initdf12,c("Material","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
initdf12 <- as.data.frame(initdf12)

##change the name of division

initdf12$Div <- as.character(initdf12$Div)
initdf12$Div <-  str_replace(initdf12$Div,"D02","VSF")
initdf12$Div <-  str_replace(initdf12$Div,"D03","VSAO")
initdf12$Div <-  str_replace(initdf12$Div,"D05","VSE")
initdf12$Div <-  str_replace(initdf12$Div,"D06","VSI")
initdf12$Div <-  str_replace(initdf12$Div,"D07","VSG")
initdf12$Div <-  str_replace(initdf12$Div,"D08","VSUK")
initdf12$Div <-  str_replace(initdf12$Div,"D09","VSBE")
initdf12$Div <-  str_replace(initdf12$Div,"D10","VST")
initdf12$Div <-  str_replace(initdf12$Div,"D13","VSEE")
initdf12$Div <-  str_replace(initdf12$Div,"D20","VSR")


pdf(paste("./pdf files/Div_Top_10_Ref ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()
dlist <- c("VSF","VSAO","VSE","VSI","VSG","VSUK","VSBE","VST","VSEE","VSR") 

for (i in 1:10){
  dfi<-initdf12[initdf12$Div==dlist[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE,division=dlist[i])
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],ncol=3)
dev.off()


#### Top 10 PN / PL / filter one div ####

division<-c("D02","D03","D05","D06","D07","D08","D09","D10","D13","D20")
divname<-c("VSF","VSAO","VSE","VSI","VSG","VSUK","VSBE","VST","VSEE","VSR")
for (j in 1:10){

###define order (use initdf1)  
PLordered<-subset(initdf1[order(-initdf1$demand),],Div==division[j])[,1]
  
  
lasttable<-data.table(lasttable, key='Product.Line,Material')
lasttable13<-subset(lasttable,Div == division[j])

#### Creat the plots for Top 10 worst ref for each div
initdf13bis<-subset(lasttable13[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                   "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Material,Product.Line)]
,Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))


initdf13bis[mapply(is.infinite, initdf13bis)] <- 0
initdf13bis[mapply(is.na, initdf13bis)] <- 0

temp <- subset(initdf13bis[,list("performence"=round(sum(abs(month_1)+abs(month_2)+abs(month_3)+abs(month_4)+abs(month_5)+abs(month_6)),digits = 2)),by=list(Material,Product.Line)])

temp <- data.table(temp,key = "Material,Product.Line")
initdf13bis <- data.table(initdf13bis,key = "Material,Product.Line")
initdf13bis <- data.table(initdf13bis[temp],key = "Material,Product.Line")
initdf13bis <- initdf13bis[order(-initdf13bis$performence)]

initdf13bis <- head(initdf13bis, n =10)

initdf13bis<-cbind(initdf13bis,data.frame("Div"=rep(division[j],10)))

setnames(initdf13bis,c("Material","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))

initdf13bis <- data.frame(initdf13bis)
initdf13bis <- initdf13bis[,c(1,3:9,11)]
#initdf13bis <- subset(initdf13bis,select = c("PL","Div",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6],"demand"))
#initdf13bis <- initdf13bis[c("PL","Div",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6],"demand")]
### End of Creat the plots for Top 10 worst ref for each div


lasttable13<-subset(lasttable13[,list("demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                  +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
)
,by=list(Material,Product.Line)]
,Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
lasttable13<-lasttable13[order(-lasttable13$demand)]

lasttable13<-ddply(lasttable13,.(Product.Line),transform,Refbis=1)
lasttable13<-ddply(lasttable13,.(Product.Line),transform,Refbis=cumsum(Refbis))
lasttable13<-ddply(lasttable13,.(Product.Line),transform,Refbis=ifelse(Refbis<11,as.character(Material),paste("other",Product.Line)))
lasttable13<-data.table(lasttable13, key='Product.Line,Material')
initdf13<-lasttable13[,c(1,2,4),with=FALSE][subset(lasttable,Div == division[j])]
initdf13<-subset(initdf13[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                              "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                             +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                             +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Refbis,Product.Line)],Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
setnames(initdf13,c("Refbis","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
initdf13<-data.frame(initdf13)
initdf13[mapply(is.infinite, initdf13)] <- 0
initdf13[mapply(is.na, initdf13)] <- 0

pdf(paste("./pdf files/",division[j],"_Top_10_PN_",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()
div<-PLordered

#plot global graph
dfi<-initdf13bis[initdf13bis$Div==division[j],c(1,2:8)]
dfi<-melt(dfi,c("PL","demand"))
divs[[1]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=divname[j])

#plot PL graphes
k<-1
for (i in 1:length(PLordered)){
  dfi<-initdf13[initdf13$Product.Line==div[i],c(1,3:9)]
  if (nrow(dfi)!=0)
    {
    dfi<-melt(dfi,c("PL","demand"))
    k<-k+1
    divs[[k]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=div[i],sizePL=3)
    }
  }

if (k==14){
grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],divs[[14]],ncol=3)
}
if (k==13){
grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],ncol=3)
}
dev.off()
}


#### Top 10 PN for Oscaro VSF


lasttable<-data.table(lasttable_wo_vsao, key='Product.Line,Material')
lasttable14<-subset(lasttable_wo_vsao,Div == "D02")
lasttable14<-subset(lasttable_wo_vsao,CustGr == "OSCARO")
oscaro <- lasttable14

PLordered<-subset(initdf1[order(-initdf1$demand),],Div=="D02")[,1]

#### Creat the plots for Top 10 worst ref for each div
initdf14bis<-subset(lasttable14[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                      "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                      "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                      "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                      "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                      "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                      "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                     +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                     +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Material,Product.Line)]
,Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))


initdf14bis[mapply(is.infinite, initdf14bis)] <- 0
initdf14bis[mapply(is.na, initdf14bis)] <- 0

temp <- subset(initdf14bis[,list("performence"=round(sum(abs(month_1)+abs(month_2)+abs(month_3)+abs(month_4)+abs(month_5)+abs(month_6)),digits = 2)),by=list(Material,Product.Line)])

temp <- data.table(temp,key = "Material,Product.Line")
initdf14bis <- data.table(initdf14bis,key = "Material,Product.Line")
initdf14bis <- data.table(initdf14bis[temp],key = "Material,Product.Line")
initdf14bis <- initdf14bis[order(-initdf14bis$performence)]

initdf14bis <- head(initdf14bis, n =10)

initdf14bis<-cbind(initdf14bis,data.frame("Div"=rep("D02",10)))

setnames(initdf14bis,c("Material","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))

initdf14bis <- data.frame(initdf14bis)
initdf14bis <- initdf14bis[,c(1,3:9,11)]
### End of Creat the plots for Top 10 worst ref for each div


lasttable14<-subset(lasttable14[,list("demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                     +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                     +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
)
,by=list(Material,Product.Line)]
,Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
lasttable14<-lasttable14[order(-lasttable14$demand)]

lasttable14<-ddply(lasttable14,.(Product.Line),transform,Refbis=1)
lasttable14<-ddply(lasttable14,.(Product.Line),transform,Refbis=cumsum(Refbis))
lasttable14<-ddply(lasttable14,.(Product.Line),transform,Refbis=ifelse(Refbis<11,as.character(Material),paste("other",Product.Line)))
lasttable14<-data.table(lasttable14, key='Product.Line,Material')
initdf14<-lasttable14[,c(1,2,4),with=FALSE][subset(lasttable,Div == "D02")]
initdf14<-subset(initdf14[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Refbis,Product.Line)],Product.Line %in% c("BRA","ELE","FLT","IGN","POP","VCC","VEC","VES","VSD","VLS","VSS","VTR","VWS"))
initdf14 <- initdf14[complete.cases(initdf14),]

setnames(initdf14,c("Refbis","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))
initdf14<-data.frame(initdf14)
initdf14[mapply(is.infinite, initdf14)] <- 0
initdf14[mapply(is.na, initdf14)] <- 0


pdf(paste("./pdf files/VSF_OSCARO_Top_10_PN_",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()

div<-PLordered

#plot global graph
dfi<-initdf14bis[initdf14bis$Div=="D02",c(1,2:8)]
dfi<-melt(dfi,c("PL","demand"))
divs[[1]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division="VSF_OSCARO")

#plot PL graphes
k<-1
for (i in 1:length(PLordered)){
  dfi<-initdf14[initdf14$Product.Line==div[i],c(1,3:9)]
  if (nrow(dfi)!=0)
  {
    dfi<-melt(dfi,c("PL","demand"))
    k<-k+1
    divs[[k]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE, division=div[i],sizePL=3)
  }
}

if (k==14){
  grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],divs[[14]],ncol=3)
}
if (k==13){
  grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],ncol=3)
}
dev.off()

#### Plots for seasonality suppliers ####

lasttable15<-data.table(lasttable_wo_vsao, key='Supplier,Div')

lasttable15bis <- lasttable15[lasttable15$Product.Line == c("VCC","VWS","ELE","FLT")]

initdf15<-subset(lasttable15bis[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                               +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Div,Supplier)],Div %in% c("D02","D05","D06","D07","D08","D09","D10","D13","D20"))


initdf15tot<-subset(lasttable15bis[,list("month_1"=round(sum(get(paste("DP",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_2"=round(sum(get(paste("DP",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_3"=round(sum(get(paste("DP",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_4"=round(sum(get(paste("DP",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_5"=round(sum(get(paste("DP",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "month_6"=round(sum(get(paste("DP",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)/(sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))-1,2),
                                 "demand"=round(sum(get(paste("HDN",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[1],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[2],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                +sum(get(paste("HDN",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[3],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[4],sep="_"))*PAP_EUR,na.rm=TRUE)
                                                +sum(get(paste("HDN",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[5],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDN",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE)+sum(get(paste("HDE",six_months[6],sep="_"))*PAP_EUR,na.rm=TRUE))
),by=list(Supplier)],Supplier %in% c("917903","907017","914792","800831","914425","803816","191172","800776","804131","917346","801591","921013","800821","490001","917668","800741","913070","490000","906828","907675","920961","918355","801951"))

initdf15tot<-cbind(initdf15tot,data.frame("Div"=rep("TOT",25)))

initdf15<-rbind(initdf15,initdf15tot,use.names=TRUE)

setnames(initdf15,c("Div","month_1",'month_2',"month_3",'month_4',"month_5",'month_6'),c("PL",six_months[1],six_months[2],six_months[3],six_months[4],six_months[5],six_months[6]))

##change the name of division

initdf15$PL <- as.character(initdf15$PL)
initdf15$PL[initdf15$PL == "D02"] <- "VSF"
#initdf15$PL[initdf15$PL == "D03"] <- "VSAO"
initdf15$PL[initdf15$PL == "D05"] <- "VSE"
initdf15$PL[initdf15$PL == "D06"] <- "VSI"
initdf15$PL[initdf15$PL == "D07"] <- "VSG"
initdf15$PL[initdf15$PL == "D08"] <- "VSUK"
initdf15$PL[initdf15$PL == "D09"] <- "VSBE"
initdf15$PL[initdf15$PL == "D10"] <- "VST"
initdf15$PL[initdf15$PL == "D13"] <- "VSEE"
initdf15$PL[initdf15$PL == "D20"] <- "VSR"


### Clean table
initdf15<-data.frame(initdf15)
initdf15 <- initdf15[complete.cases(initdf15),]
initdf15[mapply(is.infinite, initdf15)] <- 0
initdf15[mapply(is.na, initdf15)] <- 0
#initdf15 <- initdf15[-1,] ### delete the first row (because of no supplier)

## Save the pdf file for VCC
pdf(paste("./pdf files/Seasonality VCC of ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()

sup<-c("917903","907017","914792","800831","914425","803816","191172","800776","804131","917346","801591","921013","800821")
supname<-c("CZECHOWICE VCC","HUMPOLEC","ZHONGCHEN","SKAWINA","SUNAIR ","COOLKING","NOGENT","CLIMETAL","MANZAI","BRAINBEE","RAKOVNIK","ZARAGOZA VCC","REIMS")

for (i in 1:13){
  dfi<-initdf15[initdf15$Supplier==sup[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE,ContainTotal=TRUE,division=supname[i])
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],divs[[11]],divs[[12]],divs[[13]],ncol=3)
dev.off()

## Save the pdf file for other suplliers
pdf(paste("./pdf files/Seasonality other suppliers of ",six_months[6],".pdf",sep=""),width=11.69,heigh=16.54)
divs <- list()

sup<-c("490001","917668","800741","913070","490000","906828","907675","920961","918355","801951")
supname<-c("ISSOIRE","BATAM","BEITIGHEIM","SKAWINA","CHATELLERAUT","SCANTOP","MADRID","ALGO","HAN YALE","MANN & HUMMEL")

for (i in 1:10){
  dfi<-initdf15[initdf15$Supplier==sup[i],c(1,3:9)]
  dfi<-melt(dfi,c("PL","demand"))
  divs[[i]]<-polarHistogram(dfi, direction="outwards",PLpercent=TRUE,ContainTotal=TRUE,division=supname[i])
}

grid.arrange(divs[[1]],legend,divs[[2]],divs[[3]],divs[[4]],divs[[5]],divs[[6]],divs[[7]],divs[[8]],divs[[9]],divs[[10]],ncol=3)
dev.off()
