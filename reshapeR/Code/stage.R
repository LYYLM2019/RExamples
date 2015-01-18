library(reshape2)

stage<-stage[,c(1:13)]
names(stage)<-c("day", month.name)

sites<-stage[seq(from=1, to=14400, by=36),]$day
sites.split<-colsplit(sites, "        ", names=c('dailymeans', 'year', 'empty', 'site'))
year.split<-colsplit(sites.split$year, " ", names=c('r', 'year'))
sites.rep<-sites.split[rep(seq_len(nrow(sites.split)), each=36),]
year.rep<-year.split[rep(seq_len(nrow(year.split)), each=36),]
stage<-cbind(sites.rep$site, year.rep$year, stage)
names(stage)[1:2]<-c('site', 'year')

(length(stage$day))/36 #287 sequences 
keep<-c(3:33)
for (i in 1:286){
     keep<-c(keep, 3:33+i*36)
}

stage.use<-stage[keep,]
stage.use$January<-as.numeric(stage.use$January)
stage.use$February<-as.numeric(stage.use$February)
stage.use$March<-as.numeric(stage.use$March)
stage.use$April<-as.numeric(stage.use$April)
stage.use$May<-as.numeric(stage.use$May)
stage.use$June<-as.numeric(stage.use$June)
stage.use$July<-as.numeric(stage.use$July)
stage.use$August<-as.numeric(stage.use$August)
stage.use$September<-as.numeric(stage.use$September)
stage.use$October<-as.numeric(stage.use$October)
stage.use$November<-as.numeric(stage.use$November)
stage.use$December<-as.numeric(stage.use$December)
summary(stage.use)

for (i in 4:15) {
  stage.use[,i]<-as.numeric(stage.use[,i])
}

stage.long<-reshape(stage.use, direction="long", varying=list(names(stage.use)[4:15]), v.name="value", idvar=c("site", "year", "day"), timevar="month", times=month.name)
stage.long$date<-paste(stage.long$day, stage.long$month, stage.long$year, sep="-")
stage.long$Date<-as.Date(stage.long$date, format="%d-%B-%Y")
#remove the NA Date (unexisting dates)
stage.long<-stage.long[is.na(stage.long$Date)==F,]
data.full<-stage.long[,c(1,5,7)]
stage.final<-reshape(data.full, direction="wide", timevar="site", idvar=c("Date"))

write.csv(stage.final, "reshape_stage_R.csv")
