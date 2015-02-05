# exploring the rail data extracted from season reports of Missouri Audubon's The Bluebird

library(ggplot2)
library(reshape)
library(gridExtra)
dat <- read.csv('The_Bluebird/the_bluebird.csv', colClasses=c("factor","numeric","numeric","numeric","factor","factor",'numeric',"numeric","numeric"))

dat <- dat[,c("species","month","location","county","day","year","number")]
mdat <- melt(dat, id=c("species","month","location","county","day","year"))

cdat <- cast(month + day + year ~ species, data=mdat, sum)

mdat <- melt(cdat, id=c("month","day","year"))

dates <- mdat[,c("month","day","year")]
varmonth<-cbind(c(1:12),c(0,31,59,90,120,151,181,212,243,273,304,334))
juldates<-data.frame(date=as.numeric())
for(i in 1:nrow(dates)) {juldates[i,1]<-varmonth[which(dates[i,1]==varmonth[,1]),2]+dates[i,2]                      
}

juldates

mdat$jdate <- juldates$date


sora <-   ggplot()+
            geom_bar(data=mdat[mdat$species=="sora",], aes(x=jdate, y=value, fill=species, colour=species),
            position=position_dodge(), 
            stat="identity")+
            ggtitle("sora")

kira <-   ggplot()+
  geom_bar(data=mdat[mdat$species=="kira",], aes(x=jdate, y=value, fill=species, colour=species),
           position=position_dodge(), 
           stat="identity")+
  ggtitle("kira")

yera <-   ggplot()+
  geom_bar(data=mdat[mdat$species=="yera",], aes(x=jdate, y=value, fill=species, colour=species),
           position=position_dodge(), 
           stat="identity")+
  ggtitle("yera")

vira <-   ggplot()+
  geom_bar(data=mdat[mdat$species=="vira",], aes(x=jdate, y=value, fill=species, colour=species),
           position=position_dodge(), 
           stat="identity")+
  ggtitle("vira")

blra <-   ggplot()+
  geom_bar(data=mdat[mdat$species=="blra",], aes(x=jdate, y=value, fill=species, colour=species),
           position=position_dodge(), 
           stat="identity")+
  ggtitle("blra")

grid.arrange(sora,vira,yera,kira,blra,ncol=1)
