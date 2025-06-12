vpd_analysis <- function() {
  

#Quantitative data only

library(dplyr) #Version 0.7.4
library(tidyr) #Version 0.7.2
library(ggplot2) #Version 2.2.1
library(stringr) #Version 1.2.0
library(gridExtra) #Version 2.3
library(lme4) #Version 1.1-14
library(MASS) #Version 7.3-47
library(readr) #Version 1.1.1
library(MuMIn)

##########################vapor pressure deficit ####

VPD <- read_csv("./Data/VPD_seg.csv")

rem <- c(-30,-25,-20,-15)
VPD <- VPD[!VPD$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(VPD)[names(VPD) == "SD_SE_CI_V"] <-"VPD_var"
names(VPD)[names(VPD) == "SD_SE_CI_V_n"] <- "VPD_var_n"

length(unique(VPD$article.id))

interiors <- VPD %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- VPD %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- VPD %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(VPD, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.vpd","just.dist","just.diff","max.vpd","max.dist", "max.diff")

sep$fullVPD_diff <- ifelse(is.na(sep$just.diff), sep$fullVPD_diff <- sep$just.vpd - sep$max.vpd, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)

#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$fullVPD_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullVPD_diff)

#for later comparison to tolerances
sepVPD <- sep

#divide to make everything relative
sep$percentVPD_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullVPD_diff/sep$max.vpd)*100))
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
groupedvpd <- sep %>% group_by(article.id,just.dist) %>% mutate(avgvpd.diff = round(mean(fullVPD_diff),2),avg.vpdvar = round(mean(VPD_var_n),2))
oneonlyvpd <- groupedvpd[is.na(sep$segment_n)|sep$segment_n == "a",]

#TO ANALYZE, SHORT
short.vpd <- sep[,c(1,2,4,6,8,9,10,11,13,14,19,21)]
short.vpd$article.id <- as.factor(short.vpd$article.id)

short.vpd$idseg <- paste(short.vpd[,1],short.vpd[,2])

#no interior points shown
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])

short<-short.vpd
#graphs
#relative to interior point
relative <- ggplot(subset(short,!is.na(percentVPD_diff)), aes(x=just.dist, y=percentVPD_diff))
relative + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))
#average of values based on article.id
avg.id <- ggplot(subset(oneonlyvpd,!is.na(percentVPD_diff)), aes(x=just.dist,y = percentVPD_diff))
avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) + 
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))


  return(short.vpd)
}
