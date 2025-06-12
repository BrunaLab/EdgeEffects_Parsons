soil_moisture_analysis <- function() {
  

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

############################soil moisture ####

SM <- read_csv("./Data/SM_seg.csv")

rem <- c(-30,-25,-20,-15)
SM <- SM[!SM$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(SM)[names(SM) == "SD_SE_CI_V"] <-"SM_var"
names(SM)[names(SM) == "SD_SE_CI_V_n"] <- "SM_var_n"

length(unique(SM$article.id))

interiors <- SM %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- SM %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- SM %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(SM, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.humid","just.dist","just.diff","max.humid","max.dist", "max.diff")

sep$fullsm_diff <- ifelse(is.na(sep$just.diff), sep$fullsm_diff <- sep$just.humid - sep$max.humid, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)

#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$fullsm_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullsm_diff)

#for later comparison to tolerances
sepSM <- sep

#divide to make everything relative
sep$percentsm_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullsm_diff/sep$max.humid)*100))
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
groupedsm <- sep %>% group_by(article.id,just.dist) %>% mutate(avgsm.diff = round(mean(fullsm_diff),2),avg.smvar = round(mean(SM_var_n),2))
oneonlysm <- groupedsm[is.na(sep$segment_n)|sep$segment_n == "a",]

#TO ANALYZE, SHORT
short.sm <- sep[,c(1,2,4,6,8,9,10,11,13,14,19,21)]
short.sm$article.id <- as.factor(short.sm$article.id)

short.sm$idseg <- paste(short.sm[,1],short.sm[,2])

#no interior points shown
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])

#graphs
short<-short.sm
#relative to interior point
relative <- ggplot(subset(short,!is.na(percentsm_diff)), aes(x=just.dist, y=percentsm_diff))
relative + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))
#average of values based on article.id
avg.id <- ggplot(subset(oneonlysm,!is.na(percentsm_diff)), aes(x=just.dist,y = percentsm_diff))
avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) + 
  scale_x_continuous(breaks=pretty(short$just.dist,n=30)) + scale_y_continuous(breaks=pretty(short$percentsm_diff,n=10))


  return(short.sm)
}
