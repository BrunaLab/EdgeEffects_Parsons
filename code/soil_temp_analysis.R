soil_temp_analysis <- function() {
  
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



###########################soil temperature ####

ST <- read_csv("./Data/ST_seg.csv")

rem <- c(-30,-25,-20,-15)
ST <- ST[!ST$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(ST)[names(ST) == "SD_SE_CI_V"] <-"ST_var"
names(ST)[names(ST) == "SD_CI_V_n"] <- "ST_var_n"

length(unique(ST$article.id))

interiors <- ST %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- ST %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- ST %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(ST, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.temp","just.dist","just.diff","max.temp","max.dist", "max.diff")

sep$fullst_diff <- ifelse(is.na(sep$just.diff), sep$fullst_diff <- sep$just.temp - sep$max.temp, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)

#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$fullst_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullst_diff)

#for later comparison to tolerances
sepST <- sep

#divide to make everything relative
sep$percentst_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullst_diff/sep$max.temp)*100))
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
groupedst <- sep %>% group_by(article.id,just.dist) %>% mutate(avgst.diff = round(mean(fullst_diff),2),avg.stvar = round(mean(ST_var_n),2))
oneonlyst <- groupedst[is.na(sep$segment_n)|sep$segment_n == "a",]

#TO ANALYZE, SHORT
short.st <- sep[,c(1,2,4,6,8,9,10,11,13,14,19,21)]
short.st$article.id <- as.factor(short.st$article.id)

short.st$idseg <- paste(short.st[,1],short.st[,2])

#no interior points
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])

#graphs
#relative to interior point
relative <- ggplot(subset(short.st,!is.na(percentst_diff)), aes(x=just.dist, y=percentst_diff))
relative + geom_point() + geom_smooth(method="loess",formula=y~x) + coord_cartesian(ylim=c(-20,90),xlim=c(-10,250)) +
  scale_x_continuous(breaks=pretty(short.st$just.dist,n=30))
#average of values based on article.id
avg.id <- ggplot(subset(oneonlyst,!is.na(percentst_diff)), aes(x=just.dist,y = percentst_diff))
avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(ylim=c(-20,90),xlim=c(-10,250)) + 
  scale_x_continuous(breaks=pretty(short.st$just.dist,n=30)) + scale_y_continuous(breaks=pretty(short.st$percentst_diff,n=10))


  return(short.st)
}
