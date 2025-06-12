air_temp_analysis <- function() {
  


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

AT <- read_csv("./Data/AT_seg.csv")

###########air temperature ####
rem <- c(-30,-25,-20,-15)
AT <- AT[!AT$dist %in% rem,] #all -30,-20,-15 distances unnecessary

length(unique(AT$article.id))

interiors <- AT %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- AT %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- AT %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

#distances
distances <- as.data.frame(table(AT$dist))
mean(interiors$dist)
#find interior point
interiors <- AT %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- AT %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- AT %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(AT, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.temp","just.dist","just.diff","max.airtemp","max.dist", "max.diff")


#find temp differences
#so <- sep$article.id == 71
#sep <- sep[!so,]

sep$full_diff <- ifelse(is.na(sep$just.diff), sep$full_diff <- sep$just.temp - sep$max.airtemp, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)


#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$full_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$full_diff)
#fix id 55
sep$full_diff[sep$article.id == 55] <- sep$just.diff[sep$article.id == 55]
#for later comparison to variances
sepAT <- sep
#divide to make everything relative
sep$percent_diff <- round((sep$full_diff/sep$max.airtemp)*100)
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
grouped <- sep %>% group_by(article.id,just.dist) %>% mutate(avg.diff = round(mean(full_diff),2),avg.var = round(mean(SD_SE_CI_V_n),2))
oneonly <- grouped[is.na(sep$segment_n)|sep$segment_n == "a",]
  


#TO ANALYZE, SHORT
short <- sep[,c(1,2,4,6,7,8,9,10,11,13,14,19)]
short$article.id <- as.factor(short$article.id)

short$idseg <- paste(short[,1],short[,2])

#no interior points shown
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,7,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])





#matrices
matrices <- as.data.frame(sort(table(AT$matrix_type),decreasing = T))
length(unique(AT$article.id[AT$matrix_type=="pasture"])) #number of studies using matrix type
bit <- AT %>% group_by(article.id, matrix_type) %>% summarize()
bitn <- as.data.frame(sort(table(bit$matrix_type), decreasing = T)) #number of studies over all matrix types

#data to spread
clip <- AT[-c(423,495,155),-c(6,7,12,13,14,15)]
wide <- clip %>% spread(dist,air_temp,fill=NA,convert=FALSE)

#simple graphs
#simple AT
ggplot(AT, aes(x=dist, y=air_temp)) + geom_point()
ggplot(short, aes(x=just.dist, y=full_diff)) + geom_point(aes(color=article.id)) + 
  geom_smooth(method = "auto") + scale_x_continuous(breaks=pretty(short$just.dist,n=40)) + scale_y_continuous(breaks=pretty(short$full_diff,n=10)) + 
  coord_cartesian(ylim=c(-5,10),xlim=c(-10,200)) + geom_hline(yintercept = 0)
#by each plot per article
plot <- ggplot(short, aes(x=just.dist, y=full_diff,group=idseg)) + scale_x_continuous(breaks=pretty(short$just.dist,n=40)) + 
  scale_y_continuous(breaks=pretty(short$full_diff,n=10)) + coord_cartesian(ylim=c(-5,10),xlim=c(-10,200))
plot + geom_line(color = "blue",alpha=.2)
#relative to interior point
relative <- ggplot(subset(short,!is.na(percent_diff)), aes(x=just.dist, y=percent_diff))
relative + geom_point() + geom_smooth(method="loess",formula=y~x) + coord_cartesian(ylim=c(-10,100),xlim=c(-10,200)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))
#without interior points
no_interior <- ggplot(subset(noint,!is.na(percent_diff)), aes(x=just.dist, y=percent_diff))
no_interior + geom_point() + geom_smooth(method="loess",formula=y~x) + coord_cartesian(ylim=c(-10,90),xlim=c(-10,140)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30)) + scale_y_continuous(breaks=pretty(short$percent_diff,n=10)) + xlab("Distance") +
  ylab("% difference from interior") + geom_point(data=data.frame(x=104,y=0),aes(x,y),color="red",size=4)
#average of values based on article.id
avg.id <- ggplot(subset(oneonly,!is.na(percent_diff)), aes(x=just.dist,y = percent_diff))
avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(ylim=c(-10,100),xlim=c(-10,200)) + 
  scale_x_continuous(breaks=pretty(short$just.dist,n=30)) + scale_y_continuous(breaks=pretty(short$percent_diff,n=10))
 

  return(short)
}
