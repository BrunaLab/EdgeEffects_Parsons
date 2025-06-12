par_analysis <- function() {

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


###########################photosynthetically active radiation ####

PAR <- read_csv("./Data/PAR_seg.csv")

rem <- c(-30,-25,-20,-15)
PAR <- PAR[!PAR$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(PAR)[names(PAR) == "SD_SE_CI_V"] <-"PAR_var"
names(PAR)[names(PAR) == "SD_SE_CI_V_n"] <- "PAR_var_n"

length(unique(PAR$article.id))

interiors <- PAR %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- PAR %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- PAR %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(PAR, top[,c(1,2,3,4,5,6)], by = c("article.id","segment_n"))
colnames(sep)[c(3,5,6,17,18,19,20)] <- c("just.PAR","just.dist","just.diff","max.PAR","log.max.PAR","max.dist", "max.diff")

sep$notes <- as.character(sep$notes)
sep$notes <- str_split_fixed(sep$notes,"\\.",n=2)


sep$fullPAR_diff <- ifelse(is.na(sep$just.diff), sep$fullPAR_diff <- sep$just.PAR - sep$max.PAR, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)


#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$fullPAR_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullPAR_diff)
sep$fullPAR_diff <- ifelse(!is.na(sep$fullPAR_diff), sep$fullPAR_diff,sep$log_value.x)
sep$fullPAR_diff <- ifelse(!is.na(sep$fullPAR_diff), sep$fullPAR_diff,sep$percent_diff)

#for later comparison to tolerances
sepPAR <- sep

#divide to make everything relative
sep$percentPAR_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullPAR_diff/sep$max.PAR)*100))
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
groupedPAR <- sep %>% group_by(article.id,just.dist) %>% mutate(avgPAR.diff = round(mean(fullPAR_diff),2),avg.PARvar = round(mean(PAR_var_n),2))


#TO ANALYZE, SHORTS
short.par <- sep[,c(1,2,5,6,9,10,11,12,13,14,15,23)]
short.par$article.id <- as.factor(short.par$article.id)

short.par$idseg <- paste(short.par[,1],short.par[,2])

#no interior points shown
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])

#graphs
short<-short.par
#relative to interior point
relative <- ggplot(subset(short,!is.na(percentPAR_diff)), aes(x=just.dist, y=percentPAR_diff))
relative + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))

return(list(short,short.par))
}