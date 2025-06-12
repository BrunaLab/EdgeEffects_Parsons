wind_speed_analysis <- function() {

# #Quantitative data only
# 
# library(dplyr) #Version 0.7.4
# library(tidyr) #Version 0.7.2
# library(ggplot2) #Version 2.2.1
# library(stringr) #Version 1.2.0
# library(gridExtra) #Version 2.3
# library(lme4) #Version 1.1-14
# library(MASS) #Version 7.3-47
# library(readr) #Version 1.1.1
# library(MuMIn)
# 
# 
# ##########################wind speed ####
# 
# WS <- read_csv("./Data/WS_seg.csv")
# 
# rem <- c(-30,-25,-20,-15)
# WS <- WS[!WS$dist %in% rem,] #all -30,-20,-15 distances unnecessary
# names(WS)[names(WS) == "SD_SE_CI_V"] <-"WS_var"
# names(WS)[names(WS) == "SD_SE_CI_V_n"] <- "WS_var_n"
# 
# length(unique(WS$article.id))
# 
# interiors <- WS %>% group_by(article.id) %>% summarize(dist = max(dist))
# top <- WS %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
# zero <- WS %>% group_by(article.id, segment_n) %>% slice(which(dist==0))
# sep <- merge(WS, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
# colnames(sep)[c(3,4,5,16,17,18)] <- c("just.ws","just.dist","just.diff","max.ws","max.dist", "max.diff")
# sep$fullws_diff <- ifelse(is.na(sep$just.diff), sep$fullws_diff <- sep$just.ws - sep$max.ws, NA)
# sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)
# 
# #combine columns of correct data into single
# sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
# sep$fullws_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullws_diff)
# 
# #for later comparison to tolerances
# sepWS <- sep
# 
# #divide to make everything relative
# sep$percentws_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullws_diff/sep$max.ws)*100))
# #remove article 56 because it can't be converted to percentages
# fivesix <- 56
# sep <- sep[!sep$article.id %in% fivesix,]
# 
# 
# #minimize to single transect per article
# sep$idseg <- paste(sep[,1],sep[,2])
# groupedws <- sep %>% group_by(article.id,just.dist) %>% mutate(avgws.diff = round(mean(fullws_diff),2),avg.wsvar = round(mean(WS_var_n),2))
# oneonlyws <- groupedws[is.na(sep$segment_n)|sep$segment_n == "a",]
# 
# #TO ANALYZE, SHORT
# short.ws <- sep[,c(1,2,4,6,8,9,10,11,13,14,19,21,22)]
# short.ws$article.id <- as.factor(short.ws$article.id)
# 
# short.ws$idseg <- paste(short.ws[,1],short.ws[,2])
# 
# #no interior points shown
# noint <- sep[!sep$just.dist == sep$max.dist,]
# noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
# noint$idseg <- paste(noint[,1],noint[,2])
# short<-short.ws
# #graphs
# #relative to interior point
# relative <- ggplot(subset(short,!is.na(percentws_diff)), aes(x=just.dist, y=percentws_diff))
# relative + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) +
#   scale_x_continuous(breaks=pretty(short$just.dist,n=30))
# #average of values based on article.id
# avg.id <- ggplot(subset(oneonlyws,!is.na(percentws_diff)), aes(x=just.dist,y = percentws_diff))
# avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) + 
#   scale_x_continuous(breaks=pretty(short$just.dist,n=30))


# tidyverse ---------------------------------------------------------------
# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readr)

# Read data
WS <- read_csv("./Data/WS_seg.csv")

# Remove unnecessary distances
rem <- c(-30, -25, -20, -15)
WS <- WS %>% filter(!dist %in% rem)

# Rename columns
WS <- WS %>%
  rename(WS_var = SD_SE_CI_V, 
         WS_var_n = SD_SE_CI_V_n)

# Number of unique article IDs
num_unique_articles <- WS %>% distinct(article.id) %>% nrow()

# Calculate interiors
interiors <- WS %>% group_by(article.id) %>% summarize(dist = max(dist), .groups = "drop")%>% ungroup()

# Find top and zero-distance entries
top <- WS %>% group_by(article.id, segment_n) %>% slice_max(dist, n = 1, with_ties = FALSE) 
top<-as.data.frame(top)
zero <- WS %>% group_by(article.id, segment_n) %>% filter(dist == 0)%>% ungroup()

# Merge top entries


top_trim<-top %>% dplyr::select(article.id,segment_n,wind_speed,dist,speed_diff)
sep <- left_join(WS,top_trim,by = c("article.id","segment_n")) %>% 
  rename("just.ws"="wind_speed.x",
         "just.dist"="dist.x",
         "just.diff"="speed_diff.x",
         "max.ws"="wind_speed.y",
         "max.dist"="dist.y",
         "max.diff"="speed_diff.y")

# 
# sep<-left_join(sep,WS,by = c("article.id", "segment_n"))
# names(sep)
# sep <- WS %>%
#   left_join(dplyr::select(top,article.id, segment_n, dist, WS_var, WS_var_n), by = c("article.id", "segment_n")) %>%
#   sep<-sep %>% 
#   rename(
#   
#   "just.ws"= "wind_speed" ,
#   "just.dist"="dist.x"       ,
#   "just.diff"="speed_diff" ,
#   "max.ws"="wind_speed" ,
#   "max.dist"="dist.y"
# )

names(sep)



# Calculate differences
sep <- sep %>%
  mutate(fullws_diff = if_else(is.na(just.diff), just.ws - max.ws, NA_real_),
         other.diff = if_else(notes == "comparison to edge", just.diff - max.diff, just.diff),
         fullws_diff = if_else(!is.na(other.diff), other.diff, fullws_diff),
         percentws_diff = if_else(!is.na(percent_diff), percent_diff, round((fullws_diff / max.ws) * 100)))

# Remove article 56
sep <- sep %>% filter(article.id != 56)

# Create a grouped dataframe with averages
groupedws <- sep %>%
  group_by(article.id, just.dist) %>%
  mutate(avgws.diff = round(mean(fullws_diff, na.rm = TRUE), 2),
         avg.wsvar = round(mean(WS_var_n, na.rm = TRUE), 2))

# Select only relevant rows
oneonlyws <- groupedws %>% filter(is.na(segment_n) | segment_n == "a")

# Prepare short dataframe
short.ws <- sep %>% 
  dplyr::select(article.id, segment_n, just.ws, WS_var, WS_var_n, just.dist, max.ws, max.dist, fullws_diff, percentws_diff) %>%
  mutate(article.id = as.factor(article.id),
         idseg = paste(article.id, segment_n))

# Filter out interior points
noint <- sep %>%
  filter(just.dist != max.dist) %>%
  dplyr::select(article.id, segment_n, just.ws, WS_var, WS_var_n, just.dist, max.ws, max.dist, fullws_diff)

# Graphs
ggplot(filter(short.ws, !is.na(percentws_diff)), aes(x = just.dist, y = percentws_diff)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  coord_cartesian(xlim = c(-10, 250)) +
  scale_x_continuous(breaks = pretty(short.ws$just.dist, n = 30)) +
  theme_minimal()

ggplot(filter(oneonlyws, !is.na(percentws_diff)), aes(x = just.dist, y = percentws_diff)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  coord_cartesian(xlim = c(-10, 250)) +
  scale_x_continuous(breaks = pretty(oneonlyws$just.dist, n = 30)) +
  theme_minimal()



  return(short.ws)
}

