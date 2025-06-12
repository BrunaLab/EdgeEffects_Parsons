rh_analysis <- function() {
  #Quantitative data only
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
# # AT <- read_csv("./Data/AT_seg.csv")
# 
# #######################relative humidity ####
# 
# RH <- read_csv("./Data/RH_seg.csv")
# 
# rem <- c(-30,-25,-20,-15)
# RH <- RH[!RH$dist %in% rem,] #all -30,-20,-15 distances unnecessary
# # names(RH)[names(RH) == "SD_SE_CI_V"] <-"RH_var"
# # names(RH)[names(RH) == "SD_SE_CI_V_n"] <- "RH_var_n"
# 
# # Rename columns
# RH <- RH %>%
#   rename(RH_var = SD_SE_CI_V,
#          RH_var_n = SD_SE_CI_V_n)
# 
# # length(unique(RH$article.id))
# 
# 
# # Count unique article IDs
# num_unique_articles <- RH %>% distinct(article.id) %>% nrow()
# 
# # Compute distance frequencies
# distances <- RH %>%
#   count(dist)
# 
# 
# #distances
# # distances2 <- as.data.frame(table(RH$dist))
# round(mean(interiors$dist))
# #find interior point
# # interiors <- RH %>% group_by(article.id) %>% summarize(dist = max(dist))
# # top <- RH %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
# # zero <- RH %>% group_by(article.id, segment_n) %>% slice(which(dist==0))
# 
# 
# 
# # Find interior points
# interiors <- RH %>%
#   group_by(article.id) %>%
#   summarize(dist = max(dist), .groups = "drop")
# 
# # Get top and zero-distance entries
# top <- RH %>%
#   group_by(article.id, segment_n) %>%
#   slice_max(dist, n = 1, with_ties = FALSE)
# 
# zero <- RH %>%
#   group_by(article.id, segment_n) %>%
#   filter(dist == 0)
# 
# 
# 
# 
# sep <- merge(RH, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
# colnames(sep)[c(3,4,5,16,17,18)] <- c("just.humid","just.dist","just.diff","max.humid","max.dist", "max.diff")
# 
# sep$fullrh_diff <- ifelse(is.na(sep$just.diff), sep$fullrh_diff <- sep$just.humid - sep$max.humid, NA)
# sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)
# 
# #combine columns of correct data into single
# sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
# sep$fullrh_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullrh_diff)
# 
# #for later comparison to tolerances
# sepRH <- sep
# 
# #divide to make everything relative
# sep$percentrh_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullrh_diff/sep$max.humid)*100))
# #minimize to single transect per article
# sep$idseg <- paste(sep[,1],sep[,2])
# groupedrh <- sep %>% group_by(article.id,just.dist) %>% mutate(avgrh.diff = round(mean(fullrh_diff),2),avg.rhvar = round(mean(RH_var_n),2))
# oneonlyrh <- groupedrh[is.na(sep$segment_n)|sep$segment_n == "a",]
# 
# #TO ANALYZE, SHORT
# short.rh <- sep[,c(1,2,4,6,8,9,10,11,13,14,19,21)]
# short.rh$article.id <- as.factor(short.rh$article.id)
# 
# short.rh$idseg <- paste(short.rh[,1],short.rh[,2])
# 
# #no interior points
# noint <- sep[!sep$just.dist == sep$max.dist,]
# noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
# noint$idseg <- paste(noint[,1],noint[,2])
# 
# 
# 
# #graphs
# short<-short.rh
# 
# #relative to interior point
# relative <- ggplot(subset(short,!is.na(percentrh_diff)), aes(x=just.dist, y=percentrh_diff))
# relative + geom_point() + geom_smooth(method="loess",formula=y~x) + coord_cartesian(ylim=c(-30,5),xlim=c(-10,250)) +
#   scale_x_continuous(breaks=pretty(short$just.dist,n=30))
# #average of values based on article.id
# avg.id <- ggplot(subset(oneonlyrh,!is.na(percentrh_diff)), aes(x=just.dist,y = percentrh_diff))
# avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(ylim=c(-30,5),xlim=c(-10,250)) +
#   scale_x_continuous(breaks=pretty(short$just.dist,n=30)) + scale_y_continuous(breaks=pretty(short$percentrh_diff,n=10))
# 



# tidyverse version -------------------------------------------------------
#######################relative humidity ####

library(tidyverse)

RH <- read_csv("./Data/RH_seg.csv")


# Remove unnecessary distances
rem <- c(-30, -25, -20, -15)
RH <- RH %>% filter(!dist %in% rem)

# Rename columns
RH <- RH %>%
  rename(RH_var = SD_SE_CI_V, RH_var_n = SD_SE_CI_V_n)

# Count unique article IDs
num_unique_articles <- RH %>% distinct(article.id) %>% nrow()

# Compute distance frequencies
distances <- RH %>%
  count(dist)

# Find interior points
interiors <- RH %>%
  group_by(article.id) %>%
  summarize(dist = max(dist), .groups = "drop")

# Get top and zero-distance entries
top <- RH %>%
  group_by(article.id, segment_n) %>%
  slice_max(dist, n = 1, with_ties = FALSE)

zero <- RH %>%
  group_by(article.id, segment_n) %>%
  filter(dist == 0)



sep <- merge(RH, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.humid","just.dist","just.diff","max.humid","max.dist", "max.diff")


# Merge with top entries
top_trim<-top %>% dplyr::select(article.id, segment_n, rel_humid, dist, humid_diff)
sep <- left_join(RH,top_trim, by = c("article.id", "segment_n")) 
  sep<-sep %>% rename(
    "just.humid" = "rel_humid.x", 
    "just.dist" = 'dist.x', 
    "just.diff" = "humid_diff.x",
    "max.humid"= "rel_humid.y",
    'max.dist'="dist.y",
    "max.diff"="humid_diff.y")

# left_join(select(top, article.id, segment_n, dist, RH_var, RH_var_n), by = c("article.id", "segment_n")) %>%
#   rename(just.humid = RH_var, just.dist = dist, just.diff = RH_var_n)
# 


sep$fullrh_diff <- ifelse(is.na(sep$just.diff), sep$fullrh_diff <- sep$just.humid - sep$max.humid, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)



# Compute differences
sep <- sep %>%
  mutate(
    fullrh_diff = if_else(is.na(just.diff), just.humid - max.humid, NA_real_),
    other.diff = if_else(notes == "comparison to edge", just.diff - max.diff, just.diff),
    fullrh_diff = if_else(!is.na(other.diff), other.diff, fullrh_diff),
    percentrh_diff = if_else(!is.na(percent_diff), percent_diff, round((fullrh_diff / max.humid) * 100))
  )

# Grouped computation
groupedrh <- sep %>%
  group_by(article.id, just.dist) %>%
  mutate(
    avgrh.diff = round(mean(fullrh_diff, na.rm = TRUE), 2),
    avg.rhvar = round(mean(RH_var_n, na.rm = TRUE), 2)
  )

# Filter for specific segments
oneonlyrh <- groupedrh %>%
  filter(is.na(segment_n) | segment_n == "a")

# Prepare short data
short.rh <- sep %>%
  # dplyr::select(article.id, segment_n, just.humid, RH_var, RH_var_n, just.dist, max.humid, max.dist, fullrh_diff, percentrh_diff) %>%
  mutate(
    article.id = as.factor(article.id),
    idseg = paste(article.id, segment_n)
  )

# Exclude interior points
noint <- sep %>%
  filter(just.dist != max.dist) %>%
  dplyr::select(article.id, segment_n, just.humid, RH_var, RH_var_n, just.dist, max.humid, max.dist, fullrh_diff) %>%
  mutate(idseg = paste(article.id, segment_n))





# Ensure short contains relevant data
short <- short.rh

# Relative to interior point
relative <- short %>%
  filter(!is.na(percentrh_diff)) %>%
  ggplot(aes(x = just.dist, y = percentrh_diff)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  coord_cartesian(ylim = c(-30, 5), xlim = c(-10, 250)) +
  scale_x_continuous(breaks = pretty(short$just.dist, n = 30)) +
  theme_minimal()

# Average values based on article.id
avg.id <- oneonlyrh %>%
  filter(!is.na(percentrh_diff)) %>%
  ggplot(aes(x = just.dist, y = percentrh_diff)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) +
  coord_cartesian(ylim = c(-30, 5), xlim = c(-10, 250)) +
  scale_x_continuous(breaks = pretty(short$just.dist, n = 30)) +
  scale_y_continuous(breaks = pretty(short$percentrh_diff, n = 10)) +
  theme_minimal()

# Print the plots
print(relative)
print(avg.id)

return(short.rh)
}