library(tidyverse)
library(purrr)
library(fs)

# Edge effects and microclimate meta-analysis

# To catch all studies potentially including environmental data of edge effects 
# per se or from fragmentation, I conducted a search in March 2017 of 
# Web of Science with the below keyphrase:

# "forest fragment*" or forest "edge eff*") AND 
# ("soil (temperatur* or moisture)" or "light intensit*" or "(air or ambient) 
#  temperatur*" or humidity or "wind speed" or precip*)

# returning 205 results whose abstracts (and, if necessary, texts) were scanned 
# to determine whether the study actually fell into these parameters. 
# The cited and cited-by lists of all 205 were similarly scanned and collected 
# from; only studies which explicitly measured at least one environmental variable 
# (light, temperature, moisture, or air), rather than relying on weather station 
# data or historical average values, were retained. Papers which only took 
# measurements from plantations were discarded, as it is unclear that these 
# evenly-spaced farms provide the same dynamics as original or regrowth forest.

# After this filtering, 71 studies remained. To examine the scientific 
# methodology, results, and reach of each experiment, I gathered information 
# from each paper concerning the geographic location of the study, its topical 
# focus, the layout of sampling techniques, the environmental variables studied, 
# the equipment used to take environmental data, the temporal dimension of 
# sampling, kinds of statisical analyses done (if any), general results, and 
# how many times the paper had been cited on Web of Science. 3 studies did not 
# have official entries on Web of Science, and I substituted the number of 
# citations from Google Scholar.

# To observe potential trends in the depth and magnitude of edge effects across 
# studies, I extracted any data found correlating a distance from a forest edge 
# and an environmental variable in figures or text. Many papers did not present 
# the data in this fashion and required contact of the authors for possible 
# inclusion. Some studies used canopy openness as a substitute for measuring 
# light levels, but because this does not measure light levels per se, these 
# data were not included in analyses. Measurements of wind direction were ignored,
# as this is not a valuable measure when observing edge effects at a global scale.

# In total, 18 unique variables were found across studies, though some of these 
# were very similar (e.g. 'light incidence' and 'light intensity'). Light levels 
# alone accounted for 6 variables, while 7 were concerned with moisture/humidity,
# 4 with temperature, and 1 for air ('wind speed').

# To merge the 71 data files, use UNIX command:

# `cat ./Dropbox/MicroclimateEdgeEffects/CSV/*.csv > mergeddata.csv`
# [note: now in `data_raw` as 'CSV' folder]


# To separate each variable into a column, making each row an 
# observation based on Article.ID:

# `In [ ]: library(reshape2)`
# `begin <- dcast(mergedrefined4, Article.ID ~ variable, fun=toString, value.var="data")`

# 26 countries represented in data most studies from Brazil (25, 35.2%) followed 
# by the US (10, 14.1%), Australia (8, 11.3%), and Mexico (6, 8.5%)
# Only other countries with more than one study: New Zealand (3), Canada (2), 
# Colombia (2), Spain (2)

# To have appropriate replication to find trends in the numerical data, we 
# chose to spotlight variables which were recorded by >10 studies:

# air temperature (AT, n = 65)
# relative humidity (RH, n = 53)
# vapor pressure deficit (VPD, n = 21)
# soil temperature (ST, n = 19)
# soil moisture (SM, n = 18)
# photosynthetically active radiation (PAR, n = 11) wind speed (WS, n = 11)

  

# overview by EB ----------------------------------------------------------

# To assess the impacts of edge creation on forests globally. 
# All data extracted from peer-reviewed studies.

# mergedrefined7 = abiotic_df
# qualitative data on studies (location, scientific design, etc)
# now generated with 01_abiotic_studies_prep.R




# vardata 
# quantitative data from studies (air temperature, relative humidity, etc), expressed in percent change (percent--diff) from most internal forest point observed.
# generated with each subset. now making with vardata_binder.r 
# saved in moremeta.r

# vardata2 
# expresses the same data with factors instead of strings for matrix type, etc and with log1p on proportions


# withbroad 
# contains broad climate categories (temperate, tropical, boreal) to add to mergedrefined7

# allvariances
# quantitative data on all variance types, including SEs, CIs, and SDs reported
# comes from abiotic_analysis.r or more-meta(originally)
# These files are manipulated in:
  
# **metacode.r**
# for qualtitative data only (how/where/when/by whom were the data collected? which journals? etc.)
# saves merged_refined8

# **moremeta.r**
# for quantitative data only (which measurements were taken?)

# **meta_qualquan.r**
# mixture of qualitative and quantitative data (did the methods influence measurements?)

# Relevant visualizations are included with their data type/s.
# A writeup is available at [esparsons.com](http://www.esparsons.com/abiotic-edge-effects/). 
# Academic paper forthcoming.

library(tidyverse)


             
# countries
mergedrefined7 %>% group_by(country) %>% tally() %>% arrange(desc(n))

#
mergedrefined7 %>% group_by(biome1) %>% tally() %>% arrange(desc(n))
             

stacked_data<-mergedrefined7 %>% group_by(biome1) %>% tally()

# Stacked
ggplot(mergedrefined7, aes(fill=biome1, x=biome1)) + 
  geom_bar(position="stack", stat="identity")

str_detect(mergedrefined7$biome,"tropical")
str_detect(mergedrefined7$biome,"temperate")
names(mergedrefined7)
country<-mergedrefined7 %>% ungroup() %>% select(biome_1) %>% distinct() %>% arrange()
# 
# 
# begin <- dcast(mergedrefined7, Article.ID ~ variable, fun=toString, value.var="data")
# 
# cats <- read_csv("./Data/withbroad.csv") %>% 
#   mutate(variable=tolower(variable)) %>% 

# 
# 
# begin <- cbind(begin,cats$broad)

