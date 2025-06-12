library(tidyverse)
library(countrycode)

cats <- read_csv("./Data/withbroad.csv") %>% 
  rename('accession.n'='accession number',
         'archive'='data archive<U+00A0>') %>% 
  select(-`data archived? Y/N`) %>% 
  mutate(across(everything(), as.character)) 

names(cats)<-tolower(names(cats))
names(cats)<-gsub(" ", ".",names(cats))
names(cats)<-gsub("?", "",names(cats))
cats$country<-countrycode(cats$country, 
                          origin = 'country.name', 
                          destination = 'iso3c',
                          custom_match = c(`Cote.D'ivore` = 'CIV'))

cats <-cats %>% 
  mutate(municipality=tolower(municipality)) %>% 
  mutate(citations=as.numeric(citations)) %>% 
  mutate(comparisons=gsub("SFRD","SRFD",comparisons)) %>% 
  mutate(comparisons=gsub("transect","perpendicular",comparisons))
  

# begin[begin=="AF"] <- "WS"
# begin[begin=="TF"] <- "TP"
# begin[begin=="GT"] <- NA
# begin[begin=="IR"] <- "LIT"
# begin[begin=="plant, plant"] <- "plant"
# remove <- is.na(begin$focal.subject.of.research)
# remove2 <- is.na(begin$layout)
# begin$focal.subject.of.research[remove==TRUE] <- "plant"
# begin$layout[remove2==TRUE] <- "fixed.point"
# todo cats$Country
%>% 
  
  
  
  pivot_longer(!Article.ID, names_to = "variable", values_to = "value") %>% 
  drop_na(value)
  
