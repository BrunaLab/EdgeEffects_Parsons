#Qualitative data only

library(reshape2) #Version 0.8.7
library(tidyverse)
# library(ggplot2) #Version 2.2.1
library(forcats) #Version 0.2.0
# library(dplyr) #Version 0.7.4
# library(stringr) #Version 1.2.0
# library(readr) #Version 1.1.1
library(gridExtra) #Version 2.3
library(here)
library(janitor)
theme_set(theme_classic())

mergedrefined7 <- read_csv(here("data", "mergedrefined7.csv")) 

begin <- dcast(mergedrefined7, Article.ID ~ variable, fun=toString, value.var="data")
names(begin)[names(begin) == "focal area of research (ecophysiology, population ecology, community ecology ecosystem ecology, animal behavior)"] <- "focal.area.of.research"

cats <- read_csv(here("data", "withbroad.csv"))
cats$biome %>% unique()
names(begin)[10] #something weird here from reading in special character

begin <- left_join(begin, cats %>% select(Article.ID, broad))
begin <-
  begin %>%
  as_tibble() %>% #just for better printing
  rename(accession.n = "accession number",
         article.id = "Article.ID",
         arch.y.n = "data archived? Y/N",
         archive = 10) %>% 
  na_if("NA") %>% 
  na_if("")

#data cleaning ####
begin$Municipality[63] <- "Manaus"
begin[begin=="SFRD"] <- "SRFD"
begin[begin=="AF"] <- "WS"
begin[begin=="TF"] <- "TP"
begin[begin=="GT"] <- NA
begin[begin=="IR"] <- "LIT"
begin[begin=="plant, plant"] <- "plant"
remove <- is.na(begin$focal.subject.of.research)
remove2 <- is.na(begin$layout)
begin$focal.subject.of.research[remove==TRUE] <- "plant"
begin$layout[remove2==TRUE] <- "fixed.point"
begin[begin=="transect"] <- "perpendicular"
begin$citations <- as.numeric(begin$citations)
begin$biome <- tolower(begin$biome)

# convert periods to spaces
begin <-
  begin %>% 
  mutate(across(c(
    starts_with("biome"),
    comparisons,
    ecosystem,
    starts_with("equip"),
    starts_with("field station"),
    starts_with("focal"),
    starts_with("habitat"),
    starts_with("municipality"),
    Country
    ), ~str_replace_all(.x, pattern = "\\.", " "))) %>% 
  mutate(across(c(
    starts_with("biome"),
    ecosystem
  ), str_to_lower))

#location data ####
length(unique(begin$Country))
sort(unique(begin$Country))
a <- ggplot(data = begin, aes(Country))
a + geom_bar()
length(begin[begin$Country=="Mexico", ])

popular_countries <- 
  begin %>% 
  count(Countries = Country, sort = TRUE)

#export for python
write_csv(popular_countries, here("outputs", "popular_countries.csv"))


b <- 
  ggplot(data = begin, aes(y = fct_rev(fct_infreq(Country)))) +
  geom_bar(aes(fill=Country), show.legend = FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(0,27)) +
  labs(x = "count", y = "")
b

popular.cities <- begin %>% count(Municipality, sort = TRUE)
popular.cities
#export mergedrefined7 to add simple latitude, first listed only
write_csv(begin, here("data", "mergedrefined8.csv"))
mergedrefined8 <- begin
# simplat <- mergedrefined8[,c(1,41)] #this is never used again


#variables data ####
#not entirely sure what the point is,  I guess to count how many unique combinations of variables there are?

g <- begin %>% select(starts_with("var"))
g <- g %>% mutate(varcombine = paste(var.1, var.2, var.3, var.4, var.5, var.6, var.7))
g %>%
  group_by(var.1, var.2, var.3, var.4, var.5, var.6, var.7) %>%
  count(sort = TRUE)

i <- g[str_detect(g$varcombine,"(?=.*AT)(?=.*RH)"),] #search by specific string type
i



#transect info ####
transect.types <- begin %>% count(layout, sort = TRUE)
transect.types

#habitat info ####
sorted.hab <- begin %>% count(biome, sort = TRUE)
biomes <- begin %>% count(broad, sort = TRUE, name = "Freq") %>%
  mutate(percent = round(Freq / 71 * 100)) %>%
  add_column(withdata = c(24, 14, 1)) %>%  #not sure where these numbers come from
  mutate(percent_with = round(withdata / 39 * 100))
biomes

forbarplot <-
  biomes[, c(1, 2, 4)] %>%
  tidyr::gather(key = "type", value = "number", Freq:withdata)

biopal <- c("#33ccff","#000099")

ggplot(forbarplot, x = Var1, y = number) +
  geom_col(aes(x = broad, y = number, fill = type), position = "identity") +
  scale_fill_manual(
    values = biopal,
    name = "Abiotic Edge\nEffects",
    labels = c("Total", "With data")
  ) +
  labs(x = "Biome type", y = "Number of studies")


cited.biomes <-
  ggplot(begin, aes(x = broad, y = citations, fill = broad)) + 
  geom_col(show.legend = FALSE) +
  labs(x = "Biome", y = "Citations")
cited.biomes

###citations ####
#by biome
mergedrefined8 %>% filter(citations>0) %>% nrow() #63
cite.biome <-
  mergedrefined8 %>% 
  group_by(broad) %>% 
  summarize(total_citations = sum(citations)) #all studies

mergedrefined8 %>% group_by(broad) %>% count()

#by country
cite.country <- 
  mergedrefined8 %>%
  group_by(Country) %>%
  summarize(Citations = sum(citations), No_Studies = n()) %>% 
  mutate(cite.per.study = (Citations / No_Studies)) %>% 
  arrange(desc(Citations))
cite.country

library(ggrepel)
library(gghighlight)
ggplot(cite.country,aes(x = No_Studies, y = cite.per.study, label = Country)) +
  geom_point() +
  scale_x_log10() +
  # geom_text_repel(aes(label = Country), color = "red", max.overlaps = 5) +
  gghighlight(No_Studies > 10 | cite.per.study > 50, 
              use_direct_label = TRUE,
              label_key = Country,
              unhighlighted_params = list(color = "black")) +
  labs(x = "Number of Studies",
       y = "Citations/Study")

portuspan <- c(7,11,12,53,58)
english <- mergedrefined8[!mergedrefined8$article.id %in% portuspan,]

english.cite <- english %>%
  group_by(Country) %>%
  summarize(total_citations = sum(citations), n = n()) %>%
  arrange(desc(n))
english.cite
#Without Portuguese, 864 citations for Brazil, 21 studies ** x=21,y=41


##years/time ####
hist(as.numeric(mergedrefined8$Year),breaks=10)
mergedrefined8$Year <- as.factor(mergedrefined8$Year) #not sure this is necessary
years <- mergedrefined8 %>% count(Year)

ggplot(mergedrefined8, aes(x = Year)) +
  geom_histogram(color = "black", fill = "thistle4", stat = "count") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5)) +
  ylim(0, 13) +
  labs(x = "Year", y = "Count", title = "Studies per year")

library(lubridate)
#not all dates are parsed correctly because of 1) some dates are in ymd, while most are mdy; 2) some dates don't exist (e.g. Feb 30), and 3) some are only years.
mergedrefined8 <- 
  mergedrefined8 %>% 
  mutate(across(contains("date"), mdy))

#TODO: Broken from here down.

# dates <- mergedrefined8[,c(1,17,18,19,71,72,73)]
dates <- mergedrefined8 %>%
  select(article.id, contains("date"), state) %>% 
  mutate(days = case_when(
    !is.na(start.date) ~ end.date - start.date,
    !is.na(start.date.1) ~ (end.date.1 - start.date.2) + (end.date.2 - start.date.2),
    TRUE ~ as.Date(NA) - as.Date(NA)
  ))

dates$days[13] <- dates$days[13]*-1 #fix entry issue where end.date and start.date were swapped

ggplot(dates, aes(x = days)) +
  geom_histogram(binwidth = 60)

dates <- dates %>% 
  mutate(end_year = as.integer(if_else(!is.na(end.date), year(end.date), year(end.date.2))))

hist(dates$end_year)

ggplot(dates, aes(x = days)) +
  geom_histogram(binwidth = 30, #bins = ~1 month
                 color = "black",
                 fill = "thistle4") +
  theme_bw() +
  geom_vline(
    aes(xintercept = mean(days, na.rm = TRUE)),
    color = "blue",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(
    aes(xintercept = median(days, na.rm = TRUE)),
    color = "orange",
    linetype = "solid",
    size = 1
  ) +
  labs(x = "# days of measurements", y = "Frequency", title = "Histogram of sampling duration")


##archives? ####
count(mergedrefined8[mergedrefined8$arch.y.n == "Y",1]) #2 of 71

##journal ####
journals <- mergedrefined8 %>% count(Journal, sort = TRUE)#most common = FEM 6, BC 5
##area, subject of research ####
areas <- count(mergedrefined8, focal.area.of.research, sort = TRUE)
subjects <- count(mergedrefined8, focal.subject.of.research, sort = TRUE)


##replicates ####
mergedrefined8 <-
  mergedrefined8 %>% 
  mutate(across(starts_with("replicates."), as.numeric))
hist(mergedrefined8$replicates.habitat.1,breaks=10)
hist(mergedrefined8$replicates.habitat.2)
sort(table(mergedrefined8$replicates.habitat.1),decreasing=T) # 22 of 71 had only one replicate of first habitat
sort(table(mergedrefined8$replicates.habitat.2),decreasing=T)
sort(table(mergedrefined8$replicates.habitat.3),decreasing=T)
sort(table(mergedrefined8$replicates.habitat.4),decreasing=T)

replicates <-
  mergedrefined8 %>% 
    select(article.id, starts_with("replicates.habitat."), reps.per.dist.1)
replicates <-
  replicates %>% 
  mutate(across(c(article.id, starts_with("replicates.habitat.")), as.factor))

reps <- replicates %>% count(replicates.habitat.1)

#instruments ####
count(mergedrefined8, equip.var.1, sort = TRUE)
count(mergedrefined8, equip.var.2, sort = TRUE)
count(mergedrefined8, equip.var.3, sort = TRUE)
count(mergedrefined8, equip.var.4, sort = TRUE)
count(mergedrefined8, equip.var.5, sort = TRUE)
count(mergedrefined8, equip.var.6, sort = TRUE)
count(mergedrefined8, equip.var.7, sort = TRUE)

#how were edges and interiors determined? ####
cited_info <- read_csv(here("data", "cited_info2.csv"))

sort(table(cited_info$cited.dist), decreasing=T)
length(unique(cited_info$article.id)) #21
studies <- unique(cited_info$article.id)

cited_interiors <- cited_info %>% filter(dist.type == "interior")

cited_intdist <- subset(cited_interiors, subset=!duplicated(cited_interiors[c("article.id")]),
                        select=c("article.id","cited.dist","dist.type"))
cited_distances <- as.data.frame(table(sort(unlist(cited_intdist$cited.dist))))


cited_papers <- count(cited_interiors, cited.author, sort = TRUE)
