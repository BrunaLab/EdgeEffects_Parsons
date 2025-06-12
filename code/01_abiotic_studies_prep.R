
# overview ----------------------------------------------------------------

# this binds ogether the files extracting infomration from papers measuring 
# abiotic edge effects. Gathered by S. Parsons for MALAS thesis. 


# load libraries ----------------------------------------------------------

library(tidyverse)


# bind the csv files   ----------------------------------------------------



data_dir<-"./data_raw/CSV"

csv_files_all <- fs::dir_ls(data_dir, regexp = "\\.csv$")

abiotic_df_raw<-purrr::map_dfr(csv_files_all, read_csv)


# standardize and clean up (mergedrefine7) --------------------------------


abiotic_df<-abiotic_df_raw %>% 
  select(article_id=Article.ID,
         variable=`data column`,
         data,
         notes) %>% 
  mutate(variable=tolower(variable)) %>% 
  mutate(data=tolower(data)) %>% 
  mutate(variable=gsub(" ","_",variable)) %>% 
  mutate(variable=gsub("[.]","_",variable)) %>% 
  mutate(variable=gsub("equp","equip",variable)) %>% 
  mutate(variable=gsub("number","no",variable)) %>% 
  mutate(data=gsub("http://","",data)) %>% 
  mutate(variable=
           case_when(
             str_detect(variable,"focal_area_of_research")~"research_focus",
             str_detect(variable,"focal_subject_of_research")~"research_subject",
             variable=="summary_of_results"~"results",
             str_detect(variable,"statistical_analysis")~"stat_analysis",
             str_detect(variable,"statistical_methods")~"stat_methods",
             str_detect(variable,"statistica_methods")~"stat_methods",
             variable=="data_archive "~"data_archive",
             variable=="data_archived?_y/n"~"data_archived_TF",
             str_detect(variable,"was_cited_before")~"cited_prior",
             str_detect(variable,"significant_differences_observed")~"sig_diff_obs",
             TRUE ~ as.character(variable)
           )
  ) %>% 
  mutate(data=
           case_when(
             data=="variable"~"varies",
             TRUE ~ as.character(data)
           )) %>% 
  arrange(article_id,variable,data) %>% 
  drop_na(article_id, c(variable, data)) %>% 
  group_by(article_id,variable) %>% 
  mutate(variable=if_else(variable=="results",paste("results",row_number(),sep="_"),variable)) %>% 
  mutate(variable=if_else(variable=="comparisons",paste("comparisons",row_number(),sep="_"),variable)) %>% 
  mutate(variable=if_else(variable=="stat_methods",paste("stat_methods",row_number(),sep="_"),variable)) %>% 
  mutate(variable=if_else(variable=="field_station",paste("field_station",row_number(),sep="_"),variable)) %>% 
  distinct() %>% 
  mutate(data=(trimws(data))) %>% 
  distinct() 





# THIS GETS YOU TO "MERGEDREFINED7
# almost - one line difference?
# foo1<-abiotic_df %>% group_by(article_id) %>% tally() %>% rename(n_eb=n)
# foo2<-mergedrefined7 %>%  group_by(article_id=Article.ID) %>% tally()
# foo3<-full_join(foo1,foo2) %>% mutate(n_eb==n)
# citations = 0 excluded for reference 51.

# convert to wide (mergedrefined8) -----------------------------------------


library(dplyr)
library(tidyr)



  
  abiotic_df_wide<-abiotic_df %>% 
    filter(variable!="accession_no") %>% 
    select(-notes) %>% 
  pivot_wider(names_from = variable, values_from=data) %>% 
    relocate(field_station_2,.after="field_station_1") %>% 
    unite("field_station", 
          matches("field_station"),
          na.rm = TRUE, remove = TRUE, sep = " and ") %>% 
    unite("comparison", 
          matches("comparison"),
          na.rm = TRUE, remove = TRUE, sep = " and ") %>% 
    unite("results", 
          matches("results_"),
          na.rm = TRUE, remove = TRUE, sep = " and ") %>% 
    unite("stat_methods", 
          matches("stat_methods_"),
          na.rm = TRUE, remove = TRUE, sep = " and ") %>% 
    unite("biome_edit", 
          matches("biome_"),
          na.rm = TRUE, remove = TRUE, sep = " and ") %>% 
    relocate(biome_edit, .after="biome") %>% 
    mutate(ecosystem=if_else(is.na(ecosystem),biome,ecosystem)) %>% 
    mutate(biome=if_else(is.na(biome),biome_edit,biome)) %>%
    select(-biome_edit) %>% 
    relocate(ecosystem,.after="biome") %>% 
    mutate(ecosystem=if_else(ecosystem==biome,NA,ecosystem)) %>%
    unite(ecosystem,
          c(ecosystem,biome),
          na.rm = TRUE, remove = FALSE, sep = " and ") %>%
    mutate(biome = na_if(biome, "")) %>% 
    mutate(biome=
           case_when(
             str_detect(biome,"boreal")~"boreal",
             str_detect(biome,"tropical")~"tropical",
             str_detect(biome,"atlantic.f")~"tropical",
             str_detect(biome,"cerrado")~"tropical",
             str_detect(biome,"amazon")~"tropical",
             str_detect(biome,"andean")~"tropical",
             str_detect(biome,"lowland.terra.firme.rainforest")~"tropical",
             str_detect(biome,"temperate")~"temperate",
             str_detect(biome,"eastern.deciduous.forest")~"temperate",
             str_detect(ecosystem,"lowland.rainforest")~"tropical",
             str_detect(ecosystem,"temperate.rainforest")~"temperate",
             article_id==25~"tropical",
             article_id==6~"tropical",
             article_id==46~"tropical",
             article_id==18~"temperate",
             article_id==38~"temperate",
             TRUE ~ as.character(biome)
           )) %>% 
  mutate(country=
           case_when(
             country=="united.states"~"usa",
             country=="new.zealand"~"new zealand",
             country=="united.kingdom"~"uk",
             country=="cote.d'ivore"~"ivory coast",
             country=="united.states"~"usa",
             TRUE ~ as.character(country)
           )) %>% 
    
    mutate(var_4=
             case_when(
               var_4=="sfrd"~"srfd",
               var_4=="gt"~NA,
               TRUE ~ as.character(var_4)
             )) %>% 
    mutate(var_3=
             case_when(
               var_3=="tf"~"tp",
               TRUE ~ as.character(var_3)
             )) %>% 
    mutate(var_2=
             case_when(
               var_2=="ir"~"lit",
               TRUE ~ as.character(var_2)
             )) %>% 
    mutate(layout=
            case_when(
              layout=="transect"~"perpendicular",
              TRUE ~ as.character(layout)
            )) %>% 
    mutate(start_date_1=if_else(is.na(start_date_1),start_date,start_date_1)) %>% 
    mutate(end_date_1=if_else(is.na(end_date_1),end_date,end_date_1)) %>% 
    select(-end_date,-start_date) %>% 
    mutate(latitude_1=if_else(is.na(latitude_1),latitude,latitude_1)) %>% 
    mutate(longitude_1=if_else(is.na(longitude_1),longitude,longitude_1)) %>% 
    select(-latitude,-longitude)
            
    
# Put columns names in ordser so similar ones together
  abiotic_df_wide <- abiotic_df_wide %>% 
    select(order(colnames(abiotic_df_wide))) %>% 
    relocate(start_date_1,.before="end_date_1") %>% 
    relocate(start_date_2,.before="end_date_2") %>% 
    relocate(longitude_1,.after="latitude_1") %>% 
    relocate(longitude_2,.after="latitude_2")
  
  abiotic_df_wide<-abiotic_df_wide %>% 
    mutate(v_air_temp = rowSums(across(starts_with("var_"), ~ str_detect(.x, "at"), .names = "temp"), na.rm = TRUE)) %>% 
    mutate(v_humidity = rowSums(across(starts_with("var_"), ~ str_detect(.x, "rh"), .names = "temp"), na.rm = TRUE)) %>% 
    mutate(v_wind = rowSums(across(starts_with("var_"), ~ str_detect(.x, "ws"), .names = "temp"), na.rm = TRUE)) %>% 
    mutate(v_soil_temp = rowSums(across(starts_with("var_"), ~ str_detect(.x, "st"), .names = "temp"), na.rm = TRUE)) %>% 
    mutate(v_vpd = rowSums(across(starts_with("var_"), ~ str_detect(.x, "vpd"), .names = "temp"), na.rm = TRUE)) %>% 
    mutate(v_par = rowSums(across(starts_with("var_"), ~ str_detect(.x, "par"), .names = "temp"), na.rm = TRUE)) %>% 
    mutate(v_other = rowSums(across(starts_with("var_"), ~ str_detect(.x, "ppfd|li|sft|pfd|srfd|sm|lux|h"), .names = "temp"), na.rm = TRUE)) %>% 
    unite(abiotic_vars,
          c(var_1,var_7),
          na.rm = TRUE, remove = TRUE, sep = ",") %>%
    mutate(across(starts_with("v_"), ~ .x == 1)) %>% 
    relocate(starts_with("v_"),.after=3)
    
    
  
  
    mutate(air_temp = case_when(if_any(str_detect(starts_with("var"), "at"))~1))
    mutate(air_temp = str_detect(starts_with("var_"), "at"))
    mutate(air_temp=str_detect(var_1:var_7,TRUE),.before=1)
  
  # Still need to clean up
  # field station and municipality - join to field_site
  
  # not sure about these
  # begin[begin=="AF"] <- "WS"
  # begin$layout[remove2==TRUE] <- "fixed.point"
  
  
  
  

abiotic_df_wide %>% 
  ungroup() %>% 
  summarize(n=n_distinct(article_id))

abiotic_df %>% 
  ungroup() %>% 
  summarize(n=n_distinct(article_id))


abiotic_df_wide %>% 
  ungroup() %>% 
  group_by(biome) %>% 
  tally()



abiotic_df_wide %>% 
  ungroup() %>% 
  group_by(layout) %>% 
  tally() %>% 
  arrange(desc(n))


# biome histogram figure -------------------------------------------------



hist_biome <- ggplot(abiotic_df_wide, aes(biome))+
  geom_bar()+
  theme_classic()+
  labs(x = "Biome", 
       y = "No. of studies with data")
ggsave("doc/images/biome_hist_eb.png", width = 4, height = 4, units = "in")



# study interval ----------------------------------------------------------


library(lubridate)

#calculate difference between start and end dates
interval_1<-abiotic_df_wide 
interval_2<-abiotic_df_wide 
interval_1$int<-interval(ymd(abiotic_df_wide$start_date_1), ymd(abiotic_df_wide$end_date_1))
interval_2$int<-interval(ymd(abiotic_df_wide$start_date_2), ymd(abiotic_df_wide$end_date_2))
#convert interval to total number of whole days
interval_1$days <- interval_1$int %/% days(1)
interval_2$days <- interval_2$int %/% days(1)


# there is a day that is reading as -12?

days<-c(interval_1$days,interval_2$days) %>% 
  as.data.frame() %>% 
  drop_na() %>% 
  rename(days=".") %>% 
  arrange(desc(days)) %>% 
  filter(days>0)

days_median<-median(days$days)
days_mean<-mean(days$days)


days_hist<-ggplot(days, aes(days)) +
  geom_histogram(binwidth = 20)+
  theme_classic()+
  labs(x = "Study duration (days)", 
       y = "No. of studies")+
  geom_vline(xintercept = days_mean, linetype="dashed", 
               color = "navy", size=0.5)+
  geom_vline(xintercept = days_median, linetype="dashed", 
             color = "darkred", size=0.5)
  
  
ggsave("doc/images/duration_hist_eb.png", width = 4, height = 4, units = "in")
