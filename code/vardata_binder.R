
library(tidyverse)
# par clean ---------------------------------------------------------------
source("./code/par_analysis.r")
short<-par_analysis()[1] 
short<-as.data.frame(short)|>as_tibble()

short.par<-par_analysis()[2] 
short.par<-as.data.frame(short.par)|>
  as_tibble() %>% 
  mutate(abiotic_var="rh") %>% 
  relocate(abiotic_var,.before=1)
 
# rh clean ----------------------------------------------------------------
source("./code/rh_analysis.r")
short.rh<-rh_analysis()
short.rh<-as.data.frame(short.rh)|>
  as_tibble() %>% 
  mutate(abiotic_var="rh") %>% 
  relocate(abiotic_var,.before=1)


# soil moisture clean -----------------------------------------------------------
source("./code/soil_moisture_analysis.r")
short.sm<-soil_moisture_analysis()
short.sm<-as.data.frame(short.sm)|>
  as_tibble() %>% 
  mutate(abiotic_var="sm") %>% 
  relocate(abiotic_var,.before=1)


# wind speed analysis -----------------------------------------------------
source("./code/wind_speed_analysis.r")
short.ws<-wind_speed_analysis()
short.ws<-as.data.frame(short.ws)|>
  as_tibble() %>% 
  mutate(abiotic_var="ws") %>% 
  relocate(abiotic_var,.before=1)


# soil temp analysis -----------------------------------------------------
source("./code/soil_temp_analysis.r")
short.st<-soil_temp_analysis()
short.st<-as.data.frame(short.st)|>
  as_tibble() %>% 
  mutate(abiotic_var="st") %>% 
  relocate(abiotic_var,.before=1)


# air temp analysis -----------------------------------------------------
source("./code/air_temp_analysis.r")
short.at<-air_temp_analysis()
short.at<-as.data.frame(short.at)|>
  as_tibble() %>% 
  mutate(abiotic_var="at") %>% 
  relocate(abiotic_var,.before=1)



# vpd analysis -----------------------------------------------------
source("./code/vpd_analysis.r")
short.vpd<-vpd_analysis()
short.vpd<-as.data.frame(short.vpd)|>
  as_tibble() %>% 
  mutate(abiotic_var="vpd") %>% 
  relocate(abiotic_var,.before=1)



combined<-left_join(short,short.par) %>% as_tibble()
combined1<-full_join(combined, short.rh)
combined2<-full_join(combined1, short.sm)
combined3<-full_join(combined2, short.st)
combined4<-full_join(combined3, short.vpd)
combined5<-full_join(combined4, short.ws)
combined6<-full_join(combined5, short.at)
names(combined6)


#################COMBINE ALL ####
# combined <- merge(short, short.par[,c(1,2,3,10,11,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
# combined1 <- merge(combined, short.rh[,c(1,2,3,9,10,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
# combined2 <- merge(combined1, short.sm[,c(1,2,3,9,10,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
# combined3 <- merge(combined2, short.st[,c(1,2,3,9,10,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
# combined4 <- merge(combined3, short.vpd[,c(1,2,3,9,10,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
# combined5 <- merge(combined4, short.ws[,c(1,2,3,9,10,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
library(janitor)
combined6<-combined6 %>% remove_empty(c("rows",'cols'))

combined6 <-
combined6 %>% dplyr::select("article.id",
         "segment_n",
         "just.dist",
         "idseg",
         "stat_sig",
         "percent_diff",
         "season",
         "matrix_type",
         "edge_orient",
         "edge_age_years",
          "SD_SE_CI_V",
          "SD_SE_CI_V_n",
          "full_diff",
         "PAR_var",
         "PAR_var_n",
         "percentPAR_diff",
         "RH_var",
         "RH_var_n",
         "percentrh_diff",
         "SM_var",
         "SM_var_n",
         "percentsm_diff",
         "ST_var",
         "ST_var_n",
         "percentst_diff",
         "VPD_var",
         "VPD_var_n",
         "percentVPD_diff",
         "WS_var",
         "WS_var_n",
         "percentws_diff")


combined6==vardata


#check - how many rows with all variables?
full <- data.frame(combined5[!is.na(combined5[,6]) & !is.na(combined5[,16]) & !is.na(combined5[,19]) &
                               !is.na(combined5[,22]) & !is.na(combined5[,25]) & !is.na(combined5[,28]) &
                               !is.na(combined5[,31]),])



#export combined dataset
write.csv(combined5,"./Outputs/vardata.csv",row.names = F)