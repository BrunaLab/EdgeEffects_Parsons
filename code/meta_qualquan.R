#Combined qualitative and quantitative data

# load libraries ----------------------------------------------------------

library(tidyverse)
library(gridExtra)


# load data ---------------------------------------------------------------

#knit together qualitative & quantitative data
vardata <- read_csv("./Outputs/vardata.csv")
mergedrefined8 <- read_csv("./Data/mergedrefined8.csv")

qualquan <- full_join(vardata,mergedrefined8,by="article.id")


qualquan<-qualquan %>% 
  mutate(AT=
           case_when(
             var.1 == "AT"|
               var.2 == "AT"|
               var.3 == "AT"|
               var.4 == "AT"|
               var.5 == "AT"|
               var.6 == "AT"|
               var.7 == "AT" ~ "AT",
             TRUE ~ NA_character_)
         ) %>% 
  mutate(DT=
           case_when(
             var.1 == "DT"|
               var.2 == "DT"|
               var.3 == "DT"|
               var.4 == "DT"|
               var.5 == "DT"|
               var.6 == "DT"|
               var.7 == "DT" ~ "DT",
             TRUE ~ NA_character_)
  )


qualquan %>% paste("var.1","var.7", sep=" ")


summary(as.factor(qualquan$DT))

# mutate(H=
# mutate(PT=
# mutate(RH=
# mutate(TP=
# mutate(VPD=
# mutate(ST=
# mutate(SH=
# mutate(SM=
# mutate(SFT=
# mutate(LI=
# mutate(LIT=
# mutate(LUX=
# mutate(PAR=
# mutate(PPFD=
# mutate(SRFD=
# mutate(WS=
  


# citations by region -----------------------------------------------------

quanbroad <- qualquan %>% 
  filter(!is.na(just.dist)) %>% 
  select(article.id, citations,broad) %>% 
  distinct()


n_trop<-quanbroad %>% 
  filter(broad=="tropical") %>% 
  summarize(n=n_distinct(article.id))

n_temp<-quanbroad %>% 
  filter(broad=="temperate") %>% 
  summarize(n=n_distinct(article.id))


n_boreal<-quanbroad %>% 
  filter(broad=="boreal") %>% 
  summarize(n=n_distinct(article.id))



quan.cite<-qualquan %>% 
  filter(!is.na(just.dist)) %>% 
  select(article.id, citations,broad) %>% 
  distinct() %>% 
  group_by(broad) %>% 
  summarize(sum_cites=sum(citations)) 
quan.cite

# figures -----------------------------------------------------------------

#graph by broad region ####

tropics <- qualquan %>% 
  filter(!is.na(just.dist)) %>% 
  filter(broad == "tropical")

boreal <- qualquan %>% 
  filter(!is.na(just.dist)) %>% 
  filter(broad == "boreal")

temperate <- qualquan %>% 
  filter(!is.na(just.dist)) %>% 
  filter(broad == "temperate")

#tropics trends ####
plot_perc_diff<-ggplot(tropics,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentws_diff), color = "red",alpha=0) +
  geom_smooth(aes(y=percentVPD_diff), color = "blue",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "cyan",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))
plot_perc_diff<-plot_perc_diff+theme_classic()
plot_perc_diff



# figure 8  TROPICAL -----------------------------------------------------


# temp & humidity panel

ba <- ggplot(tropics,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Temperature and humidity")+
  geom_point(aes(y=percent_diff),color="lightslateblue",alpha=0.2) +
  geom_point(aes(y=percentrh_diff),color="indianred3",alpha=0.2) +
  geom_line(aes(y=percent_diff,group=idseg),color="lightslateblue",alpha=0.2) +
  geom_line(aes(y=percentrh_diff,group=idseg),color="indianred3",alpha=0.2) +
  geom_smooth(aes(y=percent_diff), color = "lightslateblue",alpha=0,size=1.5) +
  geom_smooth(aes(y=percentrh_diff), color = "indianred3",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))+
  ylim(-20,20)
ba




# VPD - not used in figure
bb <- ggplot(tropics,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=percentVPD_diff), color = "lightslateblue") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))
bb

# soil panel

bc <- ggplot(tropics,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil")+
  geom_point(aes(y=percentsm_diff),color="turquoise3",alpha=0.2) +
  geom_point(aes(y=percentst_diff),color="goldenrod2",alpha=0.2) +
  geom_line(aes(y=percentsm_diff,group=idseg),color="turquoise3",alpha=0.2) +
  geom_line(aes(y=percentst_diff,group=idseg),color="goldenrod2",alpha=0.2) +
  geom_smooth(aes(y=percentsm_diff), color = "turquoise3",alpha=0,size=1.5) +
  geom_smooth(aes(y=percentst_diff), color = "goldenrod2",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))+
  ylim(-50,50)
bc




# Light panel
bd <- ggplot(tropics,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Light")+
  geom_point(aes(y=percentPAR_diff),color="maroon2",alpha=0.2)+
  geom_line(aes(y=percentPAR_diff,group=idseg),color="maroon2",alpha=0.2)+
  geom_smooth(aes(y=percentPAR_diff), color = "maroon2",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))
bd

# Wind

be <- ggplot(tropics,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Wind")+
  geom_point(aes(y=percentws_diff),color="thistle4",alpha=0.2)+
  geom_line(aes(y=percentws_diff,group=idseg),color="thistle4",alpha=0.2)+
  geom_smooth(aes(y=percentws_diff), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))
be

grid.arrange(ba,bc,bd,be,ncol=2,nrow=2)




#  Light (PAR) Difference  ------------------------------------------------


plot_perc_diff_par<-ggplot(tropics,aes(x = just.dist)) +
  geom_smooth(aes(y=percentPAR_diff), color = "yellow",alpha=0) +
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))
plot_perc_diff_par<-plot_perc_diff_par+theme_classic()
plot_perc_diff_par




# figure 8  TEMPERATE -----------------------------------------------------

ggplot(temperate,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentVPD_diff), color = "blue",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "cyan",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))+
  theme_classic()


# light
ggplot(temperate,aes(x = just.dist)) +
  geom_smooth(aes(y=percentPAR_diff), color = "yellow",alpha=0) +
  geom_smooth(aes(y=percentws_diff), color = "red",alpha=0) +
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))+
  theme_classic()

# air temp and humidity
ca <- ggplot(temperate,aes(x = just.dist)) +
  ggtitle("Temperature and humidity")+
  geom_point(aes(y=percent_diff),color="lightslateblue",alpha=0.2) +
  geom_point(aes(y=percentrh_diff),color="indianred3",alpha=0.2) +
  geom_line(aes(y=percent_diff,group=idseg),color="lightslateblue",alpha=0.2) +
  geom_line(aes(y=percentrh_diff,group=idseg),color="indianred3",alpha=0.2) +
  geom_smooth(aes(y=percent_diff), color = "lightslateblue",alpha=0,size=1.5) +
  geom_smooth(aes(y=percentrh_diff), color = "indianred3",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))+
  ylim(-15,15)+
  theme_classic()
ca

# vpd (not used)
cb <- ggplot(temperate,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=percentVPD_diff), color = "lightslateblue") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))
cb

# Soil 
cc <- ggplot(temperate,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil")+
  geom_point(aes(y=percentsm_diff),color="turquoise3",alpha=0.2) +
  geom_point(aes(y=percentst_diff),color="goldenrod2",alpha=0.2) +
  geom_line(aes(y=percentsm_diff,group=idseg),color="turquoise3",alpha=0.2) +
  geom_line(aes(y=percentst_diff,group=idseg),color="goldenrod2",alpha=0.2) +
  geom_smooth(aes(y=percentsm_diff), color = "turquoise3",alpha=0,size=1.5) +
  geom_smooth(aes(y=percentst_diff), color = "goldenrod2",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))+
  ylim(-25,25)
cc

# Light panel
cd <- ggplot(temperate,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Light")+
  geom_point(aes(y=percentPAR_diff),color="maroon2",alpha=0.2)+
  geom_line(aes(y=percentPAR_diff,group=idseg),color="maroon2",alpha=0.2)+
  geom_smooth(aes(y=percentPAR_diff), color = "maroon2",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))
cd

# wind
ce <- ggplot(temperate,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Wind")+
  geom_point(aes(y=percentws_diff),color="thistle4",alpha=0.2)+
  geom_line(aes(y=percentws_diff,group=idseg),color="thistle4",alpha=0.2)+
  geom_smooth(aes(y=percentws_diff), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))
ce

grid.arrange(ca,cc,cd,ce,ncol=2,nrow=2)


# FIgure 8 BOREAL ---------------------------------------------------------


#boreal trends ####

ggplot(boreal,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_point(aes(y=percentsm_diff),color="orange")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250),ylim=c(-30,30))


# temp nd Humidity
da <- ggplot(boreal,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Temperature and humidity")+
  geom_smooth(aes(y=percent_diff), color = "lightslateblue",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "indianred3",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))
da

# vpd
db <- ggplot(boreal,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=percentVPD_diff), color = "lightslateblue") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))
db


# soil
dc <- ggplot(boreal,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil")+
  geom_smooth(aes(y=percentsm_diff), color = "turquoise3",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "goldenrod2",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))
dc

# PAR
dd <- ggplot(boreal,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Light")+
  geom_smooth(aes(y=percentPAR_diff), color = "maroon2",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))
dd

# wind
de <- ggplot(boreal,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Wind")+
  geom_smooth(aes(y=percentws_diff), color = "thistle4",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))
de

#no data for light and wind

grid.arrange(da,dc,dd,de,ncol=2,nrow=2)


# temperate to compare to boreal ------------------------------------------


ggplot(temperate,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_point(aes(y=percentsm_diff),color="orange")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250),ylim=c(-30,30))


# tropics to compare to boreal  -------------------------------------------


ggplot(tropics,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_point(aes(y=percentsm_diff),color="orange")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250),ylim=c(-30,30))


# analyses ----------------------------------------------------------------

#GLM on distance, lat ####
#remove percentages on quanonly, log to remove neg
intermed <- log1p((quanonly[,c(6,16,19,22,25,28,31)]/100))
names(intermed) <- c("proat_diff","propar_diff","prorh_diff","prosm_diff","prost_diff","provpd_diff","prows_diff")
forlatglm <- cbind(intermed,quanonly)

atlat <- glm(proat_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm) #better
atlat2 <- glm(proat_diff ~ simple.lat, family = gaussian, data = forlatglm)
atlat3 <- glm(proat_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #better

rhlat <- glm(prorh_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm) #better
rhlat2 <- glm(prorh_diff ~ simple.lat, family = gaussian, data = forlatglm)
rhlat3 <- glm(prorh_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #better

vpdlat <- glm(provpd_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)
vpdlat2 <- glm(provpd_diff ~ simple.lat, family = gaussian, data = forlatglm)
vpdlat3 <- glm(provpd_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #best

stlat <- glm(prost_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm) #better
stlat2 <- glm(prost_diff ~ simple.lat, family = gaussian, data = forlatglm)
stlat3 <- glm(prost_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #better

smlat <- glm(prosm_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)
smlat2 <- glm(prosm_diff ~ simple.lat, family = gaussian, data = forlatglm) #somewhat better
smlat3 <- glm(prosm_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm)

wslat <- glm(prows_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm) #somewhat better
wslat2 <- glm(prows_diff ~ simple.lat, family = gaussian, data = forlatglm)
wslat3 <- glm(prows_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #somewhat better

parlat <- glm(propar_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm) #better
parlat2 <- glm(propar_diff ~ simple.lat, family = gaussian, data = forlatglm)
parlat3 <- glm(propar_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #better

summary(lm(proat_diff ~ simple.lat,data=forlatglm))

#graph GLM ####
ggplot(forlatglm,aes(x = simple.lat)) +
  geom_smooth(aes(y=proat_diff),method="glm", color = "green",alpha=0) +
  geom_smooth(aes(y=prorh_diff),method="glm", color = "red",alpha=0) +
  geom_smooth(aes(y=provpd_diff),method="glm", color = "purple",alpha=0) +
  geom_point(aes(y=proat_diff),color="green")+
  geom_point(aes(y=prorh_diff),color="red")+
  geom_point(aes(y=provpd_diff),color="purple")