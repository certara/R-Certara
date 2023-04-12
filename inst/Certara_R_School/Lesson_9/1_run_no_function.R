source("utilities.r")
# load the libraries
library(tidyverse)
library(Certara.RsNLME)
library(data.table)
library(tictoc)
library(PKNCA)
suppressPackageStartupMessages({
  library(PKNCA)
  library(dplyr)
  library(cowplot)
  library(knitr)
  library(ggplot2)
})
scale_colour_discrete <- scale_colour_hue
scale_fill_discrete <- scale_fill_hue

.runninglocal <- TRUE # set flag if running local or vdi
.Nsubjects <- 10/2   #number of subject in each formulation group
.formula = 2     # Number of formulation ( 0 = Frel 1,    1 = Frel 1.05,   2= Frel 1.10,   3= Frel 1.15, 4= Frel 1.20   , 5= Frel 1.25)
.model = "truemodel"   #truemodel or 2CMTLINNLIN
.numCores = ifelse(.runninglocal,6,27)
.numIterations = 1000 #number of iterations
.RSNLMEENGINE <- "FOCE-ELS"
.NBOOT = ifelse(.runninglocal,10,500)#number of bootstrap datasets
.sampling = "sparse" #moderate, sparse

FREL <- case_when(
  .formula == 0 ~ 1,
  .formula == 1 ~ 1.05,
  .formula == 2 ~ 1.1,
  .formula == 3 ~ 1.15,
  .formula == 4 ~ 1.2,
  .formula == 5 ~ 1.25,
  TRUE ~ -1
)

if(!.runninglocal) {Allsims.df <- fread("X:/APT fellowship/Allsims.df.csv")}
if(.runninglocal){ Allsims.df <- fread(".//Allsims.df.csv")}

Allsims.df <- Allsims.df %>%
  rename(amtorg=amt) %>%
  mutate(amt= ifelse(!is.na(amtorg),420000,NA))

## Testing the correct implementation of the previous step
Allsims.df %>%
  distinct(amt,amtorg,formulation,Frel)

head(Allsims.df)

#Sys.setenv("NLME_KEEP_GRID_RESULTS" = "FALSE")
print("Certara.NLME8")
print(
  packageVersion("Certara.NLME8"))
username <- Sys.getenv("USERNAME")
print(username)


sampling <- .sampling
time_points_grp1 <- NULL
time_points_grp2 <-  NULL
time_points <- NULL

if(sampling=="rich"){
  time_points <- c(0, 1.5, 24, 72, 168,  504 ,840, 1176 ,1512, 1680,1848, 2256 ,2352)
}
if(sampling=="moderate"){
  time_points <- c(0, 1.5,  24 , 168  ,   840 , 1512, 1680 , 1848 , 2520)
}
if(sampling=="sparse"){
  time_points_grp1 <-   c(0,1.5 , 168  , 1512 , 2016) # sparse grp1
  time_points_grp2 <-   c(0,24 , 1008 , 1680 ,  2520) # sparse grp2
  time_points <- sort(unique(c(time_points_grp1,time_points_grp2)))
}
sparse <- ifelse(sampling=="sparse",TRUE,FALSE)

time_points

source("./utilities.R")

Allsims.df <- Allsims.df %>%
  as.data.frame() %>%
  select(id,amt,time,C,CObs,CUMAUC,formulation,Frel)

frel_form <- Allsims.df  %>%
  distinct(Frel,formulation )%>%
  filter(formulation %in% c(.formula))
path <- paste0("./",username,"")
frel_form


foldername <- paste0(path,"/",paste0(.model,
                                     ",N=", .Nsubjects,
                                     ",Frel=", frel_form["Frel"],
                                     ",Sampling=",.sampling))
foldername
if(file.exists(path)){
  dir.create(foldername)
}else{
  dir.create(path)
  dir.create(foldername)
}
x =read_lines(paste0(.model,".txt"))

  scenario1 <- Allsims.df %>%
    filter(formulation %in% c(0,.formula)) %>%
    filter(time %in% time_points) %>% # select the timepoints for this scenario
    select(id,amt,time,C,CObs,formulation,Frel) %>% #keep needed columns
    filter(!(time==0 & is.na(amt)))# remove duplicate time 0
  scenario1$CObs <- ifelse(scenario1$time==0,NA,scenario1$CObs)


  ggplot(scenario1,aes(time,C,color=as.factor(Frel)))+
    geom_point()+
    facet_grid(~Frel)
    scale_y_log10()


  uniqueids <- scenario1 %>%
    group_by(formulation)%>%
    summarize(id=unique(id))%>%
    group_by(formulation) %>%
    mutate(nid=row_number())%>%
    mutate(grp=ifelse(nid<=500,"grp1","grp2"))
  scenario1 <- left_join(scenario1,uniqueids %>% select(-nid))
  scenario1 <- scenario1 %>%
    mutate(timeflag = ifelse(grp=="grp1"& time %in% time_points_grp1, 1,0))%>%
    mutate(timeflag = ifelse(grp=="grp2"& time %in% time_points_grp2, 1,timeflag))%>%
    filter(timeflag==1)
  scenario1 <- scenario1 %>% select(-timeflag)

ggplot(scenario1,aes(time,C,color=as.factor(grp)))+
  geom_point()+
  facet_grid(~Frel)+
  scale_y_log10()



bootdata1<- as.data.frame(resample_df(
  df = scenario1,
  key_cols = c("id"),
  strat_cols = c("formulation","grp"),
  n = .Nsubjects * 2
)) %>%
  mutate(replicate = 1)
library(patchwork)

ggplot(scenario1,aes(time,C,color=as.factor(grp)))+
  geom_line(aes(group=id),alpha=0.1)+
  geom_point()+
  facet_grid(~Frel)+
  scale_y_log10() |
ggplot(bootdata1,aes(time,C,color=as.factor(grp)))+
  geom_line(aes(group=KEY))+
  geom_point()+
  facet_grid(~Frel)+
  scale_y_log10()


a <- ggplot(as.data.frame(resample_df(
  df = scenario1,
  key_cols = c("id"),
  strat_cols = c("formulation","grp"),
  n = .Nsubjects * 2
)),aes(time,C,color=as.factor(grp)))+
  geom_point()+
  geom_line(aes(group=KEY))+
  facet_grid(~Frel)+
  scale_y_log10()
b <- ggplot(as.data.frame(resample_df(
  df = scenario1,
  key_cols = c("id"),
  strat_cols = c("formulation","grp"),
  n = .Nsubjects * 2
)),aes(time,C,color=as.factor(grp)))+
  geom_point()+
  geom_line(aes(group=KEY))+
  facet_grid(~Frel)+
  scale_y_log10()

c <- ggplot(as.data.frame(resample_df(
  df = scenario1,
  key_cols = c("id"),
  strat_cols = c("formulation","grp"),
  n = .Nsubjects * 2
)),aes(time,C,color=as.factor(grp)))+
  geom_point()+
  geom_line(aes(group=KEY))+
  facet_grid(~Frel)+
  scale_y_log10()
d <- ggplot(as.data.frame(resample_df(
  df = scenario1,
  key_cols = c("id"),
  strat_cols = c("formulation","grp"),
  n = .Nsubjects * 2
)),aes(time,C,color=as.factor(grp)))+
  geom_point()+
  geom_line(aes(group=KEY))+
  facet_grid(~Frel)+
  scale_y_log10()
a+b+c+d



