library("data.table")
library("dplyr")
library("tidyverse")
library(ggrepel)
library(ggh4x)
library("ggplot2")
# suppressPackageStartupMessages({
#   library(PKNCA)
#   library(dplyr)
#   library(cowplot)
#   library(knitr)
#   library(ggplot2)
# })
# scale_colour_discrete <- scale_colour_hue
# scale_fill_discrete <- scale_fill_hue
username <- Sys.getenv("USERNAME")



# dir.create("C:/Users/mali/Certara/Samer Mouksassi - AFPSandozProject/RESULTS/Automation")

# x = paste0("C:/Users/",
#            username,
#            "/Certara/Samer Mouksassi - AFPSandozProject/RESULTS")


x = paste0("C:/Users/mali/OneDrive - Certara/Desktop/RsNLME/Results")

source("utilities.r")

Y = list.files(
  path = x,
  pattern = "table01.*\\.csv$",
  all.files = TRUE,
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = FALSE,
  include.dirs = TRUE,
  no.. = FALSE
)
c <- Y[1:20]
plotdata <- list()
power <- list()
#loop to calculate all PK metrics and put it in one table for plotting
for (i in c) {
  # Head of for-loop
  
  data <- fread(i)
  pathSpliting <-  str_split(i, "/") %>% unlist()
  model <-  pathSpliting[8]
  Pharmacometrician <-  pathSpliting[9]
  Nsubjects <- max(data$id5)
  
  sampling <- fread(i) %>% filter(replicate == 1, id5 == 1) %>% nrow()
  FREL <- max(data$FORM)
  
  # calculate the Cmax and AUC for each individual alone.
  table1all <-
    fread(i) %>% select(replicate,
                        id = id5,
                        FORM,
                        time,
                        CMAX = C,
                        AUC = CUMAUC) %>%
    group_by(replicate, id, FORM) %>%
    summarise(CMAX = max(CMAX), AUC = max(AUC))
  
  
  # bioequivalence assessment (TOST) for each replicate by nesting technique
  
  table1all <- table1all %>%
    group_by(replicate) %>%
    nest() %>%
    mutate(BEresults = map(data, ~ rbind(
      BE.Parallel(.x, parameter = "AUC"),
      BE.Parallel(.x, parameter = "CMAX")
    )))
  
  # unnesting the results of TOST
  
  table1allBE <- table1all %>%
    unnest(cols = c(BEresults), names_repair = "universal")
  
  ## testing the biequivalence assessment based on
  ## the 80 < criteria<125
  table1allBE <- table1allBE %>%
    mutate(BEassess = ifelse(lower >= 80 & upper <= 125, 1, 0))
  
  
  ## Separate each PK parameter so we can assess each one alone, and combined.
  
  table1allBEres <- table1allBE %>%
    select(replicate, parameter, BEassess, ratio, lower, upper) %>%
    pivot_wider(names_from = parameter,
                values_from = c(BEassess, ratio, lower, upper)) %>%
    mutate(BEassess_AUC_CMAX = ifelse(BEassess_AUC == 1 &
                                        BEassess_CMAX == 1, 1, 0))
  
  
  # calculate the cumulative power for each parameter
  
  table1allBEres <- table1allBEres %>%
    ungroup() %>%
    mutate(
      CumPowerAUC = 100 * cumsum(BEassess_AUC) / replicate,
      CumPowerCmax = 100 * cumsum(BEassess_CMAX) / replicate,
      CumPowerAUCMax = 100 * cumsum(BEassess_AUC_CMAX) / replicate
    )
  
  
  
  # calculate the percetage of achieveing
  # bioequivalence over all replicates (power)
  powerresults <- table1allBEres %>%
    ungroup() %>%
    summarize(
      BEAUC = mean(BEassess_AUC),
      BECMAX = mean(BEassess_CMAX),
      BEauccmax = mean(BEassess_AUC_CMAX)
    ) %>%
    mutate(
      Ntotal = Nsubjects,
      # model=model,
      Narm = Nsubjects / 2,
      formulation = FREL,
      Sampling = sampling
    )
  
  ## convert the all power results to tidydata for plotting and a
  plotdata[[i]] <-
    gather(powerresults,
           "BEmeasurement",
           "Power",
           -Ntotal,
           -Narm,
           -formulation,
           -Sampling)  %>% mutate(
             Power = Power,
             FREL =  as.character(
               case_when(
                 formulation == 0 ~ 1,
                 formulation  == 1 ~ 1.05,
                 formulation  == 2 ~ 1.1,
                 formulation  == 3 ~ 1.15,
                 formulation  == 4 ~ 1.2,
                 formulation  == 5 ~ 1.25,
                 TRUE ~ -1
               )
             ),
             sampling = if_else(
               between(Sampling, 4, 6),
               "sparse",
               if_else(
                 between(Sampling, 8, 10),
                 "moderate",
                 if_else(between(Sampling, 12, 14), "rich",
                         
                         "wrong")
               )
             ),
             model = model,
             Pharmacometrician = Pharmacometrician,
             path = i
             
           )
  
  # paste0("You updated ",length(plotdatadf)*100/3*length(Y)," %, ## This the scenario number ", i, " with Total Number
  #              of subjects = ", Nsubjects, "with ", sampling, "sampling", " and fitted by", Pharmacometrician, "using ", model)
  #
  #
  #
  
  
  # ggplot(powerresults, aes(x= Ntotal, y=))
  # fwrite(paste0("C:/Users/mali/Certara/Samer Mouksassi - AFPSandozProject/RESULTS/Automation.csv"))
  #
  #
}

plotdatadf <- rbindlist(plotdata)

#extracting the table for revision and pick up errors
fwrite(
  plotdatadf,
  paste0("C:/Users/mali/OneDrive - Certara/Desktop/RsNLME/plotdatadf.csv")
)

#
ggplot(plotdatadf,
       aes(Ntotal, Power, color = FREL)) +
  #geom_point()+
  geom_hline(yintercept = 0.9) +
  geom_hline(yintercept = 0.8) +
  geom_line() +
  geom_text(aes(label = round(Power, 1)), show.legend = FALSE) +
  facet_nested(sampling ~ model + FREL, switch = "y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw(base_size = 16) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)) +
  labs(x = "Total N of Subjects in Study", y = "Simulation Based Power\nN=500 replicates")



o = plotdatadf %>% filter(BEmeasurement == "BEauccmax",  FREL != 1.15)
#myplot
ggplot2::ggplot(o, aes(
  x = Ntotal,
  y = Power,
  group = FREL,
  color = FREL
)) +
  geom_line(size = 1.2) +
  geom_label_repel(aes(label = Power), nudge_x = 0.35, size = 2) +
  facet_nested(sampling ~ model + BEmeasurement, switch = "y") +
  
  geom_hline(
    yintercept = c(0.80, 0.90),
    linetype = "dashed",
    color = "red"
  ) +
  labs(x = "Total N of Subjects in Study", y = "Simulation Based Power\nN=500 replicates")
