source("utilities.r")
library("data.table")
library("dplyr")
library("tidyverse")
library(ggrepel)
library(ggh4x)
library("ggplot2")

# we use the username to extract table01 from your username's folder 
username <- Sys.getenv("USERNAME")
x = paste0(".//",username)

# resereve all pathes that contain table01 in Y vector
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
# If you want to subset certain number of pathes
# replace c with Y in the for loop
 c <- Y[1:20]

plotdata <- list()
power <- list()
#loop to calculate all PK metrics and put it in one table for plotting
for (i in c) {
  
  # read each table01 one by one
  data <- fread(i)
  
  # extract the name of the model from the path
  pathSpliting <-  str_split(i, "/") %>% unlist()
  pathSpliting <-  str_split(pathSpliting[4], ",") %>% unlist()
  model <-  pathSpliting[1]
  
  # if you are working in a group each user will have a folder with his username
  # you can extract it after adjusting the number between the bractes according to ur case
  # Pharmacometrician <-  pathSpliting[9]
  
  # from each table01, extract the number of subjects in each table by taking the
  # the maximum of KEY variable
  Nsubjects <- max(data$KEY)
  
  # from each table01, extract the number of sample time points
  sampling <- fread(i) %>% filter(replicate == 1, KEY == 1) %>% nrow()
  
  #from each table01, extract the test formulation, which has always the largest number.
  FREL <- max(data$FORM)
  
  # calculate the Cmax and AUC for each individual alone by taking
  # the maximum for each of them from table01
  table1all <-
    fread(i) %>% select(replicate,
                        id = KEY,
                        FORM,
                        time,
                        CMAX = C,
                        AUC = CUMAUC) %>%
    group_by(replicate, id, FORM) %>%
    summarise(CMAX = max(CMAX), AUC = max(AUC))
  
  
  # bioequivalence assessment (TOST) for each replicate by nesting technique
  # using nesting technique to apply the TOST function on each replicate dataset
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
  ## the 80 <= & <= 125 criteria
  table1allBE <- table1allBE %>%
    mutate(BEassess = ifelse(lower >= 80 & upper <= 125, 1, 0))
  
  
  ## change the variable for PK parameter from longer to wider format
  ##  so we can assess each one alone, and combined.
  
  table1allBEres <- table1allBE %>%
    select(replicate, parameter, BEassess, ratio, lower, upper) %>%
    pivot_wider(names_from = parameter,
                values_from = c(BEassess, ratio, lower, upper)) %>%
    # create a new variable that assess the combined Cmax_AUC
    
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
    
    #add the charachteristics of each scenario for each power result
    mutate(
      Ntotal = Nsubjects,
      # model=model,
      Narm = Nsubjects / 2,
      formulation = FREL,
      sampling_No  = sampling
    )
  
  ## convert the all power results to tidy format for plotting
  # (i.e. changing them from the wider format to longer format)
  plotdata[[i]] <-
    gather(powerresults,
           "BEmeasurement","Power",
           -Ntotal, -Narm,-formulation,-sampling_No)  %>% 
    mutate(
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
        between(sampling_No , 4, 6),
        "sparse",
        if_else(
          between(sampling_No , 8, 10),
          "moderate",
          if_else(between(sampling_No , 12, 14), "rich",
           "wrong"
          )
               )
             ),
             model = model,
             # Pharmacometrician = Pharmacometrician,
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
  paste0(".//plotdatadf.csv")
)




#Adjusting MBBE data to be compatible with NCA data
plotdataMBBEdf <- plotdatadf %>% select( - path)


#power data for NCA

xNCA= paste0(".//",username)

YNCAPlot = list.files(
  path = xNCA,
  pattern = "plotdata.csv$",
  all.files = TRUE,
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = FALSE,
  include.dirs = TRUE,
  no.. = TRUE
)
plotdataNCA<-list()

# C <- YNCAPlot[1:3]
#loop to calculate all PK metrics and put it in one table for plotting
for(i in YNCAPlot) {
  plotdataNCA[[i]] <- fread(i)
  
}




plotdataNCAdf <- rbindlist(plotdataNCA) %>%
  select(-sampling, Ntotal, Narm, FREL, BEmeasurement, Power, Sampling, model) %>% 
  rename(sampling = Sampling)


#combining NCA AND MBBE
data <- rbind(plotdataNCAdf,plotdataMBBEdf)







# 
# #
# ggplot(plotdatadf,
#        aes(Ntotal, Power, color = FREL)) +
#   #geom_point()+
#   geom_hline(yintercept = 0.9) +
#   geom_hline(yintercept = 0.8) +
#   geom_line() +
#   geom_text(aes(label = round(Power, 1)), show.legend = FALSE) +
#   facet_nested(sampling ~ model + FREL, switch = "y") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   theme_bw(base_size = 16) +
#   theme(strip.placement = "outside",
#         strip.text.y.left = element_text(angle = 0)) +
#   labs(x = "Total N of Subjects in Study", y = "Simulation Based Power\nN=500 replicates")
# 
# 
# 
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


