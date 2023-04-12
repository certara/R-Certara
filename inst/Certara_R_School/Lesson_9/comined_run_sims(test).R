# load the libraries
library(tidyverse)
library(Certara.RsNLME)
library(data.table)
library(tictoc)
library(tidyverse)
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

# Loading the data, and its directory is based on you are running the code from VM or local PC
if (!.runninglocal) {
  Allsims.df <- fread("X:/APT fellowship/Allsims.df.csv")
}
if (.runninglocal) {
  Allsims.df <- fread("Allsims.df.csv")
}


## To mimic the reality, the fitting algorithm will
## read the dose for test and reference so the algorithm can estimate the Frel.
Allsims.df <- Allsims.df %>%
  rename(amtorg = amt) %>%
  mutate(amt = ifelse(!is.na(amtorg), 420000, NA))

## Testing the correct implementation of the previous step
Allsims.df %>%
  distinct(amt, amtorg, formulation, Frel)

username <- Sys.getenv("USERNAME")
run_sims <- function(model = "truemodel",
                     Nsubjects = 10 / 2 ,
                     #number of subject in each formulation group
                     formula = 2,
                     # Number of formulation ( 0 = Frel 1,    1 = Frel 1.05,   2= Frel 1.10,   3= Frel 1.15, 4= Frel 1.20   , 5= Frel 1.25)
                     sampling  = "sparse",
                     runninglocal = .runninglocal,
                     path_on_vdi = "X:/APT fellowship/",
                     numCores = 6,
                     numIterations = 10,
                     #number of iterations
                     RSNLMEENGINE = "FOCE-ELS",
                     maxStepsODE = .maxStepsODE,
                     NBOOT = 10#number of bootstrap datasets,
                     
){
  ## Subsetting and bootstraping
  # ----
  time_points_grp1 <- NULL
  time_points_grp2 <-  NULL
  time_points <- NULL
  
  if (sampling == "rich") {
    time_points <-
      c(0, 1.5, 24, 72, 168,  504 , 840, 1176 , 1512, 1680, 1848, 2256 , 2352)
  }
  if (sampling == "moderate") {
    time_points <-
      c(0, 1.5,  24 , 168  ,   840 , 1512, 1680 , 1848 , 2520)
  }
  if (sampling == "sparse") {
    time_points_grp1 <-   c(0, 1.5 , 168  , 1512 , 2016) # group 1
    time_points_grp2 <-   c(0, 24 , 1008 , 1680 ,  2520) # group 2
    time_points <-
      sort(unique(c(time_points_grp1, time_points_grp2)))
  }
  ## put a binary variable for sparse scenario for readability
  ## to use it instead of sampling=="sparse"
  sparse <- ifelse(sampling == "sparse", TRUE, FALSE)
  
  ## change the working directory based on the machine
  ## to get the R sources (R functions)
  if (!runninglocal) {
    setwd(paste0(path_on_vdi, username, "/"))
  }
  if (!runninglocal) {
    source("X:/APT fellowship/utilities.R")
  }
  
  if (runninglocal) {
    source("./utilities.R")
  }
  
  ## just keep the needed variables in the analysis
  ## and discard the others.
  Allsims.df <- Allsims.df %>%
    as.data.frame() %>%
    select(id, amt, time, C, CObs, CUMAUC, formulation, Frel)
  
  ## extract the Frel based on the user input for formulation
  ## to be used in labelling files.
  frel_form <- Allsims.df  %>%
    distinct(Frel, formulation) %>%
    filter(formulation %in% c(formula))
  
  
  ## file for each analyst(based on his machine's username)
  path <- paste0("./", username, "")
  
  ## folder for each scenario based on the name 
  ## of model, number of subjects, Frel, and sampling scenario
  foldername <- paste0(path,"/",paste0(model,
                                       ", N=", Nsubjects,
                                       ",Frel=", frel_form["Frel"],
                                       ",Sampling=",sampling,
                                       ",NBOOT=", NBOOT))
  if (file.exists(path)) {
    if (file.exists(foldername)) {
      print("scenario folder is exist")
    } else {
      print("scenario folder is created")
      dir.create(foldername)
    }
  } else{
    print("A new folder for analyst has been created, 
          and also scenario folder has been created")
    dir.create(path)
    dir.create(foldername)
  }
  
  scenario1 <- Allsims.df %>%
    filter(formulation %in% c(0, formula)) %>%
    filter(time %in% time_points) %>% # select the timepoints for this scenario
    select(id, amt, time, C, CObs, formulation, Frel) %>% #keep needed columns
    filter(!(time == 0 & is.na(amt)))# remove duplicate time 0
  scenario1$CObs <- ifelse(scenario1$time == 0, 0, scenario1$CObs)
  scenario1$CObs <-ifelse(scenario1$CObs<0, 0, scenario1$CObs)
  
  
  if (sparse) {
    uniqueids <- scenario1 %>%
      group_by(formulation) %>%
      summarize(id = unique(id)) %>%
      group_by(formulation) %>%
      mutate(nid = row_number()) %>%
      mutate(grp = ifelse(nid <= 500, "grp1", "grp2"))
    scenario1 <- left_join(scenario1, uniqueids %>% select(-nid))
    
    ## exclude time_points_grp2 from group 1 and
    ## exclude time_points_grp1 from group 2.
    scenario1 <- scenario1 %>%
      mutate(timeflag = ifelse(grp == "grp1" &
                                 time %in% time_points_grp1, 1, 0)) %>%
      mutate(timeflag = ifelse(grp == "grp2" &
                                 time %in% time_points_grp2, 1, timeflag)) %>%
      filter(timeflag == 1)
    scenario1 <- scenario1 %>% select(-timeflag)
  }
  
  
  file_name <- paste0(path,"\\","N=", Nsubjects,
                      ",Frel=", frel_form["Frel"],
                      ",Sampling=",sampling,
                      ",NBOOT=", NBOOT,
                      ",bootdatadf.csv")
  # code to make simmodel work
  if (file.exists(file_name)) {
    bootdatadf <- fread(paste0(file_name))
    
    print("The bootstrap has been done before, and the file is exists..")
  } else{
    print("The first time to do bootstrap for this scenario..")
    
    ## bootsrapping for scenario1
    bootdatalist = list()
    for (i in (1:NBOOT)) {
      print(i)
      if (sparse) {
        bootdata <-
          as.data.frame(resample_df(
            df = scenario1,
            #' @param df data frame
            key_cols = c("id"),
            #' @param key_cols key columns to resample on
            strat_cols = c("formulation", "grp"),
            #' @param strat_cols columns to maintain proportion for stratification
            n = Nsubjects * 2 #' @param n number of unique sampled keys, defaults to match dataset
          )) %>%
          mutate(replicate = i)   #' create replicate variable as an indicator or index for each bootstrap dataset
        bootdatalist[[i]] <- bootdata
      }
      if (!sparse) {
        bootdata <-
          as.data.frame(resample_df(
            df = scenario1,
            key_cols = c("id"),
            strat_cols = c("formulation"),
            n = Nsubjects * 2
          )) %>%
          mutate(replicate = i)
        bootdatalist[[i]] <- bootdata
      }
    }
    
    ## convert the bootstrapp list to data frame type.
    bootdatadf <- rbindlist(bootdatalist)
    
    ## save the bootstrap data in the file of the analyst and labelled 
    ## with the parameters of the scenario (Nsubjects, Frel, and sampling scenario)
    fwrite(bootdatadf,paste0(file_name))  
  }
  
  
  
  #NCA analysis will run only if user chose model=="nca"
  
  ## NCA analysis will run only if user chose model=="nca"
  # ----
  if(model=="nca"){
    
    conc_obj <- bootdatadf  %>%
      PKNCAconc(CObs ~ time |
                  replicate + formulation + KEY,
                duration = 1.5,
                route = "intravascular")
    
    ## Dosing data needs to only have one row per dose, so subset for
    ## that first.
    ## Create a dosing object specifying the dose, time, and subject
    ## columns.  (Note that the grouping factors should be the same as or a
    ## subset of the grouping factors for concentration, and the grouping
    ## columns must have the same names between concentration and dose
    ## objects.)
    
    d_dose <- bootdatadf %>% filter(amt != 0 , time == 0) %>%
      PKNCAdose(amt ~ time |
                  replicate + formulation + KEY,
                duration = 1.5,
                route = "intravascular")
    # print(d_dose)
    
    ## Combine the concentration and dosing information both to
    ## automatically define the intervals for NCA calculation and provide
    ## doses for calculations requiring dose.
    d_intervals <-
      data.frame(
        start = 0,
        end = 2520,
        cmax = TRUE,
        auclast = TRUE,
        aucint.inf.obs = TRUE,
        ceoi = TRUE
      )
    
    
    o_data <- PKNCAdata(conc_obj, d_dose, intervals = d_intervals)
    
    # print(o_data)
    
    ## Calculate the NCA parameters
    resultsList <- pk.nca(o_data)
    P = as.data.frame(resultsList) %>%
      # filter(PPTESTCD == "auclast" | PPTESTCD =="ceoi") %>%
      spread(PPTESTCD, PPORRES) %>%
      rename(
        AUC = auclast,
        CMAX = ceoi,
        CMAX2 = cmax,
        FORM = formulation
      )
    
    # print(P, table for all NCA parameters)
    fwrite(P, file= paste0(foldername,"/",
                           "Nsubjects = ",Nsubjects, "formula= ", formula, "sampling = ",
                           sampling," PKNCA parameters.csv"))
    # print(P)
    # print(kable(P))
    
    
    # bioequivalence assessment (TOST) for each replicate by nesting technique
    table1all <- P %>%
      group_by(replicate) %>%
      nest() %>%
      mutate(BEresults = map(data, ~ rbind(
        BE.Parallel(.x, parameter = "AUC"),
        BE.Parallel(.x, parameter = "CMAX")
      )))
    # print(table1all)
    
    # unnesting the results of TOST
    table1allBE <- table1all %>%
      unnest(cols = c(BEresults), names_repair = "universal")
    
    # print(table1allBE)
    
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
    # print(table1allBEres)
    
    fwrite(table1allBEres,file= paste0(foldername,"/",
                                       "Nsubjects = ",Nsubjects, "formula= ", formula, "sampling = ",
                                       sampling," NCA BE table1allBEres.csv"))
    
    
    
    
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
        formulation = formula,
        Sampling = sampling
      )
    # print(powerresults)
    fwrite(
      powerresults,
      file = paste0(
        foldername,
        "/",
        "Nsubjects = ",
        Nsubjects,
        "formula= ",
        formula,
        "sampling = ",
        sampling,
        "NCA BE powerresults.csv"
      )
    )
    
    
    ## convert the power results to tidydata for plotting and a
    plotdata <- gather(powerresults,
                       "BEmeasurement",
                       "Power",
                       -Ntotal,-Narm,-formulation,-Sampling)  %>%
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
        
        sampling_No = bootdatadf %>% 
          filter(replicate == 1, KEY == 1) %>% nrow(),
        sampling = 
        if_else(
          sampling_No == 5,
          "sparse",
          if_else(
            sampling_No == 9,
            "moderate",
            if_else(sampling_No == 13, "rich",
                    
                    "wrong")
          )
        ),
        model = model
      )
    
    fwrite(plotdata, file = paste0(foldername,"/",
                                   "Nsubjects = ",Nsubjects, "formula= ", formula, "sampling = ",
                                   sampling,"NCA BE plotdata.csv"))
    
    Congratz <-
      paste0("You finished Nsubjects = ", i, "formula= ", j, "sampling = ", k)
    print(Congratz)
    
    output <- plotdata
    
  }
  
  ## MBBE analysis will run only if user chose model any thing other than nca
  
  #----
  
  if(model!="nca"){
    
    
    ## reading the model text file from the master folder for models
    ## and then save it in the working directory to be used by RsNLME.
    if (!runninglocal) {
      x = read_lines(paste0("X:/APT fellowship/PkPML/", model, ".txt"))
    }
    if (runninglocal) {
      x = read_lines(paste0(model, ".txt"))
    }
    
    write_lines(x, paste0(path, "\\", model, ".txt"))
    # RsNLME Hosts:
    # Set of instructions that define NLME engine configuration.
    # multicore = parallelization by model. 
    
    multicoreHost <-  NlmeParallelHost(
      sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
      installationDirectory = Sys.getenv("INSTALLDIR"),
      parallelMethod = NlmeParallelMethod("multicore"),
      hostName = "multicore",
      numCores = numCores
    ) ## argument in the automated function, numCores is based on which machine (VM or local machine)
    
    
    ## Create model Using truemodel text modelfile
    bootmodelall <- textualmodel(
      modelName = "bootmodelall",
      data = bootdatadf,
      mdl = paste0(path, "\\", model, ".txt"),
      workingDir = foldername
    )
    
    ## Mapping the model variable to dataset variables
    
    bootmodelall  <- colMapping(bootmodelall,
                                mappings = c(id = "KEY",
                                             FORM = "formulation",
                                             A1 = "amt"))
    
    ## Defined a new table called table01 that contain:
    ## timpoints at 1.5 and 2520
    ## variables of formulation, concentration (for Cmax calculation), CUMAUC (for AUC calculation)
    
    bootmodelall <- addExtraDef(
      bootmodelall ,
      c(
        "table(file=\"table01.csv\",time(1.5,2520), covr(FORM),C,CUMAUC)"
      )
    )
    
    paramspd <-
      engineParams(
        bootmodelall,
        numIterations = numIterations,
        #Specifying maximum number of iterations for estimation.
        method = RSNLMEENGINE,
        ## based on the automated function, Specifying engine method for estimation. #"FOCE-ELS"
        #ODE = "DVERK", # ODE solver
        maxStepsODE = maxStepsODE,
        #maximum number of allowable steps or function evaluations for the ODE solver.
        stdErr = "None"
      ) #Specifying method for standard error computations
    
    tic()
    sortedfitmodel <-  sortfit(
      bootmodelall,
      hostPlatform = multicoreHost,
      params = paramspd,
      sortColumns = SortColumns("replicate"),
      # sort based on replicate variable, where each bootstrap dataset has a unique replicate
      workingDir = foldername
    )
    
    
    toc() # fitting time only
    return(sortedfitmodel)
  }
  
}
