# Overview ----
# This is code for Certara.R School Lesson 8, Visual Predictive Check with tidyvpc
# In Section 1, we will start by efficiently reproducing the model building steps we
# we performed to arrive at our final model (see Lesson 6).
# We will then perform a 'Visual Predictive Check' in RsNLME with the vpcmodel() function.
# Next we will use the tidyvpc package to explore various binning and stratification methods for our resulting VPC plot.
# Finally, we will demonstrate usage of the Certara.VPCResults Shiny app to easily parameterize the VPC
# from the GUI and generate tidyvpc / ggplot2 code for customization and reproducibility from command-line.

# 0: Load libraries, setup working directory, and import data

library(Certara.RsNLME) #RsNLME package
library(Certara.Xpose.NLME) #Model Diagnostics Package
library(xpose) #Model Diagnostics Package
library(ggplot2) #useful plotting package
library(dplyr) #data manipulation
library(tidyvpc) #vpc generation

setwd("./inst/Certara_R_School/Lesson_8/")

finaldat <- readRDS("../Lesson_6/finaldat.RDS") #Importing data from Lesson 6 using relative path

# 1. Base Model Setup ----
# * 1.1 Create Initial Model Object ----

basemod <- pkmodel(numCompartments = 1,
                   absorption = "FirstOrder",
                   data = finaldat,
                   columnMap = FALSE,           #map columns after model creation
                   modelName = "basemod")

# * 1.2 Map Dataset Columns to Model Variables ----
basemod <- basemod %>%
  colMapping(c(id = "ID", time = "TIME", Aa = "AMT", CObs = "CONC"))

# *RsNLME v1.2.0 Feature Preview*
# Using unquoted column names as alternative to above syntax
# basemod <- basemod %>%
#   colMapping(id = ID, time = TIME, Aa = AMT, CObs = CONC)

# * 1.3 Add Covariates to explore model diagnostics ----
basemod <- basemod %>%
  addCovariate(
    covariate = c("AGE"),
    type = "Continuous",
    center = "Value",
    centerValue = 45,
    effect = NULL
  ) %>%
  addCovariate(
    covariate = c("WT"),
    type = "Continuous",
    center = "Value",
    centerValue = 70,
    effect = NULL
  ) %>%
  addCovariate(
    covariate = c("SEX"),
    type = "Categorical",
    levels = c(0, 1),
    labels = c("M", "F"),
    effect = NULL
  ) %>%
  addCovariate(
    covariate = c("RACE"),
    type = "Categorical",
    levels = c(0, 1, 2, 3),
    labels = c("WHITE", "BLACK", "ASIAN", "OTHER"),
    effect = NULL
  ) %>%
  addCovariate(covariate = c("DOSEGRP"),
               type = "Continuous",
               effect = NULL)

# * 1.4 Fit base model ----
basemod_fit <- fitmodel(basemod)

# * 1.5 Explore Model Diagnostics ----
# Create xpdb diagnostics to show impact of covariate WT on CL
xpdb <- xposeNlme(dir = basemod@modelInfo@workingDir,
                  modelName = basemod@modelInfo@modelName)

# * 1.5.1 Evaluate CWRES vs WT
xpdb %>% res_vs_cov(covariate="WT",type='ps', scales="fixed") +
  geom_abline(slope=0)
xpdb %>% prm_vs_cov(covariate="WT",type='ps')

xpdb %>% eta_vs_cov(covariate="WT",type='ps', scales="fixed") +
  geom_abline(slope=0)

xpdb %>% eta_vs_cov(covariate="WT",type='ps', scales="fixed", guide_slope = 0)

# A note about documentation...
# A note about '...' e.g., unnamed arguments
# xpose can be extended with ggplot2 e.g., geom_point()
xpdb %>% res_vs_cov(covariate="WT",type='ps', scales="fixed") +
  geom_point(color = "red")
# But you can also pass unnamed arguments using the '...'
xpdb %>% res_vs_cov(covariate="WT",type='ps', scales="fixed", point_color = "red")

# More on ellipsis
# params <- engineParams(basemod, method = "FOCE-ELS")
# fitmodel(basemod, params = params)
# fitmodel(basemod, params = params, method = "Naive-Pooled")

# 2. VPC Base Model ----

# * 2.1 Create new VPC model by copying base model and accepting parameters estimates ----
basemod_vpc <- copyModel(basemod, acceptAllEffects = TRUE, modelName = "basemod_vpc")

# * 2.2 Setup VPC Params for run

# VPC Params Setup
vpcParams <- NlmeVpcParams(numReplicates = 50,
                           seed = 1234,
                           outputPRED = TRUE)

# * 2.3 Run VPC model
basemod_vpc_fit <- vpcmodel(model = basemod_vpc,
                            vpcParams = vpcParams)

# Usages of dots '...' to pass arguments from NlmeVpcParams, engineParams, hostParams using unnamed arguments e.g., '...' in the vpcmodel() function
# basemod_vpc_fit <- vpcmodel(
#                             model = basemod_vpc,
#                             numReplicates = 50,
#                             outputPRED = TRUE,
#                             method = "Naive-Pooled",
#                             parallelMethod = NlmeParallelMethod("LOCAL_MPI"),
#                             numCores = 4
#                             )


# 3 Using tidyvpc ----
browseURL("https://certara.github.io/tidyvpc/")
browseURL("https://certara.github.io/tidyvpc/articles/tidyvpc_cont.html")
# tidyvpc usage
# requires observed data e.g., obsdat
# requires simulated data e.g., simdat
# DV / TIME columns in the data e.g., IVAR and DV

# * 3.1 Setup ----
# * 3.1.1 Import Obs/Sim data
obsdat <- basemod_vpc_fit$predcheck0
simdat <- basemod_vpc_fit$predout
#Alternatively import from run folder
#obsdat <- read.csv(file.path(basemod_vpc@modelInfo@workingDir, "predcheck0.csv"))
#simdat <- read.csv(file.path(basemod_vpc@modelInfo@workingDir, "predout.csv"))

# Always remember ordering of data
obsdat <- obsdat %>%
  arrange(ID5, IVAR)

simdat <- simdat %>%
  arrange(REPLICATE, ID5, IVAR)

# ** 3.1.2 Required Functions ----

# ?observed
# ?simulated
# ?binning / ?binless
# ?vpcstats

# ** 3.1.3 Optional Functions ----
# ?predcorrect
# ?stratify
# ?censoring

# * 3.2 Basic Usage ----

#Binning on Variable in data e.g., TIME
vpc_1 <- observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  binning(bin = IVAR) %>%
  vpcstats()
plot(vpc_1)

# Binless
vpc_2 <- observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot(vpc_2)

# Comparison
egg::ggarrange(
  plot(vpc_1),
  plot(vpc_2, legend.position = 'none'), # remove legend in bottom plot
  labels = c("Binning", "Binless")
)
# Change prediction intervals, confidence level, quantile type
# Note: Phoenix used quantile type = 6, by default
observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  binning(bin = IVAR) %>%
  vpcstats(qpred = c(0.1, 0.5, 0.9), conf.level = 0.9, quantile.type = 6) %>%
  plot()

# Additional binning methods to select class intervals
# Class intervals are non-overlapping,
# and the classes are left-closed — see findInterval
# style can be one of  "jenks", "kmeans", "sd", "pretty", "pam", "kmeans", "hclust", "bclust", "fisher", and "dpih", 'box', 'headtails'
observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  binning(bin = "kmeans", nbins = 7) %>%
  plot()

# You can also plot() without calling vpcstats() function, e.g., plot bins only
observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  binning(bin = "pam", nbins = 7) %>%
  plot()

# Additional arguments required for 'centers' and 'breaks'
# Specify centers
observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  binning(bin = "centers", centers = c(1,2,3,4,6,10,17)) %>%
  plot()

# Specify breaks
observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  binning(bin = "breaks", breaks = c(2,4,6,8,10,16,18)) %>%
  plot()


# * 3.2.1 More on Binning ----

vpc <- observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  binning(bin = "kmeans", nbins = 5) %>%
  vpcstats()

bininfo(vpc)

obsdat %>%
  filter(IVAR >=0.25 & IVAR < 1.5) %>%
  pull(IVAR) %>%
  summary()

plot(vpc)


# What's inside the tidyvpcobj?
vpc <- observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  binning(bin = "kmeans", nbins = 7) %>%
  vpcstats()

plot(vpc)

glimpse(vpc)

# What is plotted?
# It is the xbin value that is plotted on x-axis
vpc$stats
# xbin value is the bin midpoint - but 'what midpoint is used'
bininfo(vpc)
# by default, xbin = 'xmedian'
vpc <- observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  binning(bin = "kmeans", nbins = 7, xbin = "xmean") %>%
  vpcstats()
# View below ggplot2 code to see values extracted from vpc object for plotting
ggplot(vpc$stats, aes(x = xbin)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = qname, col = qname, group = qname), alpha = 0.1, col = NA) +
  geom_line(aes(y = md, col = qname, group = qname)) +
  geom_line(aes(y = y, linetype = qname), size = 1) +
  geom_point(data = vpc$obs[!(blq | alq)], aes(x = x, y = y), color = "#757D8F", size = 3L, shape = 16, alpha = 0.5, show.legend = FALSE) +
  scale_colour_manual(name = "Simulated Percentiles\nMedian (lines) 95% CI (areas)", breaks = c("q0.05", "q0.5", "q0.95"), values = c("#D63636", "#3648D6", "#D63636"), labels = c("5%", "50%", "95%")) +
  scale_fill_manual(name = "Simulated Percentiles\nMedian (lines) 95% CI (areas)", breaks = c("q0.05", "q0.5", "q0.95"), values = c("#D63636", "#3648D6", "#D63636"), labels = c("5%", "50%", "95%")) +
  scale_linetype_manual(name = "Observed Percentiles", breaks = c("q0.05", "q0.5", "q0.95"), values = c("dashed", "solid", "dashed"), labels = c("5%", "50%", "95%")) +
  guides(fill = guide_legend(order = 2), colour = guide_legend(order = 2), linetype = guide_legend(order = 1)) +
  geom_rug(data = bininfo(vpc)[, .(x = sort(unique(c(xleft, xright))))], aes(x = x), sides = "t", size = 1) +
  ylab(sprintf("Observed/Simulated percentiles and associated %s%% CI", 100 * vpc$conf.level)) +
  xlab("TIME") +
  Certara.VPCResults::theme_certara() +
  theme(legend.position = "top")


# * 3.3 Stratification ----

# ** 3.3.1 Prepare Covariate Data ----
covdat <- finaldat %>%
  filter(TIME > 0) %>%
  mutate(ID = as.numeric(ID)) %>%
  mutate(WT_CUT = cut(WT, 3)) %>%
  select(ID, TIME, WT_CUT, SEX, RACE, DOSEGRP)

# ** 3.3.2 Merge Covariate Data into obsdat ----
obsdat <- obsdat %>%
  left_join(covdat, by = c("ID5" = "ID", "IVAR" = "TIME"))

# ** 3.3.3 Stratify on WT_CUT
# Model does not perform well for upper quantile of hi WT group
vpc_3 <- observed(obsdat, x = IVAR, yobs = DV) %>%
  simulated(simdat, ysim = DV) %>%
  stratify(~WT_CUT) %>%
  binning(bin = IVAR) %>%
  vpcstats()

plot(vpc_3)

# 4. Final Model Setup ----

# * 4.1 Copy base model and add covariate effect WT on Cl ----
finalmod <- copyModel(basemod, acceptAllEffects = TRUE, modelName = "finalmod") %>%
  addCovariate(covariate = "WT", effect = c("Cl"), center = "Value", centerValue = 70)

print(finalmod)

# * 4.2 Fit final covariate model
finalmod_fit <- fitmodel(finalmod)

# 5. VPC Final Model ----
# * 5.1 Prepare final VPC model by copying final model and accepting parameters estimates ----
finalmod_vpc <- copyModel(finalmod, acceptAllEffects = TRUE, modelName = "finalmod_vpc")

finalmod_vpc_fit <- vpcmodel(finalmod_vpc, numReplicates = 50)

obsfinaldat <- finalmod_vpc_fit$predcheck0
simfinaldat <- finalmod_vpc_fit$predout

# * 5.2 Join covariate data ----
obsfinaldat <- obsfinaldat %>%
  left_join(covdat, by = c("ID5" = "ID", "IVAR" = "TIME"))

# * Generate VPC - with stratification on WT_CUT
vpc_4 <- observed(obsfinaldat, x = IVAR, yobs = DV) %>%
  simulated(simfinaldat, ysim = DV) %>%
  stratify(~ WT_CUT) %>%
  binning(bin = IVAR) %>%
  vpcstats()

# tidyvpc supports n levels of stratification
# * Generate VPC - with stratification on WT ~ SEX
vpc_5 <- observed(obsfinaldat, x = IVAR, yobs = DV) %>%
  simulated(simfinaldat, ysim = DV) %>%
  stratify(SEX ~ WT_CUT) %>%
  binning(bin = IVAR) %>%
  vpcstats()

table(obsdat$WT_CUT, obsdat$SEX)

observed(obsfinaldat, x = IVAR, yobs = DV) %>%
  simulated(simfinaldat, ysim = DV) %>%
  stratify(SEX ~ DOSEGRP) %>%
  binning(bin = IVAR) %>%
  vpcstats()


# 6. Certara.VPCResults ----
# The Shiny GUI can be used as a learning heuristic and for easy customization of resulting ggplot
library(Certara.VPCResults)
#vpcResultsUI(obsfinaldat, simfinaldat)


# 7. Benchmarking tidyvpc ----
# Benchmark time against VPC package using 10,000 replicates
finalmod_vpc_fit_benchmark <- vpcmodel(finalmod_vpc, numReplicates = 10000)

obs_benchmark_data <- finalmod_vpc_fit_benchmark$predcheck0 %>%
  mutate(dv = DV,
         idv = IVAR,
         id = ID5
  ) %>%
  select(dv, idv, id)

sim_benchmark_data <- finalmod_vpc_fit_benchmark$predout %>%
  mutate(dv = DV,
         idv = IVAR,
         id = ID5
         ) %>%
  select(dv, idv, id)

library(vpc)
tictoc::tic()
vpc(sim = sim_benchmark_data, obs = obs_benchmark_data)
tictoc::toc()

library(tidyvpc)
tictoc::tic()
vpc <- observed(obs_benchmark_data, x = idv, yobs = dv) %>%
  simulated(sim_benchmark_data, ysim = dv) %>%
  binning(bin = idv) %>%
  vpcstats()
plot(vpc)
tictoc::toc()
