# Note, the code below assumes prior execution of LESSON 4,
# specifically the creation and fitting of the base model object
# ./Lesson_4/modelingwithRsNLME.R
# The basemod directory below was created by execution of the
# fitmodel command on line 243 in the Lesson 4 script


# LESSON 5 ----
# Evaluate Model Fit Diagnostics ----
# 1: Using Raw Data Outputs ----
# * 1.1 Convergence Data ----
library(ggplot2)
library(dplyr)

convergenceData<-read.csv("./basemod/ConvergenceData.csv")  #note use of relative path "." - saves typing full path

p1<-convergenceData %>% ggplot(aes(x=Iter,y=Value)) +
                    geom_point() +
                    geom_line() +
                    facet_wrap(~Parameter,ncol=2,scales='free')

lastdatapoint<- convergenceData %>%
  group_by(Parameter) %>%
  summarise(Iter=last(Iter),Value=last(Value))

p1

#xpdb %>% prm_vs_iteration()

#get fancy and add final values to plot
library(ggrepel) #use with ggplot to avoid text labels overlaying data
p1 + geom_label_repel(label = lastdatapoint$Value,
                     data=lastdatapoint,
                     size=3,
                     nudge_x=5,
                     nudge_y=c(100,.2,.05,.2))

# * 1.2 Covariance Matrix ----
#Simulation of parameter values based on covariance matrix

library(MASS)
VarCoVar <- read.csv("./basemod/VarCoVar.csv",
                     row.names=1)

n   <- 500
mu  <- c(1.19,80.4,7.73, # thetas (Ka, V, Cl)
         13.7, # sigma
         0.0719, # omegas (Ka)
         0,0.169,         #(V)
         0,0,0.395)       #(Cl)

sim <- mvrnorm (n, mu, VarCoVar)

library(tidyr)
as_tibble(sim[,1:3]) %>%

  pivot_longer(cols = everything(),
                names_to="Parameter",
                values_to = "Value") %>%
  ggplot(aes(x=Value,fill=Parameter)) +
  geom_histogram(alpha=0.5) +
  facet_wrap(~Parameter,scales="free")

library(GGally)
ggpairs(as_tibble(sim[,c('tvKa','tvV','tvCl')]))

# * 1.3 ETAs ----
etas <- read.csv("./basemod/eta.csv")

etas[,c("nCl","nKa","nV")] %>%
  pivot_longer(cols = everything(),
  names_to="Parameter",
  values_to = "Value") %>%

  ggplot(aes(x=Value,fill=Parameter)) +
  geom_histogram(alpha=0.5) +
  facet_wrap(~Parameter)

# * 1.4 ETAs with Covariates ----
etascovs <- read.csv("./basemod/etaCovariate.csv")

etascovs %>% ggplot(aes(x=Covr,y=Eta)) +
  geom_point() +
  facet_wrap(CovrName~EtaName,scale='free') +
  geom_hline(yintercept = 0) +
  geom_smooth(col='red',lty=2,se=FALSE)

etascovscat <- read.csv("./basemod/etaCovariatecat.csv")

etascovscat %>% ggplot(aes(y=Eta,x=as.factor(Covr))) +
  geom_boxplot() +
  facet_wrap(CovrName~EtaName,scale='free') +
  geom_hline(yintercept = 0)

# * 1.5 Omegas and Omega SE ----
omegas<-read.csv("./basemod/omega.csv")
omegas[,-c(1,2)] %>%
  flextable() %>%
  set_caption("Omega")

omegastderr<-read.csv("./basemod/omega_stderr.csv")
omegastderr[,-c(1,2)] %>%
  flextable() %>%
  set_caption("OmegaSE")

# * 1.6 Overall Summary
overall<-read.csv("./basemod/overall.csv")
overall %>%
  flextable() %>%
  set_caption("Fit Summary")

# * 1.7 Individual Posthoc Parameter Estimates ----
posthoc <- read.csv("./basemod/posthoc.csv")

p1<-posthoc %>%
  filter(time==0) %>%
  ggplot(aes(x=WT,y=Cl)) +
  geom_point() +
  geom_smooth(col='red',lty=2,se=FALSE)

p2<-posthoc %>%
  filter(time==0) %>%
  ggplot(aes(x=WT,y=V)) +
  geom_point() +
  geom_smooth(col='red',lty=2,se=FALSE)

p3<-posthoc %>%
  filter(time==0) %>%
  ggplot(aes(x=WT,y=Ka)) +
  geom_point() +
  geom_smooth(col='red',lty=2,se=FALSE)

p4<-posthoc %>%
  filter(time==0) %>%
  ggplot(aes(x=as.factor(SEX),y=Cl)) +
  geom_boxplot()

p5<-posthoc %>%
  filter(time==0) %>%
  ggplot(aes(x=as.factor(SEX),y=V)) +
  geom_boxplot()

p6<-posthoc %>%
  filter(time==0) %>%
  ggplot(aes(x=as.factor(SEX),y=Ka)) +
  geom_boxplot()

library(egg)
ggarrange(p1,p2,p3,p4,p5,p6,nrow=2)

# * 1.8 Residuals ----
resids <- read.csv("./basemod/residuals.csv")
p1 <- resids %>%
  ggplot(aes(x=DV,y=PRED)) +
  geom_point() +
  geom_abline() +
  scale_x_continuous(limits=c(0,max(max(resids$DV),max(resids$PRED)))) +
  scale_y_continuous(limits=c(0,max(max(resids$DV),max(resids$PRED))))

p2 <-resids %>%
  ggplot(aes(x=DV,y=IPRED)) +
  geom_point() +
  geom_abline() +
  scale_x_continuous(limits=c(0,max(max(resids$DV),max(resids$IPRED)))) +
  scale_y_continuous(limits=c(0,max(max(resids$DV),max(resids$IPRED))))

p3 <- resids %>%
  ggplot(aes(x=IVAR,y=CWRES)) +
  geom_point() +
  geom_abline(slope=0) +
  geom_smooth(col='red',lty=2,se=FALSE)

p4 <- resids %>%
  ggplot(aes(x=PRED,y=CWRES)) +
  geom_point() +
  geom_abline(slope=0) +
  geom_smooth(col='red',lty=2,se=FALSE)

library(egg)
ggarrange(p1,p2,p3,p4,nrow=2)

# * 1.9 Structural Parameters with Covariates ----
strparm <- read.csv("./basemod/strCovariate.csv")
strparm %>%
  ggplot(aes(x=Covr,y=Str)) +
  geom_point() +
  facet_wrap(CovrName~StrName,scales="free") +
  geom_smooth(col='red',lty=2,se=FALSE)

strparmcat <- read.csv("./basemod/strCovariatecat.csv")
strparmcat %>%
  ggplot(aes(x=as.factor(Covr),y=Str)) +
  geom_boxplot() +
  facet_wrap(CovrName~StrName,scales="free")

# * 1.10 Thetas ----
thetas<-read.csv("./basemod/theta.csv")
thetas[,-c(1,4,9)] %>%
  flextable() %>%
  set_caption("Theta")

# * 1.11 Theta Correlation ----
thetacorr<-read.csv("./basemod/thetacorrelation.csv")
thetacorr[,-1] %>%
  flextable() %>%
  set_caption("Theta Correlation")

# * 1.12 Theta Covariance ----
thetacov<-read.csv("./basemod/thetacovariance.csv")
thetacov[,-1] %>%
  flextable() %>%
  set_caption("Theta Covariance")

# * 1.13 Using the dmp.txt file ----
dmptxt<-source("./basemod/dmp.txt")
#Index elements of a list with $, e.g.,
dmptxt$value$residuals %>%
  ggplot(aes(x=DV,y=PRED)) +
  geom_point() +
  geom_abline() +
  scale_x_continuous(limits=c(0,max(max(dmptxt$value$residuals$DV),max(dmptxt$value$residuals$PRED)))) +
  scale_y_continuous(limits=c(0,max(max(dmptxt$value$residuals$DV),max(dmptxt$value$residuals$PRED))))

#Plot ETAs in Unit Space of Parameter
custdat <- dmptxt$value$posthoc
custdat <- custdat %>%
  mutate(tvKa=dmptxt$value$coefficients$fixed[1],
         tvV=dmptxt$value$coefficients$fixed[2],
         tvCl=dmptxt$value$coefficients$fixed[3]) %>%
  mutate(dlKa=Ka-tvKa, dlV=V-tvV, dlCl=Cl-tvCl)       #Subtract typical value from individual value

custdat %>%
  ggplot(aes(x=WT,y=dlKa)) +
  geom_point() +
  geom_abline(slope=0) +
  geom_smooth(col='red',lty=2,se=FALSE)

custdat %>%
  ggplot(aes(x=WT,y=dlV)) +
  geom_point() +
  geom_abline(slope=0) +
  geom_smooth(col='red',lty=2,se=FALSE)

custdat %>%
  ggplot(aes(x=WT,y=dlCl)) +
  geom_point() +
  geom_abline(slope=0) +
  geom_smooth(col='red',lty=2,se=FALSE)

# 2: Create Diagnostic Plots using xpose ----

# * 2.1 Create an xpose database object ----
library(Certara.Xpose.NLME)
library(xpose)

## xposeNlme imports results of an NLME run into xpose database to create commonly used diagnostic plots
xpdb <- xposeNlme(dir = "C:/Users/jcraig/Documents/GitHub/R-Certara/basemod")

## xpose objects are a special class of list
class(xpdb)

## Can list the variables within an xpose object
xpdb %>% list_vars()

# * 2.2 Common Diagnostic Plots and Options ----

# * * 2.2.1 DV vs PRED ----
#?dv_vs_pred for options
xpdb %>% dv_vs_pred()
xpdb %>% dv_vs_ipred()

xpdb %>% dv_vs_pred(type="ps")  #remove lines (type options are psl, points, smooth, line)
xpdb %>% dv_vs_pred(type="ps",
                    subtitle = "Ofv: @ofv, Method: @method") #use "template titles" see ?template_titles
xpdb %>% dv_vs_pred(type="ps",
                    subtitle = "Ofv: @ofv, Method: @method",
                    facets = 'SEX')  #facet by gender
xpdb %>% dv_vs_pred(type="ps",
                    subtitle = "Ofv: @ofv, Method: @method",
                    facets = SEX~cut(WT,3))
# reproducing above plot with ggplot2 syntax
xpdb %>% dv_vs_pred(type="ps",
                    subtitle = "Ofv: @ofv, Method: @method") +
  facet_grid(SEX~cut(WT,3), labeller = label_both)

# * * 2.2.2 Residual Plots ----
list_vars(xpdb) #To view all potential residuals that may be plotted
xpdb %>% res_vs_pred()  #residual vs predicted ?res_vs_pred
xpdb %>% res_vs_pred(aes(x=IPRED,y=CWRES)) #modify aesthetics to plot ipred
xpdb %>% res_vs_idv()  #res vs time
xpdb %>% absval_res_vs_pred() #absolute res vs pred

# * * 2.2.3 Individual Plots ----
#?ind_plots
xpdb %>% ind_plots(page=1,nrow=5, ncol=5)

# * * 2.2.4 Covariate Model ----
#?res_vs_cov
xpdb %>% res_vs_cov(covariate="WT",type='ps') +
  geom_abline(slope=0)

xpdb %>% res_vs_cov(covariate="SEX", type = "b") +
  geom_abline(slope=0)

xpdb %>% prm_vs_cov(covariate="WT",type='ps')

xpdb %>% eta_vs_cov(covariate="WT",type='ps') +
  geom_abline(slope=0)

# * * 2.2.5 Distributions ----
xpdb %>% cov_distrib()
xpdb %>% eta_distrib(scales="fixed")

# * * 2.2.6 QQ plots ----
xpdb %>% res_qq()
xpdb %>% eta_qq()

# Other plots available - explore on your own!
xpdb %>% prm_vs_iteration()


# * 2.3 Customizing xpose Plots with ggplot2 layers ----

xpdb %>% dv_vs_pred(type="ps") +
  scale_x_continuous(limits=c(0,400)) + #modify x axis limits to match y
  geom_abline(color='red') +
  geom_point(aes(x=IPRED,y=DV),color='blue',alpha=0.3)  #superimpose IPRED

# * 2.4 Indexing a list object directly ----
# This would be advanced use case, but point is that RsNLME output accessible in many ways
xpdb$code
xpdb$files$data[[1]]$tvCl

xpdb$files$data[[2]]$label
xpdb$files$data[[2]]$value

xpdb$files$data[[2]][,c('label','value')]

# However, it is much easier to use built in functions from Certara.Xpose.NLME, which returns a tibble.
xpdb %>% get_prmNlme()

xpdb %>% get_overallNlme()

# * 2.5 Manipulating xpose Data Objects ----

# * * 2.5.1 Filter

xpdb %>% filter(WT<60) %>% dv_vs_pred(title='WT<60 kg')  #model misses in low BW subjects


# * 2.6 Advanced Examples ----

## * * 2.6.1 highlight outliers example ----
#(the U{2264} is unicode for <=)

xpdb  %>%  mutate(CWRESflag = factor(abs(CWRES) > 2.5, labels=c("|CWRES| \U{2264} 2.5", "|CWRES| > 2.5"))) %>%
  res_vs_pred(type="ps",facets='SEX') +
  geom_point(aes(color = CWRESflag)) +
  scale_color_manual(values = c('black', 'red')) +
  scale_y_continuous(breaks = c(-2.5,0,2.5))

## * * 2.6.2 annotations ----
xpdb %>% dv_vs_ipred(type='ps') +
  geom_hline(yintercept=5, linetype="dashed") +
  geom_text(data=data.frame(x=Inf, y=5), aes(x=x, y=y),
            label="LLOQ = 5 ng/mL", hjust=1.1, vjust=-0.5, size=3) +
  theme_certara()


# 3: ggCertara Package ----
library(ggcertara)
# Use get_data function to extract data from xpdb object for
# use with ggcertara

gofData <- get_data(xpdb)

colnames(gofData) <- tolower(colnames(gofData)) #ggcertara expects lower case column names

gof(gofData)

#list of available panels
gof_list(gofData,all=TRUE)

gofPlots <- gof(gofData,panels=3:6)

# Use patchwork for customizing the multi-plot panels
library(patchwork)
gofPlots +
  plot_annotation(
    title = 'GOF Plots',
    subtitle = 'Goodness of fit plots for base model',
    caption = 'Generated with R!'
    )


#4: Certara Model Results Shiny Application ----
library(Certara.ModelResults)
resultsUI(basemod)

