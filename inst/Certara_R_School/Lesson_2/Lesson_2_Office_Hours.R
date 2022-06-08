# QUESTION: How to plot probability of a PK metric ----
#e.g., Cmax of reaching a certain threshold x mg/L,
# by dosing regimen, i.e. 1 line per dosing regimen

library(dplyr)
library(tidyr)
library(DescTools)
library(ggplot2)
library(egg)


# * Calculate Cmax and AUC24 ----
# We use AUC function from DescTools package for this (simple) example
# Note that our dataset uses nominal time 0-24 h for all subjects with
# no missing data!  Take care when calculating NCA parms in R
data <- finaldat %>%
  group_by(ID, DOSEGRP) %>%
  summarise(Cmax = max(CONC),
                   AUC24 = AUC(TIME, CONC)) %>%
  arrange(as.numeric(ID))

# * Define Thresholds ----
targetCmax <- 100 # enter the threshold you'd like to use
targetAUC24 <- 750 # enter the threshold you'd like to use


# * Calculate % of values exceeding threshold by dose group ----
targetdat <- data %>%
  group_by(DOSEGRP) %>%
  summarise(OverAUC24 = length(which(AUC24 > targetAUC24)) / length(AUC24) * 100,
            OverCmax = length(which(Cmax > targetCmax)) / length(Cmax) * 100)

# * Create the plots ----
TAplotdat <- data %>%
  pivot_longer(cols = c(Cmax, AUC24), names_to = "Parameter") %>%
  mutate(target = ifelse(Parameter == "AUC24", targetAUC24, targetCmax))

TAplot <- ggplot(TAplotdat, aes(x = value, fill = Parameter)) +
  labs(x = "Parameter", y = "Count", title = "Histograms for AUC24 and Cmax Target Attainment") +
  geom_histogram(col = "black") + # create the histogram
  geom_vline(aes(xintercept = target), lty = 2, lwd = 1) + # add a vertical line with our target
  facet_wrap(vars(DOSEGRP, Parameter), scales = "free", nrow = 3, ncol = 2) # wrap by Parameter (AUC and Cmax)



# we can use tag_facet from the egg package to add some custom text to the plots

TAplot <- tag_facet(TAplot,
                         x = Inf, y = Inf,
                         open = "",
                         close = "",
                         tag_pool = c(
                           paste("%OverTarget = ", targetdat$OverAUC24[which(targetdat$DOSEGRP==5000)]),
                           paste("%OverTarget = ", targetdat$OverCmax[which(targetdat$DOSEGRP==5000)]),
                           paste("%OverTarget = ", targetdat$OverAUC24[which(targetdat$DOSEGRP==10000)]),
                           paste("%OverTarget = ", targetdat$OverCmax[which(targetdat$DOSEGRP==10000)]),
                           paste("%OverTarget = ", targetdat$OverAUC24[which(targetdat$DOSEGRP==20000)]),
                           paste("%OverTarget = ", targetdat$OverCmax[which(targetdat$DOSEGRP==20000)])
                           ),
                         hjust = 1.1,
                         vjust = 1.5
) +
  theme(strip.text = element_text(), strip.background = element_rect())



# QUESTION: Using lubridate to calculate time after dose ----

# NOTE:  PML will accept a variety of datetime formats directly

# * Reading datetime data ----
# First step is making sure R understands your string as a datetime:
# May be necessary to do some column concatenation, text manipulation, etc.
# Be mindful of missing data!!  -- OK to make assumptions, substitutions,
# YOU are the analyst, but document, document (here in script and Report)!!
# Be sure you can describe/reproduce exactly what you did. -
# Assumptions are debatable, reproducibility is not

library(lubridate)

dosetimes <- tibble(ID=1:5,AMT=100,
                    DTIME=c("2021-03-02T08:05",
                            "March 2, 2021 8:10AM",
                            "02-MAR-21 08:15",
                            "3/2/2021 8:20",
                            "20210302 08:20:00"))

# functions for parsing strings into datetimes


ymd(dosetimes$DTIME)
ymd_hm(dosetimes$DTIME)
ymd_hms(dosetimes$DTIME)
mdy(dosetimes$DTIME)
mdy_hm(dosetimes$DTIME)
mdy_hms(dosetimes$DTIME)
dmy(dosetimes$DTIME)
dmy_hm(dosetimes$DTIME)
dmy_hms(dosetimes$DTIME)



# * Timezone? ----
today()
now()
now(tz="UTC") #Coordinated Universal Time (all timezones are defined by offset from UTC)

now()-now(tz="UTC")  #note that timezone is adjusted in calculation

now(tz="MST") #mountain standard time
now(tz="Japan")
# mind your timezones when merging dose and sample times, working with big multicenter datasets

OlsonNames() # Returns a list of valid time zone names. OlsonNames()
Sys.timezone() #Gets current time zone.

# Using lubridate to calculate time after dose
# * Create example dose file with 2 doses for each ID ----
# let's create a tibble with multiple doses in 2 subjects
# we will use the common SAS xpt date format

dosetimes <- tibble(ID=sort(rep(1:2,2)),
                    AMT=100,
                    DTIME=c("2021-03-02T08:00",
                            "2021-03-03T08:00",
                            "2021-03-02T08:05",
                            "2021-03-03T08:05"))
dosetimes <- dosetimes %>%
  mutate(DATETIME=ymd_hm(DTIME))

dosetimes$DATETIME

# * Slice out the first dose for each ID for use as time=0 reference ----
firstdose <- dosetimes %>%
  group_by(ID) %>%
  slice(1) %>%
  select(ID,DATETIME) %>%
  rename(REFTIME=DATETIME)



# * Create example observations file ----
# now let's create an observations tibble
obstimes <- tibble(ID=sort(rep(1:2,10)),

                   STIME=c("2021-03-02T08:15",     #.25h (subject 1)
                           "2021-03-02T08:31",     #.5h
                           "2021-03-02T09:00",     #1h
                           "2021-03-02T10:02",     #2h
                           "2021-03-02T12:00",     #4h
                           "2021-03-02T16:06",     #8h
                           "2021-03-02T20:00",     #12h
                           "2021-03-03T07:59",     #24h
                           "2021-03-03T19:55",     #36h (12h TAD)
                           "2021-03-04T07:55",     #48h (24h TAD)

                           "2021-03-02T08:15",     #.25h (subject 2)
                           "2021-03-02T08:33",     #.5h
                           "2021-03-02T09:02",     #1h
                           "2021-03-02T10:00",     #2h
                           "2021-03-02T12:05",     #4h
                           "2021-03-02T15:58",     #8h
                           "2021-03-02T20:04",     #12h
                           "2021-03-03T08:03",     #24h
                           "2021-03-03T20:05",     #36h (12h TAD)
                           "2021-03-04T08:08"),    #48h (24h TAD)

                     DV=c((100*2)/(25*(2-0.15))*
                     (exp(-.15*c(.25,.5,1,2,4,8,12,24,36,48))-    #1 cpt oral eqn
                        exp(-2*c(.25,.5,1,2,4,8,12,24,36,48))),

                          (100*2.4)/(25*(2.4-0.09))*
                       (exp(-.09*c(.25,.5,1,2,4,8,12,24,36,48))-
                          exp(-2.4*c(.25,.5,1,2,4,8,12,24,36,48)))))

obstimes <- obstimes %>%
  mutate(DATETIME=ymd_hm(STIME)) %>%
  mutate(DV=signif(DV,3))

obstimes %>%
  ggplot(aes(DATETIME,DV,group=ID)) +
           geom_line()

# * full_join dosetimes and obstimes ----
pkdata <- full_join(dosetimes, obstimes, by=c("ID","DATETIME")) %>%
  arrange(ID,DATETIME) %>%
  select(ID,DATETIME,AMT,DV)

# * left_join to bring firstdose reftime in ----
pkdata <- left_join(pkdata,firstdose,by="ID")

# * calculate hours from first dose ----

pkdata <- pkdata %>%
mutate(TIME = interval(start = REFTIME, end = DATETIME)) %>%    #gives an interval
mutate(TIME = as.duration(interval(start = REFTIME, end = DATETIME))) %>%  #gives you duration (in seconds)
mutate(TIME = as.duration(interval(start = REFTIME, end = DATETIME))/dhours(1))  #gives you hrs

# * Calculate Time After Dose (TAD) ----

pkdata <- pkdata %>%
  mutate(index = cumsum(c(FALSE, as.logical(diff(if_else(is.na(AMT), 0, AMT))))))   %>%
# mutate(index = cumsum(c(FALSE, as.logical(diff(if_else(is.na(pkdata$AMT), 0, pkdata$AMT))))))
  group_by(ID) %>%
  mutate(tmp = c(0,diff(TIME))* is.na(AMT)) %>%
# mutate(tmp = c(0,diff(pkdata$TIME))* is.na(pkdata$AMT))
  group_by(index) %>%
  mutate(TAD = cumsum(tmp)) %>%
  ungroup() %>%
  select(ID,TIME,TAD,AMT,DV)

pkdata <- pkdata %>%
  mutate(TIME=round(TIME,2), TAD=round(TAD,2))


#Install ggquickeda before next class!!

