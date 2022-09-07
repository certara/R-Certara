# This script captures topics presented and discussed at Lesson 5 Office Hours

# Question:  > I notice near line 320 you have prmnlme which gives SE and RSE. Can you output StdDev and CV%, i.e. RStdDev

# xpdb %>% get_prmNlme()
# # A tibble: 7 × 12
# type  name       label   value     se    rse fixed diagonal     m     n `2.5% CI` `97.5% CI`
# <chr> <chr>      <chr>   <dbl>  <dbl>  <dbl> <lgl> <lgl>    <int> <int>     <dbl>      <dbl>
#   1 the   THETA(1)   tvKa   1.20   0.0394 0.0329 FALSE NA           1    NA    1.12        1.27
# 2 the   THETA(2)   tvV   80.4    2.83   0.0352 FALSE NA           2    NA   74.9        86.0
# 3 the   THETA(3)   tvCl   7.73   0.425  0.0550 FALSE NA           3    NA    6.90        8.56
# 4 sig   SIGMA(1,1) CEps  13.8    0.775  0.0562 FALSE TRUE         1     1   12.3        15.3
# 5 ome   OMEGA(1,1) nKa    0.0719 0.0155 0.215  FALSE TRUE         1     1    0.0415      0.102
# 6 ome   OMEGA(2,2) nV     0.169  0.0233 0.138  FALSE TRUE         2     2    0.123       0.214
# 7 ome   OMEGA(3,3) nCl    0.395  0.0471 0.119  FALSE TRUE         3     3    0.303       0.488

# Answer:  The Stderr in this table is the standard error of the parameter estimate,
# not an indication of variability of e.g., Cl across subjects.    It’s a measure of
# how well the data inform that parameter estimate. So in this example, tvCl was
# estimated as 7.73, but the program is saying it could be 5.3% higher or lower.

# If one is interested in a mean/sd of the parameter estimate across subjects,
# one could use the posthoc output which gives the individual parameter estimates
# (The tvCl plus the individuals estimated ETA), and do a mean/sd calculation:

# Get the posthoc output by your method of choice:

posthoc <- read.csv("./basemod/posthoc.csv")     #read csv directly

# or...

source("./basemod/dmp.txt")                      #source the dmp.txt file and index
posthoc <- dmp.txt$posthoc

#or...

xpdb <- xposeNlme(basemod@modelInfo@workingDir)  #create the xpose object and index
posthoc <- xpdb$data$data[[1]]

posthoc %>%
  select(ID5, Ka, V, Cl) %>%     #keep only Ka V and Cl
  distinct(ID5,.keep_all = TRUE)  %>%              #only want 1 row per individual - selects distinct rows
  summarise(meanCl = mean(Cl),  #use summarise from
            sdCl = sd(Cl),
            #cvCl = 100*sd(Cl)/mean(Cl),             #calc %CV
            #cvCl = round(100*sd(Cl)/mean(Cl),1),    #round for display
            #cvCl = sigfig(100*sd(Cl)/mean(Cl),3),   #significant digits for display (see sigfig function below)
            meanV = mean(V),
            sdV = sd(V),
            meanKa = mean(Ka),
            sdKa = sd(Ka)) %>%
  flextable()

View(xpdb)
# I like gtsummary for this type of thing:  https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html
# Note gtsummary doesn't include %cv as an available statistic though
library(gtsummary)
posthoc %>%
  select(ID5, Ka, V, Cl) %>%     #keep only Ka V and Cl
  distinct(ID5,.keep_all = TRUE) %>%
  select(Ka, V, Cl) %>%
  tbl_summary(statistic = all_continuous() ~ c("{mean} ({sd})"))



#simple function for displaying significant figures
sigfig <- function(vec, n=3){
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig
  # output:  outvec    vector of numeric rounded to N sigfig

  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#")

}      # end of function   sigfig






