# Overview ----
# This is code for Certara.R School Lesson 2, R basics for PMX
# In this lesson we create an NLME dataset from source SDTM datasets
# in xpt file format, and generate some basic summary table and plot displays

# 0: Import libraries

library(haven) #Useful for import of SAS files
library(dplyr) #Most popular package for wrangling data in R
library(tidyr) #useful data manipulation and reshaping functions that work with dplyr
library(gtsummary) #useful for quick data summary tables
library(flextable) # useful table packge supporting critical file output types
library(lubridate) #useful for working with date-times
library(ggplot2) #useful plotting package

# 1: Import Data ----
# * 1.1 Read in xpt datasets ----
dm<-read_xpt("DM.xpt")
ex<-read_xpt("EX.xpt")
pc<-read_xpt("PC.xpt")
vs<-read_xpt("VS.xpt")

# * 1.2 Inspect data ----
# * * 1.2.1 Covariate Data Overview ----
# Use the glimpse function from dplyr to inspect the data, including column type
glimpse(dm)
# Check unique values of RACE for recoding
table(dm$RACE)


# 2. Dataset preparation ----
# * 2.1 Demographic data ----
# * * 2.1.1 DM ----
#Prepare dataframe with desired covariates from dm and vs
demo <- dm %>%
  select(USUBJID,SUBJID,AGE,SEX,RACE,ETHNIC) %>%  #Select desired columns
  mutate(RACEOR=RACE) %>% #Create race original column (RACEOR)
  mutate(RACE=ifelse(RACEOR=="BLACK OR AFRICAN AMERICAN","BLACK",RACE)) %>% #Recode RACE variable
  mutate(RACE=ifelse(RACEOR=="NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER","OTHER",RACE)) %>%
  mutate(RACE=ifelse(RACEOR=="AMERICAN INDIAN OR ALASKAN NATIVE","OTHER",RACE)) %>%
  select(USUBJID,SUBJID,AGE,SEX,RACE) #keep only columns needed

# * * * 2.1.1.1 Summary ----
#prepare a quick summary of demo data to check n's, etc.
demo %>%
  select(AGE,SEX,RACE) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({min} - {max})") %>%
  add_n()

# * * 2.1.2 VS ----
# Get subject weights from vs dataset
wtdat <- vs %>%
  filter(VISIT=="WEEK 1") %>%
  select(USUBJID,VSSTRESN) %>%
  rename(WT=VSSTRESN)

# * * * 2.1.2.1 Summary ----
#quick summary
wtdat %>%
  select(WT) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({min} - {max})") %>%
  add_n()

# * * * 2.1.2.1 Data Correction ----
#NOTE: On review of vs summary we note that subject 137 does not have a WEEK 1 value for WT
#We need to substitute the SCREENING value for this subject

wtdat <- vs %>%
  select(USUBJID,VSSTRESN,VISIT) %>%
  pivot_wider(names_from = VISIT, values_from = VSSTRESN) %>%  #pivot_wider splits a column into mult columns
  mutate(WT=ifelse(is.na(`WEEK 1`), SCREENING,`WEEK 1`)) %>%  #here we replace NA in Week 1 with screening value
  select(USUBJID, WT)

# * 2.2 Join Demographic Data ----

#join demo with wtdat
demo <- demo %>%
  left_join(wtdat, by="USUBJID") %>%
  rename(ID=SUBJID)

# * * 2.2.1 Summary ----
#quick summary
demo %>%
  select(AGE,SEX,RACE,WT) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({min} - {max})") %>%
  add_n()

# * 2.3 Dosing Data ----
dosing <- ex %>%
  select(USUBJID,EXDOSE,EXSTDTC)

# * 2.4 Observation Data ----
pk <- pc %>%
  select(USUBJID,PCSTRESN,PCTPT,PCDTC,PCRFTDTC)

# * 2.5 Join Dosing and Observation Data ----
pkdose <- pk %>%
  left_join(dosing, by="USUBJID")

#check to see if reference dose time in PC matches EX
identical(pkdose$PCRFTDTC,pkdose$EXSTDTC) # it does, we can use either for time calculations

# * 2.6 Calculate relative times from datetime ----
# we use the lubridate package from the tidyverse
# The %--% operator is shortand syntax for the interval() function
# For details enter: ?lubridate::`%--%

pkdose <- pkdose %>%
  mutate(TIME=as.duration(ymd_hm(EXSTDTC) %--% ymd_hm(PCDTC))/dhours(1)) %>%
  select(USUBJID,PCSTRESN,EXDOSE,TIME) %>%
  rename(CONC=PCSTRESN,DOSEGRP=EXDOSE)


# * 2.7 Join Dose and Concentration Data with Demography  ----
finaldat <- pkdose %>% left_join(demo,by="USUBJID") %>%
  mutate(AMT=ifelse(TIME<0,DOSEGRP,0),TIME=ifelse(TIME<0,0,TIME)) %>%
  mutate(CONC=ifelse(is.na(CONC),0,CONC)) %>%
  select(ID, TIME, AMT, CONC, AGE, WT, SEX, RACE, DOSEGRP)

# 3. Descriptive Statistics Tables ----

# * 3.1 Demographic summary table using gt ----
dm_table <- finaldat %>%
  select(AGE,SEX,RACE,WT) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({min} - {max})") %>%
  add_n() %>%
  as_flex_table() %>%
  bold(part="header")


# * 3.2 Concentration summary by time and dose group using flextable ----
# gtsummary allows only one grouping column, we'll use dplyr for more
# complex data summary operations, then convert summary data.frame to a flextable

mean_conc_dosegrp_time_tbl <- finaldat %>%
  select(TIME, CONC, DOSEGRP) %>%
  filter(TIME > 0) %>%
  group_by(DOSEGRP, TIME) %>%
  summarise(CONC_mean = round(mean(CONC), 2),
            CONC_min = round(min(CONC), 2),
            CONC_max = round(max(CONC), 2)) %>%
  mutate(CONC_combined_stat = paste0(CONC_mean, " (", CONC_min, "-", CONC_max, ")")) %>%
  select(DOSEGRP, TIME, CONC_combined_stat) %>%
  pivot_wider(names_from = "DOSEGRP", values_from = "CONC_combined_stat") %>%
  flextable() %>%
  set_header_labels(`5000` = "5,000/mg",
                    `10000` = "10,000/mg",
                    `20000` = "20,000/mg"
  ) %>%
  add_header_row(colwidths = c(1,3), values = c("", "Dose Group")) %>%
  set_caption("Mean Concentration by Dose Group and Time", ) %>%
  align(align = "center", part = "all") %>%
  bold(part="header")


# * 3.3 Save Tables ----
# To save table as pdf or image, webshot and phantom_js are required
if(!require(webshot)){
  install.packages("webshot")
}
webshot::install_phantomjs()

# * * 3.3.1 Word ----
save_as_docx(dm_table, mean_conc_dosegrp_time_tbl, path = "tables.docx")
# * * 3.3.2 PDF ----
save_as_image(dm_table, mean_conc_dosegrp_time_tbl, path = "tables.pdf", zoom = 1)
# * * 3.3.3 PNG ----
save_as_image(dm_table, mean_conc_dosegrp_time_tbl, path = "tables.png")
# * * 3.3.4 HTML ----
save_as_html(dm_table, mean_conc_dosegrp_time_tbl,  path = "tables.html")
# * * 3.3.5 PowerPoint ----
save_as_pptx(dm_table, mean_conc_dosegrp_time_tbl,  path = "tables.pptx")



# 4. Exploratory Data Analysis ----

# * 4.1 Time-Concentration by Subject ----
p1 <- finaldat %>%
  mutate(ID = as.factor(ID)) %>%
  ggplot(aes(TIME,CONC,group=ID, color=ID)) +
  geom_line() +
  geom_point()

# * * 4.1.1 Interactive Visualization ----

if(!require(plotly)){
  install.packages("plotly")
}

plotly::ggplotly(p1)


# * 4.2 Time-Concentration by Subject Faceted by Dose Group ----
p2 <- p1 + facet_wrap(~DOSEGRP)

# * 4.3 Time-Concentration by Gender & Dose Group ----
p3 <- p2 + aes(color=SEX)

# * 4.4 Time-Concentration by Subject Faceted by Dose Group & Gender ----
p4 <- p1 + facet_wrap(DOSEGRP~SEX,ncol=2)

# * 4.5 Time-Concentration by Subject Faceted by Dose Group & Race ----
p5 <- p1 + facet_wrap(DOSEGRP~RACE) #facet synonymous with lattice

# * 4.6 Time-Concentration by Subject Faceted by Equal WT Intervals ----
p6 <- p1 + facet_wrap(~cut(WT,3))  #group into 3 equal "cuts" of WT

# * 4.6 Time-Concentration by Subject Faceted by Age Quantiles ----
p7 <- p1 + facet_wrap(~cut(AGE,quantile(AGE),include.lowest=TRUE)) #group into quartile "cuts"
