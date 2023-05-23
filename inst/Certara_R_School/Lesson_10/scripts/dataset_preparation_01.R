# Reference: Lesson 2

# 1: Import Data ----
# * 1.1 Read in xpt datasets ----
dm<-read_xpt("./data/DM.xpt")
ex<-read_xpt("./data/EX.xpt")
pc<-read_xpt("./data/PC.xpt")
vs<-read_xpt("./data/VS.xpt")


# 2. Dataset preparation ----
# * 2.1 Demographic data ----
# * * 2.1.1 DM ----
#Prepare 'demo' dataframe with desired covariates from dm and vs
demo <- dm %>%
  select(USUBJID,SUBJID,AGE,SEX,RACE) %>%  #Select desired columns
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

# * * * 2.1.2.2 Data Correction ----
#NOTE: On review of vs summary we note that subject 137 does not have a WEEK 1 value for WT
#wtdat %>% filter(is.na(WT))
#We need to substitute the SCREENING value for this subject

wtdat <- vs %>%
  select(USUBJID,VSSTRESN,VISIT) %>%
  pivot_wider(names_from = VISIT, values_from = VSSTRESN) %>%  #pivot_wider splits a column into mult columns
  mutate(WT=ifelse(is.na(`WEEK 1`), SCREENING,`WEEK 1`)) %>%  #here we replace NA in Week 1 with screening value
  select(USUBJID, WT)

# * 2.2 Join Demographic Data ----
#join demo with wtdat, using 'USUBJID' key
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

pkdose <- pkdose %>%
  mutate(TIME=as.duration(interval(start = ymd_hm(EXSTDTC), end = ymd_hm(PCDTC)))/dhours(1)) %>%
  select(USUBJID,PCSTRESN,EXDOSE,TIME) %>%
  rename(CONC=PCSTRESN,DOSEGRP=EXDOSE)


# * 2.7 Join Dose and Concentration Data with Demography  ----
finaldat <- pkdose %>% left_join(demo,by="USUBJID") %>%
  mutate(AMT=ifelse(TIME<0,DOSEGRP,0),TIME=ifelse(TIME<0,0,TIME)) %>%
  mutate(CONC=ifelse(is.na(CONC),0,CONC)) %>%
  select(ID, TIME, AMT, CONC, AGE, WT, SEX, RACE, DOSEGRP)



