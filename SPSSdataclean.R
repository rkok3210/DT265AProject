
# df2<-dfnf%>% filter(as.integer(dfnf$AGE2)>6) originally looking at adults as mental health and treatment variables for them are different but felt I was missing out on part of the story
# and also realised I could include a youth mental health indicator as well as an adult one.
# YSDSOVRL Len : 1 RC-YOUTH: MAX SEVERITY LEVEL OF MDE ROLE IMPAIRMENT
# ASDSOVL2 RC-ADULT: MAX SEVERITY LEVEL OF DEP FEELINGS ROLE IMPAIRMENT

#levels(dfs$IRSEX)
#labels(dfs$IRSEX) #row numbers
#levels(df$IRSEX) <- c("1M","2F")
# or levels(df$IRSEX)[2] <- 'F'    levels(df$IRSEX)[1] <- 'M'
# need to convert to zero and one for regression?

library(dplyr)
library(stringr)
library(dlookr)
library(survey)
library(sqldf)
library(expss)
library(lessR)
library(foreign)
library(plyr)
library(forcats)

options(warn=1) # shows import warnings - where trying to factorize non-factors  e.g. ALCYFU year of alcohol first use

# Read the SPSS data and factorize - 2668 variables
dfs<- read.spss('c:/Users/keogh/Desktop/Project/NSDUH-2017-DS0001-bndl-data-spss/NSDUH_2017.SAV',to.data.frame=TRUE, use.value.labels=TRUE)
# 278 errors but looking at the variables they were not in the final dataframe so did not need to be addressed

levels(dfs$AGE2)
summary(dfs$AGE2)

myvars <- c("QUESTID2","BOOKED","ABODALC","NEDHER","PNRNM30AL","PNRWYNORX","PNRWYGAMT","PNRWYOFTN","PNRWYLNGR","PNRWYOTWY","PNRNMLAS1","PNRRSMAIN","IRPNRANYREC","IRPNRNMREC","IROXCNNMYR","IRPNRNMINIT","IRPNRNMYFU","IRPNRNMAGE","TOBYR","TOBMON","ALCFLAG","ALCYR","ALCMON","MRJFLAG","MRJYR","MRJMON","COCFLAG","COCYR","COCMON","CRKFLAG","CRKYR","CRKMON","HERFLAG","HERYR","HERMON","HALLUCFLAG","HALLUCYR","HALLUCMON","INHALFLAG","INHALYR","INHALMON","METHAMFLAG","METHAMYR","METHAMMON","PNRANYFLAG","PNRANYYR","OXYCNANYYR","TRQANYFLAG","TRQANYYR","STMANYFLAG","STMANYYR","SEDANYFLAG","SEDANYYR","PSYANYFLAG","PSYANYYR","PNRNMFLAG","PNRNMYR","PNRNMMON","OXYCNNMYR","TRQNMFLAG","TRQNMYR","TRQNMMON","STMNMFLAG","STMNMYR","STMNMMON","SEDNMFLAG","SEDNMYR","SEDNMMON","PSYCHFLAG","PSYCHYR","PSYCHMON","OPINMYR","OPINMMON","HERPNRYR","ILLFLAG","ILLYR","ILLMON","MJONLYFLAG","MJONLYYR","MJONLYMON","ILLEMFLAG","ILLEMYR","ILLEMMON","CDUFLAG","DCIGMON","BNGDRKMON","HVYDRKMON","ILLANDALC","HYDCPDAPYU","ZOHYANYYR2","OXCOPDAPYU","TRAMPDAPYU","CODEPDAPYU","MORPPDAPYU","FENTPDAPYU","BUPRPDAPYU","OXYMPDAPYU","DEMEPDAPYU","HYDMPDAPYU","MTDNPDAPYU","PNROTANYR2","HYDCPDPYMU","OXCOPDPYMU","TRAMPDPYMU","CODEPDPYMU","MORPPDPYMU","FENTPDPYMU","BUPRPDPYMU","OXYMPDPYMU","DEMEPDPYMU","HYDMPDPYMU","MTDNPDPYMU","PNROTHPYMU2","PNRNDAYPM","FUPNRNM18","FUPNRNM21","PNRMAINRSN","SRCPNRNM2","SRCFRPNRNM","SRCCLFRPNR","GRSKMRJMON","GRSKMRJWK","GRSKCOCMON","GRSKCOCWK","GRSKHERTRY","GRSKHERWK","GRSKLSDTRY","GRSKLSDWK","GRSKBNGDLY","GRSKBNGWK","NDSSDNSP","DNICNSP","DEPNDALC","DEPNDMRJ","DEPNDCOC","DEPNDHER","DEPNDPYHAL","DEPNDPYINH","DEPNDPYMTH","DEPNDPYPNR","DEPNDPYTRQ","DEPNDPYSTM","DEPNDPYSED","DEPNDPYPSY","DEPNDPYILL","ABUSEALC","ABUSEMRJ","ABUSECOC","ABUSEHER","ABUSEPYHAL","ABUSEPYINH","ABUSEPYMTH","ABUSEPYPNR","ABUSEPYTRQ","ABUSEPYSTM","ABUSEPYSED","ABUSEPYPSY","ABUSEPYILL","ABUSEPYIEM","ABPYILLALC","ABPYILANAL","ABODMRJ","ABODCOC","ABODHER","UDPYHAL","UDPYINH","UDPYMTH","UDPYPNR","UDPYTRQ","UDPYSTM","UDPYSED","UDPYPSY","UDPYOPI","UDPYHRPNR","UDPYILL",
            "NOBOOKY2","BKMVTHFT","BKLARCNY","BKBURGL","HERSMOK2","BKSRVIOL","BKSMASLT","BKROB","BKARSON","BKDRVINF","BKDRUNK","BKPOSTOB","BKDRUG","BKSEXNR","BKFRAUD","BKOTH","BKOTHOF2","DRVINALCO2","DRVINMARJ2","DRVINDRG","DRVINDROTMJ","DRVINALDRG","PAROL","PROB","TXEVRRCVD2","TXYRILL","TXLTYPNRL2","NMERTMT2","INHOSPYR","HRTCONDEV","COPDEVER","CIRROSEVR","HEPBCEVER","KIDNYDSEV","HIVAIDSEV","CANCEREVR","CABLADDER","CABLOLEULYM","CAOTHER2","CABREAST","CACERVIX","CACOLNRECT","CAESOPSTOM","CAGALLIVPAN","CAKIDNEY","CALARYLUNG","CAMELANOM","CAMOUTTHRO","CAOVARY","CAPROSTEST","CASKINOTH","CASKINDK","CATHYROID","CAUTERUS","CANCERYR","HRTCONDYR","HIGHBPMED","SNYSELL","SNYSTOLE","SNYATTAK","SPDMON","SPDYR","MHSUITHK","MHSUTK_U","MHSUIPLN","MHSUITRY","SMIYR_U","AMIYR_U","SMMIYR_U","MMIYR_U","LMIYR_U","LMMIYRU","MI_CAT_U","SMISUDPY","AMISUDPY","LMMISUDPY","AMDELT","AMDEYR","AMDETXRX","AMDERXO2","AMDEIMP","SEXIDENT","IRSEX","IRMARIT","IREDUHIGHST2","CATAGE","CATAG2","CATAG3","CATAG6","CATAG7","SEXAGE","NEWRACE2","SEXRACE","EDUHIGHCAT","HEALTH2","WRKSICKMO","WRKSKIPMO","IRWRKSTAT","IRWRKSTAT18","EDFAM18","IMOTHER","IFATHER","IRHHSIZ2","IRKI17_2","IRMCDCHP","IRMEDICR","IRCHMPUS","IRPRVHLT","IROTHHLT","IRINSUR4","IRFAMSOC","IRPINC3","IRFAMIN3","GOVTPROG","INCOME","POVERTY3","COUTYP4","ANALWT_C","VESTR","VEREP","ASDSOVL2","HERSMOKE","HRSMKREC","HERSNIFF","HRSNFREC","HERNEEDL","HEOTSMK","HEOTSNF","HEOTNDL","HEOTOTH","HEOTSP","HRNDLREC","AGE2", "RSKHERTRY", "RSKHERWK","YSDSOVRL")

dfs<- dfs[myvars]# initially 56276 obs 308 variables

rm(myvars)

View(names(dfs))

dfs<-select (dfs,-c("PNRNM30AL","PNRWYNORX","PNRWYGAMT","PNRWYOFTN","PNRWYLNGR","PNRWYOTWY","PNRNMLAS1","PNRRSMAIN","IRPNRANYREC","IRPNRNMREC","IROXCNNMYR","IRPNRNMYFU","TOBYR","TOBMON","ALCFLAG","ALCYR","ALCMON","MRJFLAG","MRJMON","COCFLAG","COCMON","CRKFLAG","CRKMON","HALLUCFLAG","HALLUCMON","INHALFLAG","INHALMON","METHAMFLAG",
                    "METHAMMON","TRQANYFLAG","STMANYFLAG","SEDANYFLAG","PSYANYFLAG","TRQNMMON","STMNMMON","SEDNMMON","PSYCHMON","ILLMON","MJONLYFLAG","MJONLYYR","MJONLYMON","ILLEMFLAG","ILLEMYR","ILLEMMON","CDUFLAG","DCIGMON","GRSKMRJMON","GRSKMRJWK","GRSKCOCMON",
                    "GRSKCOCWK","GRSKLSDTRY","GRSKLSDWK","GRSKBNGDLY","GRSKBNGWK","DNICNSP","NOBOOKY2","BKMVTHFT","BKLARCNY","BKBURGL","BKSRVIOL","BKSMASLT","BKROB","BKARSON","BKDRVINF","BKDRUNK","BKPOSTOB","BKDRUG","BKSEXNR","BKFRAUD","BKOTH","BKOTHOF2","DRVINALCO2","DRVINMARJ2","DRVINDRG","DRVINDROTMJ","INHOSPYR","HRTCONDEV","COPDEVER","CIRROSEVR","KIDNYDSEV","CABLADDER","CABLOLEULYM","CAOTHER2","CABREAST","CACERVIX","CACOLNRECT","CAESOPSTOM","CAGALLIVPAN","CAKIDNEY","CALARYLUNG","CAMELANOM","CAMOUTTHRO","CAOVARY","CAPROSTEST","CASKINOTH",
                    "CASKINDK","CATHYROID","CAUTERUS","HRTCONDYR","HIGHBPMED","MHSUITHK","MHSUTK_U","MHSUIPLN","MHSUITRY","SMIYR_U","AMIYR_U","SMMIYR_U","MMIYR_U","LMIYR_U","LMMIYRU","IREDUHIGHST2","IMOTHER",
                    "IFATHER","IRHHSIZ2","IRKI17_2","IRFAMIN3","ABPYILLALC","ABPYILANAL","ABUSEPYIEM"))

dfs$YEAR<-2017 # thinking about adding 2015 and 2016 for trends and or bigger sample for training /testing
rm(nfs)
# checking for missing variables


colSums(is.na(dfs))

# dataframe of missing values with counts
missing<-dfs%>%
  diagnose() %>%
  select(-unique_count, -unique_rate) %>% 
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count))
View(missing)


sum(is.na(dfs))#498981
mean(is.na(dfs)) #0.0461806



# There are 24 columns with missing values. Analyse to see what to do with them#
# Health2 RC-OVERALL HEALTH RECODE there are 19 NAs recoded NAs as a factor Level
str(dfs$HEALTH2)
summary(dfs$HEALTH2)
nlevels(dfs$HEALTH2)
levels(dfs$HEALTH2)
dfs$HEALTH2<-fct_explicit_na(dfs$HEALTH2, "None-NA") #gives missing value an explicit factor level
table(dfs$HEALTH2)
class(dfs$DRVINALDRG)
levels(dfs$DRVINALDRG)
# AMDEIMP - past year MDE with severe impairment
dfs$AMDEIMP<-fct_explicit_na(dfs$AMDEIMP, "None-NA")
table(dfs$AMDEIMP)

# AMDELT Lifetime Major Depressive Episode #436
dfs$AMDELT<-fct_explicit_na(dfs$AMDELT, "None-NA")
table(dfs$AMDELT)

# AMDERXO2 RC-ADULT: RCVD RX MED, NO HLTH PROF FOR DEPRS FEELINGS IN PY # 35511
dfs$AMDERXO2<-fct_explicit_na(dfs$AMDERXO2, "None-NA")
table(dfs$AMDERXO2)

#AMDETXRXRC -ADULT: RCVD TRT/CNSLG OR RX MED FOR DEPRESS FEELING IN PY #35515
dfs$AMDETXRX<-fct_explicit_na(dfs$AMDETXRX, "None-NA")
table(dfs$AMDETXRX)

#AMDEYR RC-ADULT: PAST YEAR MAJOR DEPRESSIVE EPISODE (MDE) # 488 
# ASDSOVL2 RC-ADULT: MAX SEVERITY LEVEL OF DEP FEELINGS ROLE IMPAIRMENT # 38576
#AMISUDPY RC-AMI AND DRUG/ALCOHOL DEP OR ABUSE - PAST YEAR 13722
#DRVINALDRG Len : 1 RC-DROVE UNDER INFLUENCE OF ALC OR SELECTED ILL DRG IN PY 498
#DRVINDRG Len : 1 RC-DROVE UNDER INFLUENCE OF SELECTED ILL DRUGS IN PAST YR 387
#DRVINDROTMJ Len : 1 RC-DROVE UNDER INFLUENCE OF SELECTED ILL DRG OTH MRJ IN PY 320
#DRVINMARJ2 Len : 1 RC-DROVE UNDER INFLUENCE OF MARIJUANA IN PAST YEAR 143
#GRSKBNGDLY Len : 1 RC-GREAT RISK: HAVE 4-5 ALC DRINKS PER DAY 313
#GRSKBNGWK Len : 1 RC-GREAT RISK: HAVE 5+ ALC DRINKS 1-2 TIMES PER WEEK 319
#GRSKCOCMON Len : 1 RC-GREAT RISK: USE COCAINE ONCE A MONTH 617
#GRSKCOCWK Len : 1 RC-GREAT RISK: USE COCAINE 1-2 TIMES PER WEEK 591
#GRSKHERTRY Len : 1 RC-GREAT RISK: TRYING HEROIN ONCE OR TWICE 617
#GRSKHERWK Len : 1 RC-GREAT RISK: USE HEROIN 1-2 TIMES PER WEEK 586
#SRCFRPNRNM Len : 2 RC-SOURCE OF FRIEND/RELATIVE PAIN RELIEVER FOR LAST MISUSE 41766
#SRCCLFRPNR Len : 1 RC-COLLAPSED SOURCE OF FRND/REL PAIN RLVRS FOR LAST MISUSE 41766
#SRCPNRNM2 Len : 2 RC-SOURCE OF PAIN RELIEVERS FOR LAST MISUSE 40403
#GRSKLSDTRY Len : 1 RC-GREAT RISK: TRYING LSD ONCE OR TWICE 941
#GRSKLSDWK Len : 1 RC-GREAT RISK: USE LSD 1-2 TIMES PER WEEK 940
#GRSKMRJWK Len : 1 RC-GREAT RISK: USE MARIJUANA 1-2 TIMES PER WEEK 593
#GRSKMRJMON Len : 1 RC-GREAT RISK: USE MARIJUANA ONCE A MONTH 558
#POVERTY3 Len : 1 RC-POVERTY LEVEL (% OF US CENSUS POVERTY THRESHOLD) 385
#MHSUIPLN Len : 1 RC-MADE PLANS TO KILL SELF IN PAST YEAR 317
#MHSUITRY Len : 1 RC-ATTEMPTED TO KILL SELF IN PAST YEAR 317
#MHSUITHK Len : 1 RC-SERIOUSLY THOUGHT ABOUT KILLING SELF IN PAST YEAR 314
#PROB Len : 1 RC-PROBATION STATUS IN THE PAST YEAR 116
#PNRMAINRSN Len : 1 RC-MAIN REASON FOR MISUSING PAIN RELIEVERS LAST EPISODE 115
#PAROL Len : 1 RC-PAROLE/SUPERVISED RELEASE STATUS IN THE PAST YEAR 103
#YSDSOVRL Len : 1 RC-YOUTH: MAX SEVERITY LEVEL OF MDE ROLE IMPAIRMENT 13722
#LMMISUDPY Len : 1 RC-LMMI AND DRUG/ALCOHOL DEP OR ABUSE - PAST YEAR 13722
#SPDMON Len : 1 RC-PAST MONTH SERIOUS PSYCH DISTRESS INDICATOR 13722
#SPDYR Len : 1 RC-PAST YEAR SERIOUS PSYCHOLOGICAL DISTRESS INDICATOR 13722
#MI_CAT_U Len : 1 RC-CATEGORICAL MI INDICATOR REVISED 13722
#SMISUDPY Len : 1 RC-SMI AND DRUG/ALCOHOL DEP OR ABUSE - PAST YEAR 13722

dfs$AMDEYR<-fct_explicit_na(dfs$AMDEYR, "None-NA")
dfs$ASDSOVL2<-fct_explicit_na(dfs$ASDSOVL2, "None-NA")
dfs$AMISUDPY <-fct_explicit_na(dfs$AMISUDPY, "None-NA")
dfs$DRVINALDRG<-fct_explicit_na(dfs$DRVINALDRG, "None-NA")
dfs$GRSKHERTRY<-fct_explicit_na(dfs$GRSKHERTRY, "None-NA")
dfs$GRSKHERWK<-fct_explicit_na(dfs$GRSKHERWK, "None-NA")
dfs$SRCFRPNRNM <-fct_explicit_na(dfs$SRCFRPNRNM, "None-NA")
dfs$SRCCLFRPNR<-fct_explicit_na(dfs$SRCCLFRPNR,  "None-NA")
dfs$SRCPNRNM2 <-fct_explicit_na(dfs$SRCPNRNM2,  "None-NA")
dfs$POVERTY3 <-fct_explicit_na(dfs$POVERTY3,  "None-NA")
dfs$PROB<-fct_explicit_na(dfs$PROB,"None-NA")
dfs$PNRMAINRSN <-fct_explicit_na(dfs$PNRMAINRSN, "None-NA")
dfs$PAROL <-fct_explicit_na(dfs$PAROL ,  "None-NA")
dfs$YSDSOVRL<-fct_explicit_na(dfs$YSDSOVRL,  "None-NA")
dfs$LMMISUDPY<-fct_explicit_na(dfs$LMMISUDPY, "None-NA")
dfs$SPDMON<-fct_explicit_na(dfs$SPDMON, "None-NA")
dfs$SPDYR <-fct_explicit_na(dfs$SPDYR,"None-NA")
dfs$MI_CAT_U<-fct_explicit_na(dfs$MI_CAT_U,"None-NA")
dfs$SMISUDPY<-fct_explicit_na(dfs$SMISUDPY,"None-NA")

rm(missing)# now that it is down to 0 observations


#Finding values for Bad Data, Don't know, Refused, Skip 
#count number in each column

# 99, 999, 9999 Legitimate Skip/NEVER MISUSED - dit it on orig 308 variables - very few left in remaining data set
count.99.per.column <- ldply(dfs, function(c) sum(c=="99 - LEGITIMATE SKIP")) # HRTCONDEV 28711, COPDEVER 28711, CIRROSEVR 28711, HEPBCEVER 28711, KIDNYDSEV 28711, HIVAIDSEV 28711, CANCEREVR 28711, CABLADDER 40681, CABLOLEULYM 40681, CAOTHER2 40681, CABREAST 40681, CACERVIX 41236, CACOLNRECT 40681, CAESOPSTOM 40681, CAGALLIVPAN 40681, CAKIDNEY 40681, CALARYLUNG 40681, CAMELANOM 40681, CAMOUTTHRO 40681, CAOVARY 41236, CAPROSTEST 41605, CASKINOTH 40681, CASKINDK 40681, 
#CATHYROID 40681, CAUTERUS 41236, CANCERYR 40681, HRTCONDYR 39361, HIGHBPMED 36971, WRKSICKMO 13272, WRKSKIPMO 13272, IROTHHLT 36517, HEOTSMK   940, HEOTSNF   940, HEOTNDL   940, HEOTOTH   940, HEOTSP   983
View(count.99.per.column)

count.999.per.column <- ldply(dfs, function(c) sum(c=="999 - LEGITIMATE SKIP")) #NOBOOKY2 34989 NA in reduced set
View(count.999.per.column)
count.999b.per.column <- ldply(dfs, function(c) sum(c=="999 - NEVER MISUSED")) #IRPNRNMAGE 37319
count.9999.per.column <- ldply(dfs, function(c) sum(c=="9999 - NEVER MISUSED")) #IRPNRNMYFU 37319    NA in reduced set                            

#83 or 983, 9983, Did not use/misuse/Not a past year initiate
count.83.per.column <- ldply(df, function(c) sum(c=="83 - DID NOT MISUSE IN THE PST 12 MOS Logically assigned")) #PNRNMLAS1 89
count.83b.per.column <- ldply(df, function(c) sum(c=="83 - DID NOT MISUSE PAIN RELEIVERS IN PAST 12 mos Log assn")) #0
count.83c.per.column <- ldply(df, function(c) sum(c=="83 - DID NOT MISUSE PAIN RELIEVERS IN PST 30 days Log assn"))#PNRWYNORX 48
count.83d.per.column <- ldply(df, function(c) sum(c=="83 - DID NOT USE PAIN REL IN THE PAST 12 MOS Log assn"))#PNRNM30AL 12, PNRWYGAMT 23
count.83e.per.column <- ldply(df, function(c) sum(c=="83 - DID NOT USE PAIN REL IN THE PAST 12 MOS logically assigned")) #PNRWYOFTN 23
count.83f.per.column <- ldply(df, function(c) sum(c=="83 - DID NOT USE PAIN RELIEVERS PST 12 MOS Log assn")) #PNRWYLNGR 30, PNRWYOTWY 37
# 983, 9983 not in this df


#85 or 985, 9985 Bad Data
count.85.per.column<- ldply(df, function(c) sum(c=="85 - BAD DATA Logically assigned"))#HEOTSP  3, WRKSKIPMO 16,WRKSICKMO 61,SEXIDENT  7,PNRNM30AL  2,PNRWYNORX  3,PNRWYGAMT  2,PNRWYOFTN  3,PNRWYLNGR  3,PNRWYOTWY  1,PNRNMLAS1  9,PNRRSMAIN  1,BKMVTHFT  4,BKLARCNY  4,BKBURGL  4,BKSRVIOL  4,BKSMASLT  4,BKROB  4,BKARSON  4,BKDRVINF  4,BKDRUNK  4,BKDRUG  4,BKSEXNR  4,BKFRAUD  4,BKOTH  3,BKOTHOF2 80,INHOSPYR 14,HRTCONDEV 14,COPDEVER 11,CIRROSEVR 11,HEPBCEVER 11,KIDNYDSEV 11,HIVAIDSEV 11,CANCEREVR 11,CABLADDER 40,210  CABLOLEULYM 40,CAOTHER2 40,CABREAST 40,CACERVIX 21,CACOLNRECT 40,CAESOPSTOM 40,CAGALLIVPAN 40,CAKIDNEY 40,CALARYLUNG 40,CAMELANOM 40,CAMOUTTHRO 40,CAOVARY 21,CAPROSTEST 19,CASKINOTH 40,CASKINDK 40,CATHYROID 40,CAUTERUS 21,CANCERYR 11,HRTCONDYR 15,HIGHBPMED 11,SNYSELL 18,SNYSTOLE 18,SNYATTAK 18
count.985.per.column<- ldply(df, function(c) sum(c=="985 - BAD DATA Logically assigned")) #NMERTMT2 10,NOBOOKY2  9
count.9985.per.column<- ldply(df, function(c) sum(c=="9985 - BAD DATA Logically assigned")) #0

#91 or 991 or 9991 Never used
count.91.per.column<- ldply(dfs, function(c) sum(c=="91 - NEVER USED HEROIN")) #HERSMOKE,HRSMKREC,HERSNIFF,HRSNFREC,HERNEEDL,HEOTSMK,HEOTSNF,HEOTNDL,HEOTOTH,HEOTSP,HRNDLREC 41540 
count.91b.per.column<- ldply(dfs, function(c) sum(c=="91 - NEVER USED/MISUSED PAIN RELIEVERS")) #PNRNM30AL,PNRWYNORX,PNRWYGAMT,PNRWYOFTN,PNRWYLNGR ,PNRWYOTWY,PNRNMLAS1,PNRRSMAIN 37083
count.91c.per.column<- ldply(dfs, function(c) sum(c=="91 - NEVER MISUSED")) #IRPNRNMINIT 37319 na in reduced data set
#count.91d.per.column<- ldply(df, function(c) sum(c=="91 - NEVER USED ALCOHOL")) #0
#count.91d.per.column<- ldply(df, function(c) sum(c=="91 - NEVER USED CIGARETTES")) #0
# 991 NEVER USED x 0 in this df
# 9991 NEVER USED x 0 in this df

# 93, 993, 9993 Did not use/not a past year initiate etc.

count.93.per.column<- ldply(dfs, function(c) sum(c=="93 - DID NOT MISUSE PAIN RELIEVERS IN THE PAST 12 MOS")) #PNRWYNORX 3099,PNRWYGAMT 3124,PNRWYOFTN 3124,PNRWYLNGR 3117,PNRWYOTWY 3110,PNRNMLAS1 3058,PNRRSMAIN 3142
count.93b.per.column<- ldply(dfs, function(c) sum(c=="93 - DID NOT MISUSE PAIN RELIEVERS IN THE PAST 30 DAYS")) #PNRNM30AL 4737
count.93c.per.column<- ldply(dfs, function(c) sum(c=="93 - NO PAST YEAR MISUSE"))#IRPNRNMINIT 2966
count.93d.per.column<- ldply(dfs, function(c) sum(c=="93 - USED HEROIN BUT NEVER SMOKED IT"))#HRSMKREC 585
count.93e.per.column<- ldply(dfs, function(c) sum(c=="93 - USED HEROIN BUT NEVER SNIFFED IT"))#HRSNFREC 293
count.93f.per.column<- ldply(dfs, function(c) sum(c=="93 - USED HEROIN BUT NEVER WITH A NEEDLE"))#HRNDLREC 516
count.993.per.column<- ldply(dfs, function(c) sum(c=="993 - NOT A PAST YEAR INITIATE"))#IRPNRNMAGE 4855
count.9993.per.column<- ldply(dfs, function(c) sum(c=="9993 - NOT A PAST YEAR INITIATE"))#IRPNRNMYFU 4855

# 94, 994, 9994 Don't Know
count.94.per.column<-ldply(dfs, function(c) sum(c=="94 - DON T KNOW"))#WRKSICKMO 194,WRKSKIPMO 169,SEXIDENT 256,PNRWYNORX  43,PNRWYGAMT  43,PNRWYOFTN  43, PNRWYLNGR  43,PNRWYOTWY  43,PNRNMLAS1   7,BKMVTHFT   2,BKLARCNY   1,BKSMASLT   1,BKDRUNK   1,BKFRAUD   1,BKOTH   1,BKOTHOF2  13,INHOSPYR  66,HRTCONDEV 146,COPDEVER 146,CIRROSEVR 146,HEPBCEVER 146,KIDNYDSEV 146,HIVAIDSEV 146,CANCEREVR 146,CABLADDER   4,CABLOLEULYM   4,CAOTHER2   4,CABREAST   4,CACERVIX   2,CACOLNRECT   4,CAESOPSTOM   4,CAGALLIVPAN   4,CAKIDNEY   4,CALARYLUNG   4,CAMELANOM   4,CAMOUTTHRO   4,CAOVARY   2,CAPROSTEST   2,CASKINOTH   4,
#CASKINDK   4,CATHYROID   4,CAUTERUS   2,CANCERYR   5,HRTCONDYR   9,SNYSELL  60,SNYSTOLE  56,SNYATTAK  54
count.994.per.column<-ldply(dfs, function(c) sum(c=="994 - DON T KNOW"))#NMERTMT2 508,NOBOOKY2  26
#9994 0 in this df

View(count.97.per.column)

# 97,997,9997(0 in this df) Refused
count.97.per.column<-ldply(dfs, function(c) sum(c=="97 - REFUSED"))#PNRNM30AL   1,PNRWYNORX  43,PNRWYGAMT  43,PNRWYOFTN  43,PNRWYLNGR  43,PNRWYOTWY  43,PNRNMLAS1   5,BKMVTHFT 110,BKLARCNY 109,BKBURGL 109,BKSRVIOL 110,BKSMASLT 110,BKROB 109,BKARSON 109,BKDRVINF 110,BKDRUNK 109,BKDRUG 109,BKSEXNR 109,BKFRAUD 109,BKOTH 109,BKOTHOF2 134,INHOSPYR  97,HRTCONDEV 230,COPDEVER 230,CIRROSEVR 230,HEPBCEVER 230,KIDNYDSEV 230,HIVAIDSEV 230,CANCEREVR 230,
#HRTCONDYR   3,SNYSELL 104,SNYSTOLE  85,SNYATTAK  81,SEXIDENT 470,WRKSICKMO 386,WRKSKIPMO 408, HERSMOKE   2,HRSMKREC   2,HERSNIFF   2,HRSNFREC   2,HERNEEDL   2,HEOTSMK   4,HEOTSNF   4,HEOTNDL   4,HEOTOTH   4,HEOTSP   6,HRNDLREC   2
count.997.per.column<-ldply(dfs, function(c) sum(c=="997 - REFUSED"))#NMERTMT2 305,NOBOOKY2 140


View(count.98.per.column)
View(count.998.per.column)

# 98, 998, 9998 ( 0 in this df) Blank
count.98.per.column<-ldply(dfs, function(c) sum(c=="98 - BLANK"))# HERSMOKE 7,HRSMKREC 7,HERSNIFF 7,HRSNFREC 7,HERNEEDL 6,HEOTSMK 7,HEOTSNF 7,HEOTNDL 7,HEOTOTH 7,HEOTSP   7,WRKSICKMO 152,WRKSKIPMO 152,SEXIDENT  19,INHOSPYR  11,HRTCONDEV  15,COPDEVER 18,CIRROSEVR 18,HEPBCEVER 18,KIDNYDSEV 18,HIVAIDSEV 18,CANCEREVR 18,CABLADDER 395,CABLOLEULYM 395,CAOTHER2 395,CABREAST 395,CACERVIX 394,CACOLNRECT 395,CAESOPSTOM 395,CAGALLIVPAN 395,CAKIDNEY 395,CALARYLUNG 395,CAMELANOM 395,CAMOUTTHRO 395,CAOVARY 394,CAPROSTEST 395,CASKINOTH 395,CASKINDK 395,CATHYROID 395,CAUTERUS 394,CANCERYR 395,HRTCONDYR 392,HIGHBPMED 395,SNYSELL  15,SNYSTOLE  16,SNYATTAK  16,BKMVTHFT 233,BKLARCNY 233,BKBURGL 233,
#BKSRVIOL 233,BKSMASLT 233,BKROB 233,BKARSON 233,BKDRVINF 233,BKDRUNK 233,BKDRUG 233,BKSEXNR 233,BKFRAUD 233,BKOTH 233,BKOTHOF2 233,PNRNM30AL  82,PNRWYNORX  61,PNRWYGAMT  62,PNRWYOFTN  62,PNRWYLNGR  62,PNRWYOTWY  63,PNRNMLAS1  61,PNRRSMAIN  98
count.998.per.column<-ldply(dfs, function(c) sum(c=="998 - BLANK"))# NMERTMT2  11,NOBOOKY2 231


summary(dfs$CANCERYR)
levels(dfs$CANCERYR)
str(dfs$CANCERYR)
table(dfs$CANCERYR)
levels(dfs$CANCERYR)[levels(dfs$CANCERYR)=='99 - LEGITIMATE SKIP'] <- 'NA'
levels(dfs$CANCERYR)[levels(dfs$CANCERYR)=='98 - BLANK'] <- 'NA'
levels(dfs$CANCERYR)[levels(dfs$CANCERYR)=='5 - Yes LOGICALLY ASSIGNED from skip pattern'] <- '5 - Yes LOGICALLY ASSIGNED '
levels(dfs$CANCERYR)[levels(dfs$CANCERYR)=='94 - DON T KNOW'] <- 'NA'
levels(dfs$CANCERYR)[levels(dfs$CANCERYR)=='85 - BAD DATA Logically assigned'] <- 'NA'

table(dfs$SEXIDENT)
levels(dfs$SEXIDENT)[levels(dfs$SEXIDENT)=='85 - BAD DATA Logically assigned']<- 'NA'
levels(dfs$SEXIDENT)[levels(dfs$SEXIDENT)=='99 - LEGITIMATE SKIP'] <- 'NA'
levels(dfs$SEXIDENT)[levels(dfs$SEXIDENT)=='98 - BLANK'] <- 'NA'
levels(dfs$SEXIDENT)[levels(dfs$SEXIDENT)=='97 - REFUSED'] <- 'NA'
levels(dfs$SEXIDENT)[levels(dfs$SEXIDENT)=='94 - DON T KNOW'] <- 'NA'
levels(dfs$SEXIDENT)[levels(dfs$SEXIDENT)=='1 - Heterosexual, that is, straight']<- '1-Heterosexual'                                              
levels(dfs$SEXIDENT)[levels(dfs$SEXIDENT)=='2 - Lesbian or Gay']<- '2 - Lesbian_Gay'    

table(dfs$IROTHHLT)
levels(dfs$IROTHHLT)[levels(dfs$IROTHHLT)=='1 - Yes, R is covered by other health insurance']<- '1-HIns'                                              
levels(dfs$IROTHHLT)[levels(dfs$IROTHHLT)=='2 - No, R is not covered by other health insurance']<- '2-No_HIns'
levels(dfs$IROTHHLT)[levels(dfs$IROTHHLT)=='99 - LEGITIMATE SKIP'] <- 'NA'

table(dfs$HEPBCEVER)
levels(dfs$HEPBCEVER)[levels(dfs$HEPBCEVER)=='99 - LEGITIMATE SKIP'] <- 'NA'
levels(dfs$HEPBCEVER)[levels(dfs$HEPBCEVER)=='94 - DON T KNOW'] <- 'NA'
levels(dfs$HEPBCEVER )[levels(dfs$HEPBCEVER)=='98 - BLANK'] <- 'NA'               
levels(dfs$HEPBCEVER )[levels(dfs$HEPBCEVER)=='97 - REFUSED'] <- 'NA' 
levels(dfs$HEPBCEVER )[levels(dfs$HEPBCEVER)=='85 - BAD DATA Logically assigned']<- 'NA'    
                                                  
table(dfs$HIVAIDSEV)                                           
levels(dfs$HIVAIDSEV)[levels(dfs$HIVAIDSEV)=='99 - LEGITIMATE SKIP'] <- 'NA'    
levels(dfs$HIVAIDSEV)[levels(dfs$HIVAIDSEV)=='94 - DON T KNOW'] <- 'NA'   
levels(dfs$HIVAIDSEV)[levels(dfs$HIVAIDSEV)=='98 - BLANK'] <- 'NA'  
levels(dfs$HIVAIDSEV)[levels(dfs$HIVAIDSEV)=='97 - REFUSED'] <- 'NA'  
levels(dfs$HIVAIDSEV)[levels(dfs$HIVAIDSEV)=='85 - BAD DATA Logically assigned']<- 'NA'

# Shouldn't be factors - should be integers
table(dfs$WRKSICKMO) 
levels(dfs$WRKSICKMO) 
table(dfs$WRKSKIPMO) 
levels(dfs$WRKSKIPMO) 
levels(dfs$WRKSICKMO)[levels(dfs$WRKSICKMO)=='85 - BAD DATA Logically assigned']<- 'NA'
levels(dfs$WRKSKIPMO)[levels(dfs$WRKSKIPMO)=='85 - BAD DATA Logically assigned']<- 'NA'

table(dfs$CANCEREVR) 
levels(dfs$CANCEREVR)[levels(dfs$CANCEREVR)=='99 - LEGITIMATE SKIP'] <- 'NA'    
levels(dfs$CANCEREVR)[levels(dfs$CANCEREVR)=='94 - DON T KNOW'] <- 'NA'   
levels(dfs$CANCEREVR)[levels(dfs$CANCEREVR)=='98 - BLANK'] <- 'NA'  
levels(dfs$CANCEREVR)[levels(dfs$CANCEREVR)=='97 - REFUSED'] <- 'NA'  
levels(dfs$CANCEREVR)[levels(dfs$CANCEREVR)=='85 - BAD DATA Logically assigned']<- 'NA'

table(dfs$SNYSELL) #Len : 2 SOLD ILLEGAL DRUGS - last 12 months
levels(dfs$SNYSELL)[levels(dfs$SNYSELL)=='99 - LEGITIMATE SKIP'] <- 'NA'    
levels(dfs$SNYSELL)[levels(dfs$SNYSELL)=='94 - DON T KNOW'] <- 'NA'   
levels(dfs$SNYSELL)[levels(dfs$SNYSELL)=='98 - BLANK'] <- 'NA'  
levels(dfs$SNYSELL)[levels(dfs$SNYSELL)=='97 - REFUSED'] <- 'NA'  
levels(dfs$SNYSELL)[levels(dfs$SNYSELL)=='85 - BAD DATA Logically assigned']<- 'NA'

table(dfs$SNYSTOLE) #Len : 2 STOLEN/TRIED TO STEAL ANYTHING WORTH > $50 past 12 mths
levels(dfs$SNYSTOLE)[levels(dfs$SNYSTOLE)=='99 - LEGITIMATE SKIP'] <- 'NA'    
levels(dfs$SNYSTOLE)[levels(dfs$SNYSTOLE)=='94 - DON T KNOW'] <- 'NA'   
levels(dfs$SNYSTOLE)[levels(dfs$SNYSTOLE)=='98 - BLANK'] <- 'NA'  
levels(dfs$SNYSTOLE)[levels(dfs$SNYSTOLE)=='97 - REFUSED'] <- 'NA'  
levels(dfs$SNYSTOLE)[levels(dfs$SNYSTOLE)=='85 - BAD DATA Logically assigned']<- 'NA'

table(dfs$SNYATTAK) # Len : 2 ATTACKED SOMEONE W/INTENT TO SERIOUSLY HURT THEM
levels(dfs$SNYATTAK)[levels(dfs$SNYATTAK)=='99 - LEGITIMATE SKIP'] <- 'NA'    
levels(dfs$SNYATTAK)[levels(dfs$SNYATTAK)=='94 - DON T KNOW'] <- 'NA'   
levels(dfs$SNYATTAK)[levels(dfs$SNYATTAK)=='98 - BLANK'] <- 'NA'  
levels(dfs$SNYATTAK)[levels(dfs$SNYATTAK)=='97 - REFUSED'] <- 'NA'  
levels(dfs$SNYATTAK)[levels(dfs$SNYATTAK)=='85 - BAD DATA Logically assigned']<- 'NA'

table(dfs$HEOTSP) # Len : 2 HOW USED HEROIN: SPECIFY
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='99 - LEGITIMATE SKIP'] <- 'NA'    
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='94 - DON T KNOW'] <- 'NA'   
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='98 - BLANK'] <- 'NA'  
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='97 - REFUSED'] <- 'NA'  
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='85 - BAD DATA Logically assigned']<- 'NA'
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='91 - NEVER USED HEROIN']<- 'NA'
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='1 - You smoked heroin']<- '1-Smoked Heroin'
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='3 - You injected heroin with a needle']<- '3-Injected Heroin'
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='5 - You took it orally/by mouth/in pill form']<- '5-Oral Heroin/pill'
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='6 - You sniffed or snorted in liquid/melted form']<-  '6-Sniffed/Snorted Heroin liquid/melted'
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='15 - You combined it w/some other drug; not specified']<-  '15-Used Heroin with other drug'
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='16 - You sniffed/snorted/huffed it; pwdr/liquid unspfd ']<-  '16-Sniffed/snorted/huffed Heroin pwdr/liquid'
levels(dfs$HEOTSP)[levels(dfs$HEOTSP)=='24 - Acknowledged use; how unspecified ']<-  '24-Heroin use method not spfd'

table(dfs$HERSMOKE) # Len : 2 EVER SNIFFED HEROIN

dfs[,c("HERSMOKE")] <- list(NULL) # Dropped as covered by HERSMOK2
dfs[,c("HRSNFREC")] <- list(NULL) # not required

table(dfs$HEOTSNF) # Len : 2 EVER SNIFFED HEROIN
# Dropped as covered by HERSNIFF1 
dfs[,c("HEOTSNF")] <- list(NULL)

dfs[,c("HEOTOTH","HEOTNDL","HEOTSMK")] <- list(NULL)
# Dropped as NA or covered by HERSMOK2 and NEDHER

dfs[,c("HERNEEDL")] <- list(NULL) 
# Dropped as covered by NEDHER

table(dfs$IRPNRNMAGE) # Len : 3 PAIN RELIEVER AGE OF FIRST MISUSE - IMPUTATION REVISED
levels(dfs$IRPNRNMAGE)[levels(dfs$IRPNRNMAGE)=='993 - NOT A PAST YEAR INITIATE']<- 'NA'
levels(dfs$IRPNRNMAGE)[levels(dfs$IRPNRNMAGE)=='999 - NEVER MISUSED']<- 'NA'

table(dfs$IRPNRNMYFU) #Len : 4 PAIN RELIEVER YEAR OF FIRST MISUSE - IMPUTATION REVISED 2016-2017 or not
table(dfs$PNRNMLAS1) # NA in reduced set       

table(dfs$HRNDLREC)#Len : 2 TIME SINCE LAST USED NEEDLE TO INJECT HEROIN
# put values of Logically Assigned 9 and 13 LA into
levels(dfs$HRNDLREC)[levels(dfs$HRNDLREC)=='91 - NEVER USED HEROIN']<- 'NA'
levels(dfs$HRNDLREC)[levels(dfs$HRNDLREC)=='97 - REFUSED'] <- 'NA'  
levels(dfs$HRNDLREC)[levels(dfs$HRNDLREC)=='2 - More than 30 days ago but within the past 12 mos']<- '1-12mths'
levels(dfs$HRNDLREC)[levels(dfs$HRNDLREC)=='9 - At some point in the lifetime LOG ASSN']<- '9-At some pt in lifetime LA'
levels(dfs$HRNDLREC)[levels(dfs$HRNDLREC)=='13 - More than 12 months ago LOGICALLY ASSIGNED']<- '13-More than 12mth LA'
levels(dfs$HRNDLREC)[levels(dfs$HRNDLREC)=='93 - USED HEROIN BUT NEVER WITH A NEEDLE'] <- 'NA'
levels(dfs$HRNDLREC)[levels(dfs$HRNDLREC)=='98 - BLANK'] <- 'NA'

table(dfs$HERSNIFF)#Len : 2 EVER SNIFFED HEROIN
# put values of Logically Assigned 3 into 1
levels(dfs$HERSNIFF)[levels(dfs$HERSNIFF)=='91 - NEVER USED HEROIN']<- 'NA'
levels(dfs$HERSNIFF)[levels(dfs$HERSNIFF)=='97 - REFUSED'] <- 'NA'  
levels(dfs$HERSNIFF)[levels(dfs$HERSNIFF)=='98 - BLANK'] <- 'NA'

 
table(dfs$HRSMKREC)#Len : 2 TIME SINCE LAST SMOKED HEROIN
# put values of Logically Assigned into others
levels(dfs$HRSMKREC)[levels(dfs$HRSMKREC)=='91 - NEVER USED HEROIN']<- 'NA'
levels(dfs$HRSMKREC)[levels(dfs$HRSMKREC)=='97 - REFUSED'] <- 'NA'  
levels(dfs$HRSMKREC)[levels(dfs$HRSMKREC)=='98 - BLANK'] <- 'NA'
levels(dfs$HRSMKREC)[levels(dfs$HRSMKREC)=='2 - More than 30 days ago but within the past 12 mos']<- '1-12mths'
levels(dfs$HRSMKREC)[levels(dfs$HRSMKREC)=='13 - More than 12 months ago LOGICALLY ASSIGNED']<- '13-More than 12mth LA'
levels(dfs$HRSMKREC)[levels(dfs$HRSMKREC)=='93 - USED HEROIN BUT NEVER SMOKED IT'] <- 'NA'

table(dfs$RSKHERTRY)#Len : 2 RISK TRYING HEROIN ONCE OR TWICE
levels(dfs$RSKHERTRY)[levels(dfs$RSKHERTRY)=='85 - BAD DATA Logically assigned']<- 'NA'
levels(dfs$RSKHERTRY)[levels(dfs$RSKHERTRY)=='98 - BLANK'] <- 'NA'
levels(dfs$RSKHERTRY)[levels(dfs$RSKHERTRY)=='97 - REFUSED'] <- 'NA'  
levels(dfs$RSKHERTRY)[levels(dfs$RSKHERTRY)=='94 - DON T KNOW'] <- 'NA'    

table(dfs$RSKHERWK)#Len : 2 RISK USING HEROIN ONCE OR TWICE A WEEK
levels(dfs$RSKHERWK)[levels(dfs$RSKHERWK)=='85 - BAD DATA Logically assigned']<- 'NA'
levels(dfs$RSKHERWK)[levels(dfs$RSKHERWK)=='98 - BLANK'] <- 'NA'
levels(dfs$RSKHERWK)[levels(dfs$RSKHERWK)=='97 - REFUSED'] <- 'NA'  
levels(dfs$RSKHERWK)[levels(dfs$RSKHERWK)=='94 - DON T KNOW'] <- 'NA'    

table(dfs$NMERTMT2)#Len : 3 # OF TIMES BEEN TREATED IN EMER ROOM PAST 12 MOS
# shouldn't be a factor
levels(dfs$NMERTMT2)[levels(dfs$NMERTMT2)=='985 - BAD DATA Logically assigned']<- 'NA'
levels(dfs$NMERTMT2)[levels(dfs$NMERTMT2)=='994 - DON T KNOW'] <- 'NA'
levels(dfs$NMERTMT2)[levels(dfs$NMERTMT2)=='998 - BLANK'] <-'NA'
levels(dfs$NMERTMT2)[levels(dfs$NMERTMT2)=='997 - REFUSED']<- 'NA'

rm(count.83.per.column)
rm(count.83b.per.column)
rm(count.83c.per.column)
rm(count.91.per.column)
rm(count.91b.per.column)
rm(count.91c.per.column)
rm(count.93.per.column)
rm(count.93b.per.column)
rm(count.93c.per.column)
rm(count.93d.per.column)
rm(count.93e.per.column)
rm(count.93f.per.column)
rm(count.94.per.column)
rm(count.97.per.column)
rm(count.98.per.column)
rm(count.99.per.column)
rm(count.993.per.column)
rm(count.994.per.column)
rm(count.997.per.column)
rm(count.998.per.column)
rm(count.999.per.column)
rm(count.999b.per.column)
rm(count.9993.per.column)
rm(count.9999.per.column)
dfsbak<-dfs
View(names(dfs))
