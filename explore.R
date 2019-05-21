library(dplyr)
library(stringr)
library(dlookr)
library(sqldf)
library(lessR)
library(plyr)
library(survey)
library(ggplot2)
library(ggpubr)
library(survey)
library(knitr)
library(readr)
library(gridExtra)
library(kableExtra)
library(jtools)
library(stargazer)
library(sjPlot)

# load dataset
head(df)

# filter data past year misuse opioids - 30236 observations representing 
dfp<-df %>% filter(PNRANYFLAG_R==1)

detach("package:plyr") #causing errors with this graph
#graph of misuse by county size

both%>%
  group_by(PNRNMFLAG_S,COUTYP4_S)%>%
  summarize(n=n())%>%
  mutate(percent = n/sum(n))%>%
  filter(PNRNMFLAG_S%in%c("1 - Ever misused","0 - Never misused"),
         !is.na(COUTYP4_S))%>%
  ggplot()+
  geom_col(aes(x=COUTYP4_S, y=percent, fill=PNRNMFLAG_S), position = "dodge")+
  ggtitle("Pain reliever misusers by geography")

# hist of age of first use excluding zeros
hist( dfp$IRPNRNMAGE_R[ !dfp$IRPNRNMAGE_R==0], main="Age of first Misuse")

# Testing relationship between two variables Gender and Past Year Pain Reliever Misuse - PNRMYR
# cor.test(df$PNRNMYR_R, df$IRSEX_R)
# relation <- lm(df$IRSEX_R~df$PNRNMYR_R)
# print(relation)
# print(summary(relation))

#Construct a complex sample survey design:
nsduh <- 
  svydesign( 
    id = ~ VEREP_R , 
    strata = ~ VESTR_R , 
    data = dfp , 
    weights = ~ ANALWT_C_R, 
    nest = TRUE 
  )
#nest=TRUE as cluster IDs are nested withing the strata

class(nsduh)# survey.design2 Shows it's a survey design object
View(nsduh)

# To find total population of the orginal survey
nsduhb <- 
  svydesign( 
    id = ~ VEREP_R , 
    strata = ~ VESTR_R , 
    data = df , 
    weights = ~ ANALWT_C_R, 
    nest = TRUE 
  )

## Add new column to the data set- The variable one contains only the number 1 for every observation.
nsduh<-update(nsduh,one = 1 )# Ever used Pain Relievers - same as Tableau data set
nsduhb<-update(nsduhb,one = 1 )# Whole survey
degf( nsduh ) # Degrees of freedom 50


### Weighted Counts
##Count the weighted size of the generalisable population, overall and by groups:
svytotal( ~ one , nsduh)# # 166,854,298 people who had used opioids SE 1675122
svytotal( ~ one , nsduhb)# # 272,103,335 people who had used opioids SE 1892182
svyby( ~ one ,  ~ PNRNMYR_R, nsduh, svytotal ) # 10804202  SE 329074.9 - - same as in Tableau
svyby( ~ one ,  ~ CATAG3_S, nsduhb, svytotal ) # 



### Calculate the distribution of categorical variables, overall and by groups - Variables ending in _S are factors, those in _R are not.
svymean( ~ HEALTH2_S , nsduh , na.rm = TRUE )
svyby( ~ HEALTH2_S , ~ COUTYP4_S , nsduh, svymean , na.rm = TRUE )
svyby( ~ one ,  ~ COUTYP4_R, nsduh, svytotal )
svyby( ~ HEALTH2_S ,  ~ IRSEX_S, nsduh, svytotal )

svyby( ~ one ,  ~NEWRACE2_S ,  nsduh, svytotal )
svyby( ~one , ~NEWRACE2_S , nsduh, unwtd.count) # unweighted counts
svyby( ~one , ~NEWRACE2_S  , nsduh, svytotal ) # weighted counts - multiplies records by weights - ANALWT_C for each observation

### Print the survey-weighted glm
(mysvyglm<-svyglm(NEWRACE2_R~PNRNMYR_R,nsduh))# brackets to print immediately
summary(mysvyglm)

#Estimate a ratio:
#svyratio( 
#  numerator = ~ ABODHER_R , 
#  denominator = ~PNRNMFLAG_R , 
#  nsduh,
#  na.rm = TRUE
#)

## Initially in when I started the exploration I was looking at PNRNMFLAG - Ever misused Prescription Pain Relievers
## I decided to go with past year misuse as I felt it would give more definite information so 
### Regression Models and Tests of Association
### Perform a design-based t-test:
## Is there a relationship between past year heroin dependance or abuse and lifetime opioid misuse

svyttest(~IRSEX_R,~ PNRNMYR_R, nsduh) #p-value = 0.00000000006143

#Perform a chi-squared test of association for survey data
svychisq(~ PNRNMYR_R + HEALTH2_S ,   nsduh,statistic = "Chisq")
svychisq(~ PNRNMYR_R+ IRSEX_S ,   nsduh,statistic = "Chisq")
svychisq(~ ABODHER_R + PNRNMFLAG_R ,   nsduh_design)

tbl <- svytable(~PNRNMYR_S+HEALTH2_S, nsduh)
tbl1 <- svytable(~PNRNMYR_S+IRSEX_S, nsduh)
tbl2 <- svytable(~PNRNMYR_S+CATAG2_S, nsduh_)
tbl3 <- svytable(~PNRNMFLAG_S+NEWRACE2_S, nsduh)
tbl4 <- svytable(~PNRNMFLAG_S+COUTYP4_S, nsduh)
tbl5 <- svytable(~PNRNMFLAG_S+IRINSUR4_S, nsduh)#yes/npo
tbl7 <- svytable(~PNRNMFLAG_S+IRMARIT_S, nsduh)# needs recoded from 99 - LEGITIMATE SKIP Respondent is <= 14 years old
tbl8 <- svytable(~PNRNMFLAG_S+INCOME_S, nsduh)
tbl9 <- svytable(~PNRNMFLAG_S+ABODHER_S, nsduh)
tbl10  <- svytable(~PNRNMFLAG_S+ABUSEPYSED_S, nsduh)# sedative abuse
tbl11<- svytable(~PNRNMFLAG_S+ABUSEPYSTM_S, nsduh)# stimulant abuse
tbl12<- svytable(~PNRNMFLAG_S+ASDSOVL2_S, nsduh)# adult mental health -MAX SEVERITY LEVEL OF DEP FEELINGS ROLE IMPAIRMENT
tbl13<- svytable(~PNRNMFLAG_S+YSDSOVRL_S, nsduh)# youth
tbl14<- svytable(~PNRNMFLAG_S+IRWRKSTAT_S, nsduh)# Employment status



prop.table(svytable(~PNRNMYR_S+ASDSOVL2_S, nsduh))# adult
prop.table(svytable(~PNRNMYR_S+YSDSOVRL_S, nsduh))# youth
plot(PNRNMFLAG_S,ASDSOVL2_S)
svyplot(IRSEX_S~HEALTH2_S, design=nsduh, style="bubble")# not for categorical
svyhist(IRSEX_R)# not for categorical

prop.table(svytable(~PNRNMFLAG_S, nsduh_design))
prop.table(svytable(~PNRNMFLAG_S+BOOKED_S,nsduh),1)%>%
  kable()%>%
  kable_styling(bootstrap_options = "hover", full_width = F, position = "left")

#svyglm weighted regression model 
mysvyglm<-svyglm(PNRNMFLAG_R ~ IRSEX_R + HEALTH2_R, design = nsduh, family = quasipoisson(log))
summary(mysvyglm)

svyquantile(~IRPNRNMAGE_R, nsduh , 0.75 , na.rm = TRUE)

svyby( 
  ~ IRPNRNMAGE_R, 
  ~ INCOME_R , 
  nsduh, 
  svyquantile , 
  0.5 ,
  ci = TRUE ,
  keep.var = TRUE ,
  na.rm = TRUE
)

svyratio( 
  numerator = ~ PNRNMYR_R, 
  denominator = ~ PNRANYYR_R, 
  nsduh,
  na.rm = TRUE
)

svychisq(~ PNRNMYR_R + HEALTH2_R, nsduh)

svymean( ~ IRPNRNMAGE_R, nsduh , na.rm = TRUE , deff = TRUE )
svymean( ~ IRPNRNMAGE_R , nsduh , na.rm = TRUE , deff = "replace" )

svycor(~PNRNMYR_R + HEALTH2_R, design =nsduh)
svyvar(~PNRNMYR_R + HEALTH2_R, design = nsduh)

t.test(ABUSEHER_R~ PNRNMYR_R, data=both) 
svyttest( ABUSEHER_R ~ PNRNMYR_R , nsduh)

library(plotly)
cor.test(both$ABUSEHER_R, both$PNRNMYR_R)


boxplot(data2proj3$depression ~ data2proj3$alcohol)
install.packages('TMB', type = 'source')
library(TMB)
library(sjPlot)

glmOut <- svyglm(PNRNMYR_R ~ NEWRACE2_R + CATAG3_R + IRSEX_R, design = nsduh, family = quasibinomial(link = "logit"))

lab <- c("Race", "Age", "Sex")
labdep <- c("Opioid Misuse")
tab_model(glmOut)


# Load ggplot2
library(ggplot2)

# Construct a histogram of the weights
ggplot(data = both, mapping = aes(x = ANALWT_C_R)) +
  geom_histogram()


