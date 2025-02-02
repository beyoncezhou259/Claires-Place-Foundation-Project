#load libraries
library(tidyverse)
library(moderndive)
library(infer)
library(dplyr)
library(lubridate) 

#rename dataset
patients <- Claires_Patients_SP24_Final

###EHSG #########
#########################################
###number of applications to EHSG before and after the policy change

#converte date column to Date class
patients$Date <- as.Date(patients$Date,format="%m/%d/%y")
class(patients$Date)

#check if there are any NA dates
missing_dates <- is.na(patients$Date)
sum(missing_dates)
#there are 0 missing dates

#create subsets of data before and after the policy change
beforeChange <- patients %>% 
  filter(Date<ymd("2023-06-01"))
  
afterChange <- patients %>% 
  filter(Date >= ymd("2023-06-01"))


#number of applications in each subset
summary(beforeChange)
#303 qpplications before policy change
summary(afterChange)
#72 applications after policy change

#visualize differences 
patients <- patients %>% 
  mutate(policyChange = ifelse(Date<ymd("2023-06-01"),"Before","After"))
ggplot(patients,aes(x=policyChange))+
  geom_bar()+
  theme_minimal()+
  labs(x="Date Relative to Policy Change on 2023-06-01",y="Number of Applications")

#########################################

#grant amounts before and after policy change

#how many applicants not granted anything
patients$Amt_granted <- as.numeric(patients$Amt_granted)
noGrant <- filter(patients,Amt_granted==0)
#31 applicants

#what is the diff in proportion of applicants that didn't receive grants for before and after the policy change
patients <- patients %>% 
  mutate(granted= ifelse(Amt_granted==0,"NO","YES"))

ggplot(patients,aes(x=policyChange,fill=granted))+
  geom_bar()+
  theme_minimal()

beforeChange$Amt_granted <- as.numeric(beforeChange$Amt_granted)
beforeChange <- beforeChange %>% 
  mutate(granted_b= ifelse(Amt_granted==0,"NO","YES"))

#########################################
#are factors like state, new/returning, or proportion of minors impacted by policy changes

#regression analysis with interaction terms
patients$Date <- as.numeric(patients$Date)
lm_policyChange <- lm(Date~Minor*New,
              patients)
summary(lm_policyChange)

#proportion of status based on minor or not

table_sm <- table(patients$Minor,patients$Status)
proportions(table_sm, margin=1)

#findings: higher prop of declined apps for non-minors. higher prop of withdrawn apps for minors.

#regression analysis for predicting amount granted (independent variable) based on factors like state, age, and new (dependent variables)

# Fit linear regression model
amt_granted_model <- lm(Amt_granted ~ State, data = patients,na.rm=TRUE)

# Summarize the regression model
summary(amt_granted_model)
#findings: overall model is statistically significant (p value<0.05). 
#CT, MD. ME, MI, RI, and WY have negative coefficient correlations, so generally receive smaller grants than intercept amount (962.07)
#new applicants have positive coefficient correlation
#minors have positive coefficient correlation
#Delaware and Massachusetts are statistically significant, significantly higher amounts granted

#hypothesis test comparing prop of those that received grants before and after policy change
#h0: prop of applicants before policy change that received grant = prop of applicants after policy change that received grant
#h1: prop of applicants before policy change that received grant < prop of applicants after policy change that received grant

#obs
obs_receive_grant <- patients %>% 
  specify(granted~policyChange,
          success="YES") %>% 
  calculate(stat="diff in props",
            order=c("Before","After"))

#null 
null_receive_grant <- patients %>% 
  specify(granted~policyChange,
          success="YES") %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000) %>% 
  calculate(stat="diff in props",
            order=c("Before","After"))
#p-value
get_p_value(null_receive_grant,obs_receive_grant,
            direction="left")

#p value is 0.246, fail to reject null hypothesis 

#hypothesis test comparing average grant amount before and after policy change
#h0: average amount granted before policy change = average amount granted after policy change
#h1: average amount granted before policy change < average amount granted after policy change

#obs
obs_diff_grant <- patients %>% 
  specify(Amt_granted~policyChange) %>% 
  calculate(stat="diff in means",
            order=c("Before","After"))

#null
null_diff_grant <- patients %>% 
  specify(Amt_granted~policyChange) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000) %>% 
  calculate(stat="diff in means",
            order=c("Before","After"))

#get p value
get_p_value(null_diff_grant,obs_diff_grant,
            direction="left")

#p value is 0.771, fail to reject null hypothesis
