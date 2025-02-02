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
  geom_bar(fill="gold")+
  theme_minimal()+
  labs(x="Date Relative to Policy Change on 2023-06-01",y="Number of Applications")

#########################################

#grant amounts before and after policy change

#how many applicants not granted anything
patients$Amt_granted <- as.numeric(patients$Amt_granted)
noGrant <- filter(patients,Amt_granted==0)
summary(patients,Status=="Completed",)
#31 applicants

#what is the diff in proportion of applicants that didn't receive grants for before and after the policy change
patients <- patients %>% 
  mutate(granted= ifelse(Amt_granted==0,"NO","YES"))

ggplot(patients,aes(x=policyChange,fill=granted))+
  geom_bar(position="dodge")+
  theme_minimal()+
  labs(x="Date Relative to Policy Change on 2023-06-01",y="Number of Applications",title="Applicants that Received Grants Before and After Policy Change")

beforeChange$Amt_granted <- as.numeric(beforeChange$Amt_granted)
beforeChange <- beforeChange %>% 
  mutate(granted_b= ifelse(Amt_granted==0,"NO","YES"))

afterChange$Amt_granted <- as.numeric(afterChange$Amt_granted)
afterChange <- afterChange %>% 
  mutate(granted_a= ifelse(Amt_granted==0,"NO","YES"))

before_prop <- beforeChange %>% 
  group_by(granted_b) %>% 
  summarize(count=n()) %>% 
  mutate(proportion = count/sum(count))
before_prop

after_prop <- afterChange %>% 
  group_by(granted_a) %>% 
  summarize(count=n()) %>% 
  mutate(proportion = count/sum(count))
after_prop
#########################################
#are factors like state, new/returning, or proportion of minors impacted by policy changes

#regression analysis of amount granted based on minor or new
patients$Date <- as.numeric(patients$Date)
lm_minor_new <- lm(Amt_granted~Minor*New,
              patients)
summary(lm_minor_new)
get_regression_table(lm_minor_new)
#findings: neither minor or new are statistically significant in determining amount granted

#proportion of status based on minor or not

table_sm <- table(patients$Minor,patients$Status)
proportions(table_sm, margin=1)

#findings: higher prop of declined apps for non-minors. higher prop of withdrawn apps for minors.

#regression analysis for predicting amount granted (independent variable) based on factors like state, age, and new (dependent variables)

# Fit linear regression model for amount granted by state
amt_granted_state<- lm(Amt_granted ~ State, data = patients)

# Summarize the regression model
summary(amt_granted_state)

#get regression table
get_regression_table(amt_granted_state)
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

#visualize
visualize(null_receive_grant)+
  shade_p_value(obs_stat = obs_receive_grant,
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

#visualize
visualize(null_diff_grant)+
  shade_p_value(obs_stat = obs_diff_grant,
                direction="left")


#p value is 0.771, fail to reject null hypothesis


##################################################################################
###WPP#########

#rename dataset
wpp <- WPP_app_04212024

#convert date column to Date class
wpp$Date <- as.Date(wpp$Date,format="%m/%d/%y")
class(wpp$Date)

#create subsets of wpp applicants from before and after policy change(9/19/23)
wppBefore <- wpp %>% 
  filter(Date<ymd("2023-09-19"))
#87 applicants

wppAfter <- wpp %>% 
  filter(Date >= ymd("2023-09-19"))
#20 applicants

#use bootstrap see if diff in prop of employed applicants before and after

#BOOTSTRAP FOR BEFORE POLICY CHANGE
# find the point estimate from the original sample
sample_wppBefore <- 
  mean(wppBefore$Employed=="Yes")

sample_wppBefore <- wppBefore %>% 
  specify(response=Employed, success="Yes") %>% 
  calculate(stat="prop")
#point estimate is 0.3218391

# generate bootstrap distribution
wppBefore_dist <- wppBefore %>% 
  specify(response=Employed,success="Yes") %>% 
  generate(reps=1000) %>% 
  calculate(stat="prop")

#find 95% CI
# percentile method
ci <- wppBefore_dist %>% 
  get_confidence_interval(type="percentile")

visualize(wppBefore_dist)+
  shade_confidence_interval(endpoints=ci)+
  labs(title="Simulation-Based Bootstrap Distribution for Employed Applicants Before Policy Change")

#BOOTSTRAP FOR AFTER POLICY CHANGE
# find the point estimate from the original sample
sample_wppAfter <- 
  mean(wppAfter$Employed=="Yes")

sample_wppAfter <- wppAfter %>% 
  specify(response=Employed, success="Yes") %>% 
  calculate(stat="prop")
#point estimate is 0.5

# generate bootstrap distribution
wppAfter_dist <- wppAfter %>% 
  specify(response=Employed,success="Yes") %>% 
  generate(reps=1000) %>% 
  calculate(stat="prop")

#find 95% CI
# percentile method
ci <- wppAfter_dist %>% 
  get_confidence_interval(type="percentile")

visualize(wppAfter_dist)+
  shade_confidence_interval(endpoints=ci)+
  labs(title="Simulation-Based Bootstrap Distribution for Employed Applicants After Policy Change")
#very bell shaped curve /normal distribution 

#BOOTSTRAP FOR DIFF IN PROPS
# two sample proportion
wpp <- wpp %>% 
  mutate(policyChange = ifelse(Date<ymd("2023-09-19"),"Before","After"))

employed_diff_dist <- wpp %>% 
  specify(Employed~policyChange, success="Yes") %>% 
  generate(reps=1000) %>% 
  calculate(stat="diff in props",
            order=c("Before","After"))

employed_diff_dist %>% 
  get_confidence_interval(type="percentile")

#negative and positive ci values, no support from the data

###age groups overall 
#bar graph of wpp applicant age groups
ggplot(wpp,aes(x=Age))+
  geom_bar(fill="blue")+
  labs(x="Age Groups of WPP Applicants",y="Number of Applications")
  theme_minimal()
#findings: 26-35 is the most prominent age group

##bar graph of age groups and gender
ggplot(wpp,aes(x=Age,fill=Gender))+
  geom_bar(position="dodge")+
  labs(x="Age Groups of WPP Applicants",y="Number of Applications")
  theme_minimal()
#findings: signifizantly higher proportion of females for every age group

###age groups before and after policy change 
#bar graph of wpp applicant age groups
ggplot(wpp,aes(x=Age))+
  geom_bar(fill="blue")+
  labs(x="Age Groups of WPP Applicants",y="Number of Applications")+
  facet_wrap(~policyChange)
  theme_minimal()

##bar graph of age groups and gender
ggplot(wpp,aes(x=Age,fill=Gender))+
  geom_bar(position="dodge")+
  labs(x="Age Groups of WPP Applicants",y="Number of Applications")+
  facet_wrap(~policyChange)
  theme_minimal()
  
table_ag <- table(wpp$Age,wpp$Gender, wpp$policyChange)
  proportions(table_ag,margin=1)

#findings: non binary applicant was before policy change. no applicants of 46-55 age group after policy change

  
# contributed by ryan: extra year
wppBefore <- wppBefore %>% 
  mutate(year=year(Date),
         month=month(Date), 
         day=day(Date))

wppAfter<- wppAfter %>% 
  mutate(year=year(Date),
         month=month(Date), 
         day=day(Date))


#applications by month before and after policy change 

#add app_month to group all same months over years together before policy change
applications_by_month_before <- wppBefore %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarise(count = n())

wppBefore <- wppBefore %>% 
  mutate(app_month=ifelse(month==1,"January",
                          ifelse(month==2,"February",
                          ifelse(month==3,"March",
                          ifelse(month==4,"April",
                          ifelse(month==5,"May",
                          ifelse(month==6,"June",
                          ifelse(month==7,"July",
                          ifelse(month==8,"August",
                          ifelse(month==9,"September",
                          ifelse(month==10,"October",
                          ifelse(month==11,"November","December"))))))))))))

#visualize
ggplot(wppBefore, aes(x = app_month)) +
  geom_bar(fill="lightblue") +
  labs(
    title = "Number of Applications per Month Before Policy Change",
    x = "Month",
    y = "Number of Applications"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size=10, angle = 45, vjust = 0.5, hjust = 0.5))

#findings: aug 2020 and nov 2020 are notable months for application #. data is skewed right - significantly less apps in 2023

#add app_month to group all same months over years together after policy change
applications_by_month_after <- wppAfter %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarise(count = n())

wppAfter <- wppAfter %>% 
  mutate(app_month=ifelse(month==1,"January",
                          ifelse(month==2,"February",
                                 ifelse(month==3,"March",
                                        ifelse(month==4,"April",
                                               ifelse(month==5,"May",
                                                      ifelse(month==6,"June",
                                                             ifelse(month==7,"July",
                                                                    ifelse(month==8,"August",
                                                                           ifelse(month==9,"September",
                                                                                  ifelse(month==10,"October",
                                                                                         ifelse(month==11,"November","December"))))))))))))

#visualize
ggplot(wppAfter, aes(x = app_month)) +
  geom_bar(fill="pink") +
  labs(
    title = "Number of Applications per Month After Policy Change",
    x = "Month",
    y = "Number of Applications"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size=10, angle = 45, vjust = 0.5, hjust = 0.5))
#november 2023 notable month for app 

############## initial and exit self surveys ##################

##how many applicants did not complete the program (no end date)
noExit <- filter(Ini_ext_self,is.na(Date_end))
#34 our of 53 applicants did not fill out exit survey

#how long on average did patients stay in the program
# Convert Date_ini and Date_ex to proper date format
Ini_ext_self$Date_in <- as.Date(Ini_ext_self$Date_in,format="%m/%d/%y")
Ini_ext_self$Date_end <- as.Date(Ini_ext_self$Date_end,format="%m/%d/%y")

# Calculate duration of participation

Ini_ext_self <- Ini_ext_self %>%
  mutate(programLength = Ini_ext_self$Date_end - Ini_ext_self$Date_in)

# Find average length
averageLength<- mean(Ini_ext_self$programLength, na.rm = TRUE)
print(averageLength)
#411 days

#visualize spread of program length
ggplot(Ini_ext_self,aes(x=programLength))+
  geom_histogram(fill="lightgreen")+
  labs(x="Time Participants stayed in Program",y="Number of Participants")
theme_minimal()

#mutate question answers to be on scale from 1-4 (1 = strongly disagree, 4 = strongly agree)
Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q1num = ifelse(Q1 == "Strongly disagree", "1",
                        ifelse(Q1 == "Disagree", "2",
                               ifelse(Q1 == "Agree", "3", "4"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q2num = ifelse(Q2 == "Strongly disagree", "4",
                        ifelse(Q2 == "Disagree", "3",
                               ifelse(Q2 == "Agree", "2", "1"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q3num = ifelse(Q3 == "Strongly disagree", "1",
                        ifelse(Q3 == "Disagree", "2",
                               ifelse(Q3 == "Agree", "3", "4"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q4num = ifelse(Q4 == "Strongly disagree", "1",
                        ifelse(Q4 == "Disagree", "2",
                               ifelse(Q4 == "Agree", "3", "4"))))
Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q5num = ifelse(Q5 == "Strongly disagree", "4",
                        ifelse(Q5 == "Disagree", "3",
                               ifelse(Q5 == "Agree", "2", "1"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q6num = ifelse(Q6 == "Strongly disagree", "4",
                        ifelse(Q6 == "Disagree", "3",
                               ifelse(Q6 == "Agree", "2", "1"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q7num = ifelse(Q7 == "Strongly disagree", "1",
                        ifelse(Q7 == "Disagree", "2",
                               ifelse(Q7 == "Agree", "3", "4"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q8num = ifelse(Q8 == "Strongly disagree", "4",
                        ifelse(Q8 == "Disagree", "3",
                               ifelse(Q8 == "Agree", "2", "1"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q9num = ifelse(Q9 == "Strongly disagree", "4",
                        ifelse(Q9 == "Disagree", "3",
                               ifelse(Q9 == "Agree", "2", "1"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q10num = ifelse(Q10 == "Strongly disagree", "1",
                        ifelse(Q10 == "Disagree", "2",
                               ifelse(Q10 == "Agree", "3", "4"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q11num = ifelse(Q11 == "Very confident", "3",
                        ifelse(Q11 == "Somewhat confident", "2", "1")))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q1Enum = ifelse(Q1e == "Strongly disagree", "1",
                        ifelse(Q1e == "Disagree", "2",
                               ifelse(Q1e == "Agree", "3", "4"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q2Enum = ifelse(Q2e == "Strongly disagree", "4",
                        ifelse(Q2e == "Disagree", "3",
                               ifelse(Q2e == "Agree", "2", "1"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q3Enum = ifelse(Q3e == "Strongly disagree", "1",
                        ifelse(Q3e == "Disagree", "2",
                               ifelse(Q3e == "Agree", "3", "4"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q4Enum = ifelse(Q4e == "Strongly disagree", "1",
                        ifelse(Q4e == "Disagree", "2",
                               ifelse(Q4e == "Agree", "3", "4"))))
Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q5Enum = ifelse(Q5e == "Strongly disagree", "4",
                        ifelse(Q5e == "Disagree", "3",
                               ifelse(Q5e == "Agree", "2", "1"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q6Enum = ifelse(Q6e == "Strongly disagree", "4",
                        ifelse(Q6e == "Disagree", "3",
                               ifelse(Q6e == "Agree", "2", "1"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q7Enum = ifelse(Q7e == "Strongly disagree", "1",
                        ifelse(Q7e == "Disagree", "2",
                               ifelse(Q7e == "Agree", "3", "4"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q8Enum = ifelse(Q8e == "Strongly disagree", "4",
                        ifelse(Q8e == "Disagree", "3",
                               ifelse(Q8e == "Agree", "2", "1"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q9Enum = ifelse(Q9e == "Strongly disagree", "4",
                        ifelse(Q9e == "Disagree", "3",
                               ifelse(Q9e == "Agree", "2", "1"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q10Enum = ifelse(Q10e == "Strongly disagree", "1",
                         ifelse(Q10e == "Disagree", "2",
                                ifelse(Q10e == "Agree", "3", "4"))))

Ini_ext_self <- Ini_ext_self %>% 
  mutate(Q11Enum = ifelse(Q11e == "Very confident", "3",
                         ifelse(Q11e == "Somewhat confident", "2", "1")))

#convert variable to numeric
Ini_ext_self$Q1num<- as.numeric(Ini_ext_self$Q1num)
Ini_ext_self$Q2num<- as.numeric(Ini_ext_self$Q2num)
Ini_ext_self$Q3num<- as.numeric(Ini_ext_self$Q3num)
Ini_ext_self$Q4num<- as.numeric(Ini_ext_self$Q4num)
Ini_ext_self$Q5num<- as.numeric(Ini_ext_self$Q5num)
Ini_ext_self$Q6num<- as.numeric(Ini_ext_self$Q6num)
Ini_ext_self$Q7num<- as.numeric(Ini_ext_self$Q7num)
Ini_ext_self$Q8num<- as.numeric(Ini_ext_self$Q8num)
Ini_ext_self$Q9num<- as.numeric(Ini_ext_self$Q9num)
Ini_ext_self$Q10num<- as.numeric(Ini_ext_self$Q10num)
Ini_ext_self$Q11num<- as.numeric(Ini_ext_self$Q11num)
Ini_ext_self$Q1Enum<- as.numeric(Ini_ext_self$Q1Enum)
Ini_ext_self$Q2Enum<- as.numeric(Ini_ext_self$Q2Enum)
Ini_ext_self$Q3Enum<- as.numeric(Ini_ext_self$Q3Enum)
Ini_ext_self$Q4Enum<- as.numeric(Ini_ext_self$Q4Enum)
Ini_ext_self$Q5Enum<- as.numeric(Ini_ext_self$Q5Enum)
Ini_ext_self$Q6Enum<- as.numeric(Ini_ext_self$Q6Enum)
Ini_ext_self$Q7Enum<- as.numeric(Ini_ext_self$Q7Enum)
Ini_ext_self$Q8Enum<- as.numeric(Ini_ext_self$Q8Enum)
Ini_ext_self$Q9Enum<- as.numeric(Ini_ext_self$Q9Enum)
Ini_ext_self$Q10Enum<- as.numeric(Ini_ext_self$Q10Enum)
Ini_ext_self$Q11Enum<- as.numeric(Ini_ext_self$Q11Enum)

#add up values of responses to determine self esteem level
Ini_ext_self <- Ini_ext_self %>% 
  mutate(Ini_total = Ini_ext_self$Q1num + Ini_ext_self$Q2num + Ini_ext_self$Q3num+ Ini_ext_self$Q4num + Ini_ext_self$Q5num + Ini_ext_self$Q6num + Ini_ext_self$Q7num + Ini_ext_self$Q8num + Ini_ext_self$Q9num + Ini_ext_self$Q10num + Ini_ext_self$Q11num )
  
Ini_ext_self <- Ini_ext_self %>% 
  mutate(Exit_total = Ini_ext_self$Q1Enum + Ini_ext_self$Q2Enum + Ini_ext_self$Q3Enum+ Ini_ext_self$Q4Enum + Ini_ext_self$Q5Enum + Ini_ext_self$Q6Enum + Ini_ext_self$Q7Enum + Ini_ext_self$Q8Enum + Ini_ext_self$Q9Enum + Ini_ext_self$Q10Enum + Ini_ext_self$Q11Enum )

clean_Ini_ext <- Ini_ext_self[!is.na(Ini_ext_self$Exit_total),]
clean_Ini_ext <- clean_Ini_ext[!is.na(clean_Ini_ext$Ini_total),]

clean_Ini_ext <- clean_Ini_ext %>% 
  mutate(policyChange = ifelse(Date_in<ymd("2023-09-19"),"Before","After"))
clean_Ini_ext <- clean_Ini_ext %>% 
  mutate(changeInSE = clean_Ini_ext$Exit_total - clean_Ini_ext$Ini_total)

#t test to compare self esteem scores before and after the program
difference <- t.test(clean_Ini_ext$Ini_total,clean_Ini_ext$Exit_total)
view(difference)
#p value is 0.937, difference is not statistically significant 
