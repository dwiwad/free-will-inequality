rm(list = ls()) # clear enviroment
#load packages
require(foreign)
require(dplyr)
require(QuantPsyc)
require(ggplot2)
require(ggrepel)
require(heplots)
require(mediation)
require(ggthemes)
require(psych)
require(foreign)
library(lsr)
require(pwr)

 #set working directory to dropbox folder, e.g.:
setwd("C:/Users/merci/Dropbox/Free Will Inequality")

#--------------------------------------------------------------------------------------------------------
#Study 1
#--------------------------------------------------------------------------------------------------------
data1 <- read.csv("FW Study 1.csv") #read in data. For information on how summary statistics were calculated by country, and how data were cleaned for study 1, see the accompanying file: "Cleaning and joining data for study 1.r"

#regression with FW and support for inequality
lm(support_inequality ~ FW, data = data1) %>% summary()


#regression
model <- lm(support_inequality ~ FW + Mobility + gini + gdp_pc, data = data1)
summary(model)
round(model$coefficients, 2)

#prepare theme for graph
theme_update(plot.title = element_text(size = 22, hjust = 0.5),
             axis.title = element_text(size = 20),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 16), 
             legend.title = element_text(size = 16), 
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())

#best so far: 

ggplot(data1, aes(x = FW, y = support_inequality, label=Country)) +
  geom_point(size = 2, shape = 21, fill = "Blue") + 
  geom_text(fontface = "bold", hjust = -0.1) +
  ggtitle("Support for Inequality and Belief in Free Will by Country") + 
  ylab("Support for Inequality") + 
  xlab("Free Will Belief") + 
  geom_smooth(method = "lm", color = "Blue", fullrange = T, se = T, size = 1.25, linetype = "solid") +
  coord_cartesian(xlim = c(6., 8))  

#--------------------------------------------------------------------------------------------------------
#Study 2
#--------------------------------------------------------------------------------------------------------
data2 <- read.csv("Study 2.csv", header = TRUE)

#Measures

#Support for Inequality
#recode reverse scored questions
data2$support_inequality1 <- (8 - data2$support_inequality1)
data2$support_inequality2 <- (8 - data2$support_inequality2)
data2$support_inequality3 <- (8 - data2$support_inequality3)

#create scale
col.ineq <- c("support_inequality1", "support_inequality2","support_inequality3", "support_inequality4", "support_inequality5")

data2$support_inequality <- rowMeans(data2[, col.ineq])

#Scale stats
psych::alpha(data2[,col.ineq]) %>% summary()
mean(data2$support_inequality, na.rm = T)
sd(data2$support_inequality, na.rm = T)

 #Free Will

#Free Will (columns 74 to 76)

col.fw <- c("free_will1", "free_will2", "free_will3", "free_will4", "free_will5")
data2$FWI_FW <- rowMeans(data2[, col.fw])
psych::alpha(data2[,col.fw]) %>% summary()
mean(data2$FWI_FW, na.rm = T)
sd(data2$FWI_FW, na.rm = T)

#Political ideology 
#8 and 9 are don't know/other, so recode as NA
data2$ideology[data2$ideology >= 8] <-  NA
mean(data2$ideology, na.rm = T)
sd(data2$ideology, na.rm = T)

# income
mean(data2$income, na.rm = T)
sd(data2$income, na.rm = T)

#gender
table(data2$gender) #1 = male, 2 = female, 3 =other, 4 = prefer not to say
#percentages of participants identifying as each gender
table(data2$gender)[1]/sum(table(data2$gender))
table(data2$gender)[2]/sum(table(data2$gender))
table(data2$gender)[3]/sum(table(data2$gender))
table(data2$gender)[4]/sum(table(data2$gender))

#to exclude those selecting prefer not to say or other from future analysis, replace this data with NA 
data2$gender[data2$gender >= 3] <- NA
#make gender variable a factor
data2$gender <- as.factor(data2$gender)

#other demographics - age
mean(data2$age)
sd(data2$age)

##Analysis
#Regression on support for inequality
lm(support_inequality ~ FWI_FW, data = data2) %>% summary()

model2 <- lm(support_inequality ~ FWI_FW + income + gender + age + ideology, data = data2)
summary(model2)

  #--------------------------------------------------------------------------------------------------------
#Study 3
#--------------------------------------------------------------------------------------------------------

#power analysis
power <- pwr.t.test(d = .2, power = .9, sig.level = .05, type = "two.sample", alternative = "greater")
# *2 to get total n (n from pwr.t.test is for one condition)
power$n * 2 
# *(1/.85) to account for expected 15% attrition
power$n * 2 * (1/.85)

#read in data
data3 <- read.spss("Study 3.sav",  to.data.frame = T, use.value.labels = F)

#participants
#age (exclude 2 participants who reported "1990" or 5 for age)
filter(data3, age != c(1990, 5))$age %>% mean(na.rm = T)
filter(data3, age != c(1990, 5))$age %>% sd(na.rm = T)

#sex
table(data3$sex) # 1 = male, 2 =  female, 3 = other/prefer not to say 

#materials
#make condition a factor
data3$Condition<- factor(data3$Condition, labels = c("Pro FW", "Anti FW"))



#Attention check
#indicate which participants passed the attention check, 0 for failed, 1 for passed
data3$attentionCheck <- 0
#in Pro Free Will condition response "6" is correct, in Anti free will condition response 5 is correct
data3$attentionCheck[(data3$Condition == "Pro FW") & (data3$essay_attention == 6) | (data3$Condition == "Anti FW") & (data3$essay_attention == 5)] <- 1


#compare failure rate by condition
summary_attention_check <- group_by(data3, Condition) %>%
  summarise(n = length(Condition), passed = sum(attentionCheck), failed = n - passed, percent_failed = failed/n)

summary_attention_check 

#chi squared test
prop.test(summary_attention_check$failed, summary_attention_check$n)

#Analyses reported in the paper are with all participants included. To exclude participants who failed the attention check, run the code below before running analysis:
#data3 <- filter(data3, attentionCheck == 1)

#Support for Inequality

#recode reverse scored questions
data3$Inequality_2 <- (8 - data3$Inequality_2)
data3$Inequality_3 <- (8 - data3$Inequality_3)
data3$Inequality_5 <- (8 - data3$Inequality_5)


#create scale
col.ineq <- c("Inequality_1", "Inequality_2","Inequality_3", "Inequality_4", "Inequality_5")

data3$support_inequality <- rowMeans(data3[, col.ineq])

#Scale stats - reported in Study 2, but if desired run code below:
#psych::alpha(data3[,col.ineq]) %>% summary()
#mean(data3$support_inequality, na.rm = T)
#sd(data3$support_inequality, na.rm = T)

#Free Will
col.fw <- c("FWI_FW_1", "FWI_FW_2", "FWI_FW_3", "FWI_FW_4", "FWI_FW_5")
data3$FWI_FW <- rowMeans(data3[, col.fw])
#psych::alpha(data3[,col.fw]) %>% summary()
#mean(data3$FWI_FW, na.rm = T)
#sd(data3$FWI_FW, na.rm = T)

#Support for Meritocracy

col.m <- c("meritocracy_1", "meritocracy_2", "meritocracy_3", "meritocracy_4")
data3$meritocracy <- rowMeans(data3[, col.m])
psych::alpha(data3[,col.m]) %>% summary()
mean(data3$meritocracy, na.rm = T)
sd(data3$meritocracy, na.rm = T)

#support for redistribution

col.r <- c("redistribution_1", "redistribution_2", "redistribution_3")
data3$redistribution <- rowMeans(data3[, col.r])
psych::alpha(data3[,col.r]) %>% summary()
mean(data3$redistribution)
sd(data3$redistribution)

#Explanations of economic inequality
#reverse score dispositional items 
data3$reason_inheritance <- 6 - data3$reason_inheritance
data3$reason_political_influence <- 6 - data3$reason_political_influence
data3$reason_economic_structure <- 6 - data3$reason_economic_structure
data3$reason_personal_background <- 6 - data3$reason_personal_background
data3$reason_educational_opportunity <- 6 - data3$reason_educational_opportunity
data3$reason_wages <- 6 - data3$reason_wages
data3$reason_prejudice <- 6 - data3$reason_prejudice


col.explain <- c("reason_inheritance", "reason_political_influence", "reason_economic_structure", "reason_personal_background", "reason_educational_opportunity", "reason_wages", "reason_prejudice", "reason_ambition", "reason_ability", "reason_hard_work", "reason_effort", "reason_money_management")

data3$contextual <- rowMeans(data3[, col.explain])
psych::alpha(data3[,col.explain]) %>% summary()
mean(data3$contextual, na.rm = T)
sd(data3$contextual, na.rm = T)

##Analysis
#Manipulation check
#t test
t.test(FWI_FW ~ Condition, alternative  = "greater", data = data3)
#group means and standard deviations
group_by(data3, Condition) %>% summarise(mean = mean(FWI_FW), sd = sd(FWI_FW), n = n())
#effect size and 95%CI
cohensD(FWI_FW ~ Condition, method = "unequal", data = data3) %>% cohen.d.ci(n1 = 486, n2 = 524, alpha = .05)

#Is support for inequality different by condition?  
t.test(support_inequality ~ Condition, alternative  = "greater", data = data3)
group_by(data3, Condition) %>% summarise(mean = mean(support_inequality), sd = sd(support_inequality), n = n())
cohensD(support_inequality ~ Condition, method = "unequal", data = data3) %>% cohen.d.ci(n1 = 486, n2 = 524, alpha = .05)
#test for indirect effect of dispositional explanations for inequality on support for inequality
mid.fit<- lm(contextual ~ Condition, data = data3) #A path          
out.fit<- lm(support_inequality ~ contextual + Condition, data = data3) #B path        
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "contextual", robustSE = F, sims = 1000) 
summary(med.est)
med.est$d1.p #get p-value to 3 digits

#Is support for meritocracy different by condition 
t.test(meritocracy ~ Condition, alternative = "greater", data = data3)
group_by(data3, Condition) %>% summarise(mean = mean(meritocracy), sd = sd(meritocracy), n = n())
cohensD(meritocracy ~ Condition, method = "unequal", data = data3) %>% cohen.d.ci(n1 = 486, n2 = 524, alpha = .05)

#test for indirect effect of dispositional explanations for inequality on support for meritocracy
mid.fit<- lm(contextual ~ Condition, data = data3) #A path          
out.fit<- lm(meritocracy ~ contextual + Condition, data = data3) #B path        
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "contextual", robustSE = F, sims = 1000) 
summary(med.est)
med.est$d1.p 

#Is support for redistribution different by condition
t.test(redistribution~ Condition, alternative = "less", data = data3)
group_by(data3, Condition) %>% summarise(mean = mean(redistribution), sd = sd(redistribution), n = n())
cohensD(redistribution ~ Condition, method = "unequal", data = data3) %>% cohen.d.ci(n1 = 486, n2 = 524, alpha = .05)


#Exploratory Analysis
#regression predicting support for inequality from FW (those selecting something other than Male or Female are exclude from this analysis, as in study 2)
lm(support_inequality~ FWI_FW + age + sex + conservatism, data = filter(data3, sex %in% c(1, 2))) %>% summary()
lm(support_inequality~ FWI_FW + age + sex + conservatism, data = filter(data3, sex %in% c(1, 2))) %>%
  lm.beta() %>%
  round(3) #standardized coeffcients 

#test mediation of free will on support for inequality 
mid.fit<- lm(FWI_FW ~ Condition + conservatism + ladder + sex + age, data = data3_sex)          
out.fit<- lm(support_inequality ~ FWI_FW + Condition + conservatism + ladder + sex + age, data = data3_sex)        
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)


mid.fit %>% summary()
lm(support_inequality ~ FWI_FW, data = data3) %>% summary
out.fit %>% summary()
lm(support_inequality ~ Condition, data = data3) %>% summary

library(diagram)
data <- c(0, "'B = -.22, p = .006'", 0,
          0, 0, 0, 
          "'B = .19, p <.001'", "'B = -.01, p = .883 (B = -0.04, p = .002)'", 0)
M<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data)
plot<- plotmat (M, pos=c(1,2), 
                name= c( "Free Will","Condition", "Support for Inequality"), 
                box.type = "rect", box.size = 0.07, box.prop=0.5,  curve=0, cex = 1.2, box.cex = 1, shadow.size = 0, arr.length = 0.3)

#test mediation controlling for covariates 

data3_sex <- filter(data3, sex %in% c(1, 2)) #remove 
mid.fit<- lm(FWI_FW ~ Condition + conservatism + ladder + sex + age, data = data3_sex)          
out.fit<- lm(support_inequality ~ FWI_FW + Condition + conservatism + ladder + sex + age, data = data3_sex)        
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)



#split by political beliefs 
data3$politics[(data3$conservatism < 4)]<- "Liberal"
data3$politics[(data3$conservatism > 4)]<- "Conservative"
data3$politics <- as.factor(data3$politics)

liberals <- filter(data3, politics == "Liberal") 
conservatives <- filter(data3, politics == "Conservative")

#for conservatives:

#Is support for inequality different by condition?  
t.test(support_inequality ~ Condition, alternative  = "greater", data = conservatives)
group_by(conservatives, Condition) %>% summarise(mean = mean(support_inequality), sd = sd(support_inequality), n = n())
cohensD(support_inequality ~ Condition, method = "unequal", data = conservatives) %>% cohen.d.ci(n1 = 116, n2 = 148, alpha = .05)

#for Liberals:

#Is support for inequality different by condition?  
t.test(support_inequality ~ Condition, alternative  = "greater", data = liberals)
group_by(liberals, Condition) %>% summarise(mean = mean(support_inequality), sd = sd(support_inequality), n = n())
cohensD(support_inequality ~ Condition, method = "unequal", data = liberals) %>% cohen.d.ci(n1 = 243, n2 = 260, alpha = .05)


#--------------------------------------------------------------------------------------------------------
#Study 4
#--------------------------------------------------------------------------------------------------------
#read in data
data4 <- read.spss("Study 4.sav", to.data.frame = T,  use.value.labels = F)

#Participants
#politics
#recode to categorical variable
data4$politics[(data4$conservatism %in% c(1, 2, 3))]<- "Liberal"
data4$politics[(data4$conservatism %in% c(5, 6, 7))]<- "Conservative"
data4$politics <- as.factor(data4$politics)

table(data4$politics)

#gender 1 = Male, 2 = Female, 3 = A gender not specified above
table(data4$gender)

#age (excluding one participant who reported "1989" as age)
filter(data4, age != 1988)$age %>% mean(na.rm = T) 
filter(data4, age != 1988)$age %>% sd(na.rm = T)

#materials
#make condition a factor
data4$Condition<- factor(data4$Condition, labels = c("Pro FW", "Anti FW"))


#results
#Essay Attention check
#indicate which participants passed the attention check, 0 for failed, 1 for passed
data4$attentionCheck <- 0
#in Pro Free Will condition response "6" is correct, in Anti free will condition response 5 is correct
data4$attentionCheck[(data4$Condition == "Pro FW") & (data4$essay_attention == 6) | (data4$Condition == "Anti FW") & (data4$essay_attention == 5)] <- 1


#compare failure rate by condition
summary_attention_check <- group_by(data4, Condition) %>%
  summarise(n = length(Condition), passed = sum(attentionCheck), failed = n - passed, percent_failed = failed/n)

summary_attention_check 

#chi squared test
prop.test(summary_attention_check$failed, summary_attention_check$n)

# General Attention check
#indicate which participants passed the attention check, 0 for failed, 1 for passed
data4$generalAttentionCheck <- 0
data4$generalAttentionCheck[(data4$general_attention == 6)] <- 1

#compare failure rate by condition
summary_general_check <- group_by(data4, Condition) %>%
  summarise(n = length(Condition), passed = sum(generalAttentionCheck), failed = n - passed, percent_failed = failed/n)

summary_general_check 

#chi squared test
prop.test(summary_general_check$failed, summary_general_check$n)

#exclude participants who failed attention check 
data4 <- filter(data4, attentionCheck == 1, generalAttentionCheck == 1)


#Support for Inequality
#recode reverse scored questions
data4$Inequality_2 <- (8 - data4$Inequality_2)
data4$Inequality_3 <- (8 - data4$Inequality_3)
data4$Inequality_5 <- (8 - data4$Inequality_5)


#create scale
col.ineq <- c("Inequality_1", "Inequality_2","Inequality_3", "Inequality_4", "Inequality_5")

data4$support_inequality <- rowMeans(data4[, col.ineq])

#Scale stats - reported in Study 2, but if desired run code below:
#psych::alpha(data4[,col.ineq]) %>% summary()
#mean(data4$support_inequality, na.rm = T)
#sd(data4$support_inequality, na.rm = T)

#Free Will
col.fw <- c("FWI_FW_1", "FWI_FW_2", "FWI_FW_3", "FWI_FW_4", "FWI_FW_5")
data4$FWI_FW <- rowMeans(data4[, col.fw])
#psych::alpha(data4[,col.fw]) %>% summary()
#mean(data4$FWI_FW, na.rm = T)
#sd(data4$FWI_FW, na.rm = T)

#Support for Meritocracy

col.m <- c("meritocracy_1", "meritocracy_2", "meritocracy_3", "meritocracy_4")
data4$meritocracy <- rowMeans(data4[, col.m])
#psych::alpha(data4[,col.m]) %>% summary()
#mean(data4$meritocracy, na.rm = T)
#sd(data4$meritocracy, na.rm = T)

#support for redistribution

col.r <- c("redistribution_1", "redistribution_2", "redistribution_3")
data4$redistribution <- rowMeans(data4[, col.r])
#psych::alpha(data4[,col.r]) %>% summary()
#mean(data4$redistribution)
#sd(data4$redistribution)

#Explanations of economic inequality
#reverse score dispositional items 
data4$reason_inheritance <- 6 - data4$reason_inheritance
data4$reason_political_influence <- 6 - data4$reason_political_influence
data4$reason_economic_structure <- 6 - data4$reason_economic_structure
data4$reason_personal_background <- 6 - data4$reason_personal_background
data4$reason_educational_opportunity <- 6 - data4$reason_educational_opportunity
data4$reason_wages <- 6 - data4$reason_wages
data4$reason_prejudice <- 6 - data4$reason_prejudice


col.explain <- c("reason_inheritance", "reason_political_influence", "reason_economic_structure", "reason_personal_background", "reason_educational_opportunity", "reason_wages", "reason_prejudice", "reason_ambition", "reason_ability", "reason_hard_work", "reason_effort", "reason_money_management")

data4$contextual <- rowMeans(data4[, col.explain])
#psych::alpha(data4[,col.explain]) %>% summary()
#mean(data4$contextual, na.rm = T)
#sd(data4$contextual, na.rm = T)


#Manipulation check
#t test
t.test(FWI_FW ~ Condition, alternative  = "greater", data = data4)
#group means and standard deviations
group_by(data4, Condition) %>% summarise(mean = mean(FWI_FW), sd = sd(FWI_FW), n = n())
#effect size and 95%CI
cohensD(FWI_FW ~ Condition, method = "unequal", data = data4) %>% cohen.d.ci(n1 = 348, n2 = 370, alpha = .05)


#Analysis
#Is support for inequality different by condition?  
t.test(support_inequality ~ Condition, alternative  = "greater", data = data4)
group_by(data4, Condition) %>% summarise(mean = mean(support_inequality), sd = sd(support_inequality), n = n())
cohensD(support_inequality ~ Condition, method = "unequal", data = data4) %>% cohen.d.ci(n1 = 168, n2 = 178, alpha = .05)

#Indirect effect of condition on support for inequality through belief in free will 
data4_gender <- filter(data4, data4$gender %in% c(1, 2))

mid.fit<- lm(FWI_FW ~ Condition  + conservatism + ladder + age + gender, data = data4_gender)          
out.fit<- lm(support_inequality ~ FWI_FW + Condition + conservatism + ladder + age + gender, data = data4_gender)       
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)

mid.fit<- lm(FWI_FW ~ Condition  + conservatism + ladder + age + gender, data = data4)          
out.fit<- lm(support_inequality ~ FWI_FW + Condition + conservatism + ladder + age + gender, data = data4)       
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)

#split by politics
liberals <- filter(data4, politics == "Liberal")
liberals_gender <- filter(liberals, liberals$gender %in% c(1, 2))
conservatives <- filter(data4, politics == "Conservative")

#Is support for inequality different by condition for conservatives?  
t.test(support_inequality ~ Condition, alternative  = "greater", data = conservatives)
group_by(conservatives, Condition) %>% summarise(mean = mean(support_inequality), sd = sd(support_inequality), n = n())
cohensD(support_inequality ~ Condition, method = "unequal", data = conservatives) %>% cohen.d.ci(n1 = 168, n2 = 178, alpha = .05)

#test mediation of free will on support for inequality for conservatives
mid.fit<- lm(FWI_FW ~ Condition + ladder + age + gender, data = conservatives)         
out.fit<- lm(support_inequality ~ FWI_FW + Condition + ladder + age + gender, data = conservatives)     
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)

#Is support for inequality different by condition for liberals?  
t.test(support_inequality ~ Condition, alternative  = "greater", data = liberals)
group_by(liberals, Condition) %>%  summarise(mean = mean(support_inequality), sd = sd(support_inequality), n = n())
cohensD(support_inequality ~ Condition, method = "unequal", data = liberals) %>% cohen.d.ci(n1 = 243, n2 = 260, alpha = .05)

#test mediation of free will on support for inequality for liberals 
mid.fit<- lm(FWI_FW ~ Condition + ladder + age + gender, data = liberals_gender)          
out.fit<- lm(support_inequality ~ FWI_FW + Condition + ladder + age + gender, data = liberals_gender)   
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)

#support for redistribution
#Is support for redistribution different by condition?

t.test(redistribution~ Condition, alternative = "less", data = data4)
group_by(data4, Condition) %>% summarise(mean = mean(redistribution), sd = sd(redistribution), n = n())
cohensD(redistribution ~ Condition, method = "unequal", data = conservatives) %>% cohen.d.ci(n1 = 348, n2 = 370, alpha = .05)
#indirect effect
mid.fit<- lm(FWI_FW ~ Condition  + conservatism + ladder + age + gender, data = data4_gender)          
out.fit<- lm(redistribution~ FWI_FW + Condition + conservatism + ladder + age + gender, data = data4_gender)       
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)
#Is support for redistribution different by condition for conservatives

t.test(redistribution~ Condition, alternative = "less", data = conservatives)
group_by(conservatives, Condition) %>% summarise(mean = mean(redistribution), sd = sd(redistribution), n = n())
cohensD(redistribution ~ Condition, method = "unequal", data = conservatives) %>% cohen.d.ci(n1 = 116, n2 = 148, alpha = .05)

#Is support for redistribution different by condition for liberals

t.test(redistribution~ Condition, alternative = "less", data = liberals)
group_by(liberals, Condition) %>% summarise(mean = mean(redistribution), sd = sd(redistribution), n = n())
cohensD(redistribution ~ Condition, method = "unequal", data = liberals) %>% cohen.d.ci(n1 = 243, n2 = 260, alpha = .05)

#indirect effect
mid.fit<- lm(FWI_FW ~ Condition + ladder + age + gender, data = conservatives)          
out.fit<- lm(redistribution~ FWI_FW + Condition + ladder + age + gender, data = conservatives)    
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)

#indirect effect
mid.fit<- lm(FWI_FW ~ Condition + ladder + age + gender, data = liberals_gender)          
out.fit<- lm(redistribution~ FWI_FW + Condition + ladder + age + gender, data = liberals_gender)    
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)


#test mediation of free will on support for inequality 
mid.fit<- lm(FWI_FW ~ Condition + gender + ladder + conservatism, data = data4_gender)          
out.fit<- lm(support_inequality ~ FWI_FW + Condition + gender + ladder + conservatism, data = data4_gender)         
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)

mid.fit<- lm(FWI_FW ~ Condition, data = data4_gender)          
out.fit<- lm(support_inequality ~ FWI_FW + Condition, data = data4_gender)         
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)


#test mediation of free will on support for inequality for conservatives
mid.fit<- lm(FWI_FW ~ Condition, data = conservatives)         
out.fit<- lm(support_inequality ~ FWI_FW + Condition, data = conservatives)        
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)


#test mediation of free will on support for inequality for liberals 
mid.fit<- lm(FWI_FW ~ Condition, data = liberals)          
out.fit<- lm(support_inequality ~ FWI_FW + Condition, data = liberals)        
med.est <- mediation::mediate(mid.fit, out.fit, treat ="Condition", mediator = "FWI_FW", robustSE = F, sims = 1000) 
summary(med.est)

lm(support_inequality ~ FWI_FW + gender + ladder + conservatism, data = data4_gender) %>% summary
names(data4)
data4_gender <- filter(data4, data4$gender %in% c(1, 2))

 #Bayes
#tests done in JASP (see screenshots)
#to get data for JASP, run code below, which will save a csv file to your working directory
#x <- dplyr::select(data4, Condition, support_inequality)
#write.csv(x,"study4_Bayes.csv" )
#y <- dplyr::select(data3, Condition, support_inequality)
#write.csv(y, "study3_Bayes.csv")


#find "expected effect size" under mediatiton models:

#Study 3

# raw mean difference for change in Free Will belief produced by the manipulation
study3_FWchange <-mean(data3[data3$Condition == "Pro FW", ]$FWI_FW) - mean(data3[data3$Condition == "Anti FW", ]$FWI_FW)
study3_FWchange
#find expected change in support for inequality produced by a one unit change in free will (unstandardized Beta)
study3_beta <- (lm(support_inequality~ FWI_FW + age + sex + conservatism, data = filter(data3, sex %in% c(1, 2))) %>%
  summary())$coefficients["FWI_FW", "Estimate"]

#calculate the expected change in support for inequality based on the observed change in belief in free will (raw score)
study3_SIchange <- study3_FWchange*study3_beta
study3_SIchange
#convert expected change to cohens d (use pooled sd as estimate of expected sd)
group_by(data3, Condition) %>% summarize(mean = mean(support_inequality), sd = sd(support_inequality))
study3_d <- study3_SIchange/sqrt((1.47^2 + 1.46^2)/2)
study3_d

#achived power in study 3 (use assume conditions have equal sample size)
pwr.t.test(d = study3_d, n = nrow(data3)/2, sig.level = .05, type = "two.sample", alternative = "greater")


#Study 4

# raw mean difference for change in Free Will belief produced by the manipulation
study4_FWchange <-mean(data4[data4$Condition == "Pro FW", ]$FWI_FW) - mean(data4[data4$Condition == "Anti FW", ]$FWI_FW)

#find expected change in support for inequality produced by a one unit change in free will (unstandardized Beta)
study4_beta <- (lm(support_inequality~ FWI_FW + age + gender + conservatism, data = filter(data4, gender %in% c(1, 2))) %>% summary())$coefficients["FWI_FW", "Estimate"]
#calculate the expected change in support for inequality based on the observed change in belief in free will (raw score)
study4_SIchange <- study4_FWchange*study4_beta

#convert expected change to cohens d (use pooled sd as estimate of expected sd)
group_by(data4, Condition) %>% summarize(mean = mean(support_inequality), sd = sd(support_inequality))
study4_d <- study4_SIchange/sqrt((1.56^2 + 1.54^2)/2)
study4_d

#achived power in study 3 (use assume conditions have equal sample size)
pwr.t.test(d = study4_d, n = nrow(data4)/2, sig.level = .05, type = "two.sample", alternative = "greater")


#--------------------------------------------------------------------------------------------------------
#Study 5
#--------------------------------------------------------------------------------------------------------
#read in data
rm(list = ls())
data5<- read.spss("Study 5.sav", to.data.frame = T, use.value.labels = F)
#exclude those who fail attention check 
data5<-filter(data5, attentionCheck == "4")
data5 <- tbl_df(data5)
table(data5$Sex) #1 = male, 2 = female, 3 = Other / Prefer not to say
mean(data5$age)
sd(data5$age)

#exclude 3 participants who did not respond to question 
data5 <- filter(data5, !is.na(data5$universesLikeOurs))
data5 <- filter(data5, !is.na(data5$acceptableOur))
lut<- c("1" = "Universe A", "2" = "Universe B")
data5$universesLikeOurs<- lut[data5$universesLikeOurs]

#most people think our Universe is like universe B
table(data5$universesLikeOurs)
365/(365 + 73)

#Condition means and sd's
summarize(data5,meanOur = mean(acceptableOur, na.rm = T), sdOur = sd(acceptableOur, na.rm = T),
          meanA = mean(acceptableA, na.rm = T), sdA = sd(acceptableA, na.rm = T), 
          meanB = mean(acceptableB, na.rm = T), sdB = sd(acceptableB, na.rm = T) )

#Is inequality more acceptable in Universe B than Universe A? - Yes
with(data5, t.test(acceptableB, acceptableA, paired = T))
d <- mean(data5$acceptableB - data5$acceptableA, na.rm = T)/((sd(data5$acceptableA, na.rm = T) + sd(data5$acceptableB, na.rm = T))/2)
cohen.d.ci(d = d, n1 = 436, alpha = .05)

#Is inequality more acceptable in our universe than in Universe A? - Yes
with(data5, t.test(acceptableOur, acceptableA, paired = T))
d_our <- mean(data5$acceptableOur - data5$acceptableA, na.rm = T)/((sd(data5$acceptableOur, na.rm = T) + sd(data5$acceptableB, na.rm = T))/2)
cohen.d.ci(d = d_our, n1 = 436, alpha = .05)
