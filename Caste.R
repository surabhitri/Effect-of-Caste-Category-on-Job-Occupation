###########################################################################
###########################################################################
########################## Caste Inequality ###############################
###########################################################################
###########################################################################

#Loading libraries
library(dplyr)
library(data.table)
library(ggplot2)
library(arm)
library(pROC)
library(e1071)
library(caret)
require(gridExtra)
library(rms) #for VIF
library(nnet)
library(knitr)
library(sjPlot)

#Loading the data
rm(list = ls())
data <- load("/Users/sue/Documents/MIDS/Modeling and Repr of Data/Final Project/36151-0001-Data.rda")
data <- da36151.0001 
rm(da36151.0001)

########Data Cleaning 
#Keeping the variables that we want 
cols <- select(data, c('SURVEY', 'STATEID', 'HHID', 'PERSONID', 'RO3', 'RO4', 'RO5', 'RO6', 'RO7', 'FM1', 'FM36Y', 'AN1', 'AN5Y', 'ED2', 'ED3',
                       'EDUC7', 'MM7Y', 'MM12Y', 'URBAN4_2011', 'METRO', 'ID11', 'ID13', 'GROUPS', 'COPC', 'ASSETS', 'INCOME', 'WS3NM', 'WS4', 'WS5'))
summary(cols)
cols <- cols[complete.cases(cols[ , 22]),]

#removing NAs from the caste category variable
unique(cols[c("WS4")])

#collapsing occupation category into binary variable
cols$occupation_cat <- with(cols, ifelse(WS4 %in% c("(098) Drivers 98", 
                                             "(081) Carpenters 81", "(054) Sweepers 54", "(052) Cooks/waiters 52", "(087) Plumbers/welders 87", 
                                             "(051) House keepers 51", "(071) Miners 71",
                                             "(080) Shoe makers 80", "(053) Maids 53", "(078) Tobacco 78"
                                             ), 'LowPay', ifelse(
  WS4 %in% c("(015) Teachers 15", "(016) Journalists 16", "(008) Nursing 8", "(003) Eng. tech 3", "(002) Engineers 2", "(007) Physicians 7",
             "(005) Life scientists 5", "(012) Accountants 12", "(023) Mgr finance 23", "(014) Lawyers 14", "(001) Physical sci tech 1",
             "(009) Other scientific 9", "(004) Air/ship officers 4", "(013) Social scientists 13", "(006) Life science tech 6",
             "(011) Economists 11", "(010) Statisticians 10", "(029) Managerial nec 29", "(020) Elected officials 20", "(022) Mgr Whsl/retail 22",
             "(024) Mgr manf 24", "(025) Mgr transp/commun 25", "(026) Mgr service 26", "(032) Typists 32", "(034) Computing op 34", ""), 'HighPay', 'remove')))
cols2<-cols[!(cols$occupation_cat=="remove"),]

#renaming columns
setnames(cols2, old = c('RO3','RO4', 'RO5', 'RO6', 'RO7', 'FM1', 'FM36Y', 'AN1', 'AN5Y', 'ED2', 'ED3', 'EDUC7', 'MM7Y', 'MM12Y', 'URBAN4_2011',
                       'ID11', 'ID13', 'COPC'), 
         new = c('sex','rel_to_head', 'age', 'marrital_status', 'primary_act_status',
                                                                         'owned_cultivated', 'own_farm_work', 'owns_livestick',
                                                                         'animal_care_work', 'literacy', 'english_ability', 'educ_level',
                                                                         'uses_computer', 'owns_mobile', 'urban_rural', 'religion',
                 'caste_category', 'household_exp'))

#convert occ category to factor
cols2$occupation_cat <- factor(cols2$occupation_cat)

#collapsing caste into two categories for initial model
cols2$caste_cat_binary <- with(cols2, ifelse(caste_category %in% c('(1) Brahmin 1', '(2) Forward/General (except Brahmin) 2'), 'Forward', 
                                            ifelse(caste_category %in% c('(3) Other Backward Castes (OBC) 3', '(4) Scheduled Castes (SC) 4',
                                                                         '(5) Scheduled Tribes (ST) 5'), 'Backward', 'Other')))
cols2$caste_cat_binary <- factor(cols2$caste_cat_binary)
#removing NAs from household exp and assets
cols2 <- cols2[complete.cases(cols2[ , 24:25]),]
#cols2 <- cols2[complete.cases(cols2[ , 11]),]
#cols2 <- cols2[complete.cases(cols2[ , 13]),]
cols2 <- cols2[complete.cases(cols2[ , 14:16]),]
                                            

########EDA
#EDA for continuous variables
#occupation_cat vs age
ggplot(cols2,aes(x=occupation_cat, y=age, fill=occupation_cat)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Occupaition Category vs Age",
       x="Occupation Category",y="Age") + 
  theme_classic() + theme(legend.position="none")
#no difference in median

#occupation category vs household expenses
ggplot(cols2,aes(x=occupation_cat, y=household_exp, fill=occupation_cat)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Occupaition Category vs Household Expenses",
       x="Occupation Category",y="Household Expenses") + 
  theme_classic() + theme(legend.position="none")
#difference in median which is obvious

#occupation category vs assets
ggplot(cols2,aes(x=occupation_cat, y=ASSETS, fill=occupation_cat)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Occupaition Category vs Assets",
       x="Occupation Category",y="Assets") + 
  theme_classic() + theme(legend.position="none")
#difference in median which is obvious

#occupation category vs income
ggplot(cols2,aes(x=occupation_cat, y=INCOME, fill=occupation_cat)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Occupaition Category vs Income",
       x="Occupation Category",y="Income") + 
  theme_classic() + theme(legend.position="none")
#difference in median which is obvious (will not include this as a predictor but can use it to check correctness of 3rd occ category)

#EDA for categorical variables
#dropping unused levels in STATEID
cols2$STATEID = droplevels(cols2$STATEID)
# occupation_category vs state
apply(table(cols2[,c("occupation_cat","STATEID")])/sum(table(cols2[,c("occupation_cat","STATEID")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","STATEID")]))
#it's significant

# occupation_category vs sex
apply(table(cols2[,c("occupation_cat","sex")])/sum(table(cols2[,c("occupation_cat","sex")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","sex")]))
#it's significant

# occupation_category vs sex
apply(table(cols2[,c("occupation_cat","sex")])/sum(table(cols2[,c("occupation_cat","sex")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","sex")]))
#it's significant

# occupation_category vs marrital_status
apply(table(cols2[,c("occupation_cat","marrital_status")])/sum(table(cols2[,c("occupation_cat","marrital_status")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","marrital_status")]))
#Chi-squared approximation may be incorrect - point this out in ppt

# occupation_category vs owned_cultivated
apply(table(cols2[,c("occupation_cat","owned_cultivated")])/sum(table(cols2[,c("occupation_cat","owned_cultivated")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","owned_cultivated")]))
#it's significant

# occupation_category vs own_farm_work
apply(table(cols2[,c("occupation_cat","own_farm_work")])/sum(table(cols2[,c("occupation_cat","own_farm_work")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","own_farm_work")]))
#it's significant

# occupation_category vs owns_livestock
apply(table(cols2[,c("occupation_cat","owns_livestick")])/sum(table(cols2[,c("occupation_cat","owns_livestick")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","owns_livestick")]))
#it's significant

# occupation_category vs animal_care_work
apply(table(cols2[,c("occupation_cat","animal_care_work")])/sum(table(cols2[,c("occupation_cat","animal_care_work")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","animal_care_work")]))
#it's significant

# occupation_category vs literacy
apply(table(cols2[,c("occupation_cat","literacy")])/sum(table(cols2[,c("occupation_cat","literacy")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","literacy")]))
#it's significant

# occupation_category vs english_ability
apply(table(cols2[,c("occupation_cat","english_ability")])/sum(table(cols2[,c("occupation_cat","english_ability")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","english_ability")]))
#it's significant

# occupation_category vs educ_level
apply(table(cols2[,c("occupation_cat","educ_level")])/sum(table(cols2[,c("occupation_cat","educ_level")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","educ_level")]))
#it's significant

# occupation_category vs uses_computer
apply(table(cols2[,c("occupation_cat","uses_computer")])/sum(table(cols2[,c("occupation_cat","uses_computer")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","uses_computer")]))
#it's significant

# occupation_category vs owns_mobile
apply(table(cols2[,c("occupation_cat","owns_mobile")])/sum(table(cols2[,c("occupation_cat","owns_mobile")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","owns_mobile")]))
#it's significant

# occupation_category vs urban_rural
apply(table(cols2[,c("occupation_cat","urban_rural")])/sum(table(cols2[,c("occupation_cat","urban_rural")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","urban_rural")]))
#it's significant

# occupation_category vs METRO
apply(table(cols2[,c("occupation_cat","METRO")])/sum(table(cols2[,c("occupation_cat","METRO")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","METRO")]))
#it's significant but use urban_rural

# occupation_category vs religion
apply(table(cols2[,c("occupation_cat","religion")])/sum(table(cols2[,c("occupation_cat","religion")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(cols2[,c("occupation_cat","religion")]))
#Chi-squared approximation may be incorrect - point this out in ppt

#binned plots for continuous predictors to check for tranformations
#convert occupation_cat to num
cols2$occupation_num <- 0
cols2$occupation_num[cols2$occupation_cat == 'HighPay'] <- 1

#occupation vs age
par(mfrow=c(1,1)) 
binnedplot(y=cols2$occupation_num,cols2$age,xlab="Age",ylim=c(0,1),col.pts="navy",
           ylab ="Occupation Category?",main="Binned Age and Occupation category cases",
           col.int="white")
#might need a quadratic transformation as there is increasing trend then decreasing trend - but there isn't enough data for > 60


#occupation vs household exp
binnedplot(y=cols2$occupation_num,cols2$household_exp,xlab="Household Expenses",ylim=c(0,1),col.pts="navy",
           ylab ="Occupation Category?",main="Binned Household expenses and Occupation category cases",
           col.int="white")
#log tranformation

#occupation vs assets
binnedplot(y=cols2$occupation_num,cols2$ASSETS,xlab="Assets",ylim=c(0,1),col.pts="navy",
           ylab ="Occupation Category?",main="Binned Assets and Occupation category cases",
           col.int="white")
#ask michael

######Model fitting
mod1 <- glm(occupation_num ~ sex + age + owned_cultivated + owns_livestick
            + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_cat_binary +
              household_exp + ASSETS + educ_level, data = cols2, family = binomial)
summary(mod1)
vif(mod1)

mod2 <- glm(occupation_num ~ sex + age + owned_cultivated + owns_livestick
            + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_cat_binary +
              household_exp + ASSETS, data = cols2, family = binomial)
summary(mod2)
vif(mod2)

mod3 <- glm(occupation_num ~ sex + age + owned_cultivated + owns_livestick
            + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_cat_binary +
              household_exp + ASSETS + educ_level:caste_cat_binary, data = cols2, family = binomial)
summary(mod3)
vif(mod3)


#model diagnostic
#save the raw residuals
rawresid1 <- residuals(mod2,"resp")

#binned residual plots
binnedplot(x=fitted(mod2),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks okay

#household expenses
binnedplot(x=cols2$household_exp,y=rawresid1,xlab="Household exp",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks okay

#age
binnedplot(x=cols2$age,y=rawresid1,xlab="Age",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks okay

#assets
binnedplot(x=cols2$ASSETS,y=rawresid1,xlab="Assets",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks okay


#accuracy
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(mod1) >= 0.5, "1","0")),
                            as.factor(cols2$occupation_num),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"]

roc(cols2$occupation_num,fitted(mod1),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

###########################################################################
###########################################################################
########################## Multinomial ####################################
###########################################################################
###########################################################################


#graph to understand different occupation categories
ggplot(data, aes(x = WS4, y = INCOME)) + 
  stat_summary(fun = "mean", geom = "bar") + theme(axis.text.x = element_text(angle = 90))

###collapsing the response variable into 3 categories
cols$occupation_cat <- with(cols, ifelse(WS4 %in% c("(098) Drivers 98", 
                                                    "(081) Carpenters 81", "(054) Sweepers 54", "(052) Cooks/waiters 52", "(056) Barbers 56", 
                                                    "(051) House keepers 51", "(071) Miners 71",
                                                    "(080) Shoe makers 80", "(053) Maids 53", "(078) Tobacco 78"
), 'LowPay', ifelse(
  WS4 %in% c("(003) Eng. tech 3", "(002) Engineers 2", "(007) Physicians 7",
             "(005) Life scientists 5", "(012) Accountants 12", "(023) Mgr finance 23", "(014) Lawyers 14",
             "(009) Other scientific 9", "(004) Air/ship officers 4",
             "(011) Economists 11", "(010) Statisticians 10", "(029) Managerial nec 29", "(021) Govt officials 21", "(024) Mgr manf 24"), 'HighPay', 
  ifelse(
    WS4 %in% c("(015) Teachers 15",
               "(025) Mgr transp/commun 25", "(008) Nursing 8", "(016) Journalists 16", "(020) Elected officials 20", "(030) Clerical Supe 30",
               "(033) Book-keepers 30", "(034) Computing op 34", "(035) Clerical nec 35"), 'MedPay', 'remove'))))
data2<-cols[!(cols$occupation_cat=="remove"),]

#renaming columns
setnames(data2, old = c('RO3','RO4', 'RO5', 'RO6', 'RO7', 'FM1', 'FM36Y', 'AN1', 'AN5Y', 'ED2', 'ED3', 'EDUC7', 'MM7Y', 'MM12Y', 'URBAN4_2011',
                        'ID11', 'ID13', 'COPC'), 
         new = c('sex','rel_to_head', 'age', 'marrital_status', 'primary_act_status',
                 'owned_cultivated', 'own_farm_work', 'owns_livestick',
                 'animal_care_work', 'literacy', 'english_ability', 'educ_level',
                 'uses_computer', 'owns_mobile', 'urban_rural', 'religion',
                 'caste_category', 'household_exp'))

#convert occ category to factor
data2$occupation_cat <- factor(data2$occupation_cat)

#collapsing caste into two categories for initial model
data2$caste_cat_binary <- with(data2, ifelse(caste_category %in% c('(1) Brahmin 1', '(2) Forward/General (except Brahmin) 2'), 'Forward', 
                                             ifelse(caste_category %in% c('(3) Other Backward Castes (OBC) 3', '(4) Scheduled Castes (SC) 4',
                                                                          '(5) Scheduled Tribes (ST) 5'), 'Backward', 'Other')))
data2$caste_cat_binary <- factor(data2$caste_cat_binary)

#removing NAs from household exp and assets
data2 <- data2[complete.cases(data2[ , 24:25]),]
#cols2 <- cols2[complete.cases(cols2[ , 11]),]
#cols2 <- cols2[complete.cases(cols2[ , 13]),]
data2 <- data2[complete.cases(data2[ , 14:16]),]

########EDA
#EDA for continuous variables
#occupation_cat vs age
ggplot(data2,aes(x=occupation_cat, y=age, fill=occupation_cat)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Occupaition Category vs Age",
       x="Occupation Category",y="Age") + 
  theme_classic() + theme(legend.position="none")
#little difference in median

#occupation category vs household expenses
ggplot(data3,aes(x=occupation_cat, y=household_exp, fill=occupation_cat)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Occupaition Category vs Household Expenses",
       x="Occupation Category",y="Household Expenses") + 
  theme_classic() + theme(legend.position="none")
#difference in median which is obvious

#occupation category vs assets
ggplot(data2,aes(x=occupation_cat, y=ASSETS, fill=occupation_cat)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Occupaition Category vs Assets",
       x="Occupation Category",y="Assets") + 
  theme_classic() + theme(legend.position="none")
#difference in median which is obvious

#occupation category vs income
ggplot(data2,aes(x=occupation_cat, y=INCOME, fill=occupation_cat)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Occupaition Category vs Income",
       x="Occupation Category",y="Income") + 
  theme_classic() + theme(legend.position="none")
#difference in median which is obvious (will not include this as a predictor but can use it to check correctness of 3rd occ category)


#EDA for categorical variables
# occupation_category vs sex
apply(table(data2[,c("occupation_cat","sex")])/sum(table(data2[,c("occupation_cat","sex")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data2[,c("occupation_cat","sex")]))
#it's significant



# occupation_category vs marrital_status
apply(table(data2[,c("occupation_cat","marrital_status")])/sum(table(data2[,c("occupation_cat","marrital_status")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data2[,c("occupation_cat","marrital_status")]))
#Chi-squared approximation may be incorrect 

#dropping level gauna
data3<-data2[!(data2$marrital_status=="(5) Married no gauna 5"),]
data3$marrital_status = droplevels(data3$marrital_status)

# occupation_category vs marrital_status
apply(table(data3[,c("occupation_cat","marrital_status")])/sum(table(data3[,c("occupation_cat","marrital_status")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","marrital_status")]))
#it's significant


# occupation_category vs owned_cultivated
apply(table(data3[,c("occupation_cat","owned_cultivated")])/sum(table(data3[,c("occupation_cat","owned_cultivated")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","owned_cultivated")]))
#it's significant

# occupation_category vs own_farm_work
apply(table(data3[,c("occupation_cat","own_farm_work")])/sum(table(data3[,c("occupation_cat","own_farm_work")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","own_farm_work")]))
#it's significant

# occupation_category vs owns_livestock
apply(table(data3[,c("occupation_cat","owns_livestick")])/sum(table(data3[,c("occupation_cat","owns_livestick")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","owns_livestick")]))
#it's significant

# occupation_category vs animal_care_work
apply(table(data3[,c("animal_care_work", "occupation_cat")])/sum(table(data3[,c("animal_care_work", "occupation_cat")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","animal_care_work")]))
#it's significant

# occupation_category vs literacy
apply(table(data3[,c("literacy", "occupation_cat")])/sum(table(data3[,c("literacy", "occupation_cat")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("literacy", "occupation_cat")]))
#it's significant

# occupation_category vs english_ability
apply(table(data3[,c("english_ability", "occupation_cat")])/sum(table(data3[,c("english_ability", "occupation_cat")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","english_ability")]))
#it's significant

# occupation_category vs educ_level
apply(table(data3[,c("educ_level", "occupation_cat")])/sum(table(data3[,c("educ_level", "occupation_cat")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","educ_level")]))
#it's significant

# occupation_category vs uses_computer
apply(table(data3[,c("uses_computer", "occupation_cat")])/sum(table(data3[,c("uses_computer", "occupation_cat")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","uses_computer")]))
#it's significant

# occupation_category vs owns_mobile
apply(table(data3[,c("owns_mobile", "occupation_cat")])/sum(table(data3[,c("owns_mobile", "occupation_cat")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("owns_mobile", "occupation_cat")]))
#it's significant

# occupation_category vs urban_rural
apply(table(data3[,c("urban_rural", "occupation_cat")])/sum(table(data3[,c("urban_rural", "occupation_cat")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","urban_rural")]))
#it's significant

# occupation_category vs METRO
apply(table(data3[,c("METRO", "occupation_cat")])/sum(table(data3[,c("METRO", "occupation_cat")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","METRO")]))
#it's significant but use urban_rural

# occupation_category vs religion
apply(table(data3[,c("religion", "occupation_cat")])/sum(table(data3[,c("religion", "occupation_cat")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","religion")]))
#Chi-squared approximation may be incorrect - point this out in ppt

#dropping level gauna
data3<-data3[!(data3$religion=="(9) None 9"),]
data3$religion = droplevels(data3$religion)

# occupation_category vs religion
apply(table(data3[,c("occupation_cat", "religion")])/sum(table(data3[,c("occupation_cat", "religion")])),
      2,function(x) x/sum(x)) 
#chi-square test
chisq.test(table(data3[,c("occupation_cat","religion")]))
#do not use this for interactions

#fitting the model
mod1 <- multinom(occupation_num ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   household_exp + ASSETS + educ_level + marrital_status + religion, data=data3)
summary(mod1)

confint(mod1)
exp(confint(mod1))

#making response variable numeric
data3$occupation_num <- as.numeric(data3$occupation_cat)

#calculating p values
output1 <- summary(mod1)
z_value <- output1$coefficients/output1$standard.errors
p_value <- (1 - pnorm(abs(z_value), 0, 1))*2 
#we are using two-tailed z test, that is, a normal approximation
full_summary1 <- lapply(c(2),function(x) rbind(output1$coefficients[as.character(x),],
                                                 output1$standard.errors[as.character(x),],
                                                 z_value[as.character(x),],
                                                 p_value[as.character(x),]))
kable(lapply(full_summary1,function(x) {rownames(x) <- c("Coefficient","Std. Errors","z-value","p-value"); x}))

#let's see some interactions with caste cateory that we are interested in (basically controlling for these interactions and making the model more robust)

#caste and literacy
mod1_int1 <- multinom(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                  + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category * literacy +
                   household_exp + ASSETS + educ_level + marrital_status + religion, data=data3)
#not converging 

#caste and religion
mod1_int2 <- multinom(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                      + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category * religion +
                        household_exp + ASSETS + educ_level + marrital_status + literacy, data=data3)
#not converging 

#caste and assets
mod1_int3 <- multinom(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                      + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category * ASSETS +
                        household_exp + religion + educ_level + marrital_status + literacy, data=data3)
anova(mod1, mod1_int3, test = "Chisq")
#not significant

#caste and sex
mod1_int4 <- multinom(occupation_cat ~ ASSETS + age + owned_cultivated + owns_livestick
                      + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category * sex +
                        household_exp + religion + educ_level + marrital_status + literacy, data=data3)
#not converging 

#caste and owned_cultivated
mod1_int5 <- multinom(occupation_cat ~ ASSETS + age + sex + owns_livestick
                      + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category * owned_cultivated +
                        household_exp + religion + educ_level + marrital_status + literacy, data=data3)
anova(mod1, mod1_int5, test = "Chisq")
#not significant 

#caste and owns_livestick
mod1_int6 <- multinom(occupation_cat ~ ASSETS + age + sex + owned_cultivated
                      + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category * owns_livestick +
                        household_exp + religion + educ_level + marrital_status + literacy, data=data3)
anova(mod1, mod1_int6, test = "Chisq")
#not significant 

#caste and english_ability
mod1_int7 <- multinom(occupation_cat ~ ASSETS + age + sex + owned_cultivated
                      + owns_livestick  + uses_computer + owns_mobile + urban_rural + caste_category * english_ability +
                        household_exp + religion + educ_level + marrital_status + literacy, data=data3)
anova(mod1, mod1_int7, test = "Chisq")
#not significant 

#caste and uses_computer
mod1_int8 <- multinom(occupation_cat ~ ASSETS + age + sex + owned_cultivated
                      + owns_livestick  + english_ability + owns_mobile + urban_rural + caste_category * uses_computer +
                        household_exp + religion + educ_level + marrital_status + literacy, data=data3)
anova(mod1, mod1_int8, test = "Chisq")
#not significant 

#caste and owns_mobile
mod1_int9 <- multinom(occupation_cat ~ ASSETS + age + sex + owned_cultivated
                      + owns_livestick  + english_ability + uses_computer + urban_rural + caste_category * owns_mobile +
                        household_exp + religion + educ_level + marrital_status + literacy, data=data3)
anova(mod1, mod1_int9, test = "Chisq")
#not significant 

#caste and urban_rural
mod1_int10 <- multinom(occupation_cat ~ ASSETS + age + sex + owned_cultivated
                      + owns_livestick  + english_ability + uses_computer + owns_mobile + caste_category * urban_rural +
                        household_exp + religion + educ_level + marrital_status + literacy, data=data3)
anova(mod1, mod1_int10, test = "Chisq")
#significant 

#caste and household_exp
mod1_int11 <- multinom(occupation_cat ~ ASSETS + age + sex + owned_cultivated
                       + owns_livestick  + english_ability + uses_computer + owns_mobile + caste_category * household_exp +
                         urban_rural + religion + educ_level + marrital_status + literacy, data=data3)
anova(mod1, mod1_int11, test = "Chisq")
#not significant 

#caste and educ_level
mod1_int12 <- multinom(occupation_cat ~ ASSETS + age + sex + owned_cultivated
                       + owns_livestick  + english_ability + uses_computer + owns_mobile + caste_category * educ_level +
                         urban_rural + religion + household_exp + marrital_status + literacy, data=data3)
anova(mod1, mod1_int12, test = "Chisq")
#not converging 

#caste and marrital_status
mod1_int13 <- multinom(occupation_cat ~ ASSETS + age + sex + owned_cultivated
                       + owns_livestick  + english_ability + uses_computer + owns_mobile + caste_category * marrital_status +
                         urban_rural + religion + household_exp + educ_level + literacy, data=data3)
anova(mod1, mod1_int13, test = "Chisq")
#not converging 

#### the only significant interaction is between urban_rural and caste 

#let's do BIC now for variavle selection
n <- nrow(data3)
null_model <- multinom(occupation_cat ~ caste_category * urban_rural,data=data3)
model_step <- step(null_model,scope=formula(mod1_int10),direction="both",
                   trace=0,k = log(n))
model_step$call
summary(model_step)

final_model_mln <- model_step

###### Predictions
#predicted probabilities for cases in the model
predprobs <- fitted(mod1_int10) 
#look at first five rows just to see what results
predprobs[1:5,]

###### Diagnostics
####diagnostics comparing average raw residuals across bins based on predictor values
#for HighPay:  create a raw residual using only the first column of the predicted probabilities
rawresid1 <- (data3$occupation_cat == 'HighPay') -  predprobs[,1]

#for LowPay:  create a raw residual using only the second column of the predicted probabilities
rawresid2 <- (data3$occupation_cat == 'LowPay') -  predprobs[,2]

#for MedPay:  create a raw residual using only the third column of the predicted probabilities
rawresid3 <- (data3$occupation_cat == 'MedPay') -  predprobs[,3]

##can do binned plots for continuous variables
#make a 2 by 2 graphical display
#age
par(mfcol = c(2,2))
binnedplot(data3$age, rawresid1, xlab = "age", ylab = "Raw residuals", main = "Binned plot: occupation category = HighPay")
binnedplot(data3$age, rawresid2, xlab = "age", ylab = "Raw residuals", main = "Binned plot: occupation category = LowPay")
binnedplot(data3$age, rawresid3, xlab = "age", ylab = "Raw residuals", main = "Binned plot: occupation category = MedPay")
#looks good

#assets
par(mfcol = c(2,2))
binnedplot(data3$ASSETS, rawresid1, xlab = "Assets", ylab = "Raw residuals", main = "Binned plot: occupation category = HighPay")
binnedplot(data3$ASSETS, rawresid2, xlab = "Assets", ylab = "Raw residuals", main = "Binned plot: occupation category = LowPay")
binnedplot(data3$ASSETS, rawresid3, xlab = "Assets", ylab = "Raw residuals", main = "Binned plot: occupation category = MedPay")
#looks good

## Accuracy
pred_classes <- predict(final_model_mln)
Conf_mat <- confusionMatrix(as.factor(pred_classes),as.factor(data3$occupation_cat))
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[,c("Sensitivity","Specificity")]

## Individual ROC curves for the different levels
#here we basically treat each level as a standalone level
par(mfcol = c(2,2))
roc((data3$occupation_cat=='HighPay'),predprobs[,1],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="red3",percent=T,main="HighPay")
roc((data3$occupation_cat=='LowPay'),predprobs[,2],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="gray3",percent=T,main="LowPay")
roc((data3$occupation_cat=='MedPay'),predprobs[,3],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="green3",percent=T,main="MedPay")
#AUC for low pay is 92.2% which is what we are interested in 

###########################################################################
###########################################################################
########################## Proportional odds #############################
###########################################################################
###########################################################################

#let's order the response variable
data4 <- data3
data4$occupation_cat <- ordered(data4$occupation_cat,
                              levels=c("LowPay","MedPay","HighPay"))

#let's build the proportional odds model with all the main effects first
Mod1_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   household_exp + ASSETS + educ_level, data=data4, Hess = TRUE)
summary(Mod1_pom)

#error: Error in svd(X) : infinite or missing values in 'x'
#reducing the number of variables - turns out the problem was with household expenses
Mod2_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level, data=data4, Hess = TRUE)
summary(Mod2_pom)

#checking if interaction between caste category and urban rural is significant
Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:urban_rural, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's significant

#checking if other interactions are significant
Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:sex, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's significant

Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:age, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's significant but not very

Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:owned_cultivated, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's not significant

Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:owns_livestick, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's significant 

Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:literacy, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's not significant 

Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:english_ability, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's significant 

Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:uses_computer, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's significant 

Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:owns_mobile, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's not significant 

Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:ASSETS, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's not significant

Mod3_pom <- polr(occupation_cat ~ sex + age + owned_cultivated + owns_livestick
                 + literacy + english_ability  + uses_computer + owns_mobile + urban_rural + caste_category +
                   ASSETS + educ_level + caste_category:educ_level, data=data4, Hess = TRUE)
summary(Mod3_pom)

anova(Mod2_pom, Mod3_pom, test = "Chisq")
#it's not significant 

#let's use bic to select variables
n <- nrow(data4)
null_model_pom <- polr(occupation_cat ~ caste_category * (urban_rural + sex + age + owns_livestick + english_ability + uses_computer),data=data4)
full_model <- polr(occupation_cat ~ caste_category * (urban_rural + sex + age + owns_livestick + english_ability + uses_computer) 
                   + owned_cultivated + literacy + owns_mobile + ASSETS + educ_level,data=data4)
model_step_pom <- step(null_model_pom,scope=formula(full_model),direction="both",
                   trace=0,k = log(n))
model_step_pom$call
#it's not selecting caste category

#let's do AIC
null_model_pom <- polr(occupation_cat ~ caste_category ,data=data4)
full_model <- polr(occupation_cat ~ caste_category * (urban_rural + sex + age + owns_livestick + english_ability + uses_computer) 
                   + owned_cultivated + literacy + owns_mobile + ASSETS + educ_level,data=data4)
model_step_aic <- step(null_model_pom,scope=formula(full_model),direction="both",
                       trace=0)
model_step_aic$call

#will use multinomial because it's residual dev and aic is better

#interpreting the interaction between urbanrural and caste category
theme_set(theme_sjplot())

plot_model(mod1_int10, type = "pred", terms = c("caste_category", "urban_rural"), 
           title = "Predicted Probabilities for 100% Voter Turnout", 
           axis.title = c("Caste Category","Predicted Probability"), 
           legend.title = "Urban/ Rural") + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))






