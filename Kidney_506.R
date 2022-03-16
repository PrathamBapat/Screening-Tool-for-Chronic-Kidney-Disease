library(corrplot)
library(dplyr)
library(psych)
library(ROCR)
library(car)

ch <- read.csv('Data.csv')
summary(ch)

# Dividing the test and train data as suggested in case study
ch_train <- ch[1:6000,]
ch_test <- ch[6001:8844,]

library(tidyverse)
ch_train %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank())

summary(ch_train)

#------------------------- Data Preparation--------------#

#Deleting NA's in Dataset - Using LD Method for simplicity
ch_train_imputed <- ch_train[complete.cases(ch_train),] #or can use ** na.omit(dataframe) **
ch_train_imputed <- subset(ch_train_imputed, ch_train_imputed$ID != 5934)
ch_train_imputed <- ch_train_imputed[-1]
ch_train_imputed <- subset(ch_train_imputed, select = -c(Weight, BMI, Total.Chol, Female, Obese, Dyslipidemia, Fam.Diabetes))

str(ch_train_imputed)

attach(ch_train_imputed)

#Subsetting data into numeric and factors

ch_train_fac <- ch_train_imputed %>% select(Racegrp,CareSource, Insured, PVD, Activity,
                                            PoorVision, Smoker, Hypertension, Fam.Hypertension, 
                                            Diabetes, Stroke, CVD, Fam.CVD, CHF, Anemia)


ch_train_num <- ch_train_imputed %>% select(Age, Height, Waist, SBP, DBP, HDL, LDL)
target <- as.factor(ch_train_imputed$CKD)


# Creating Bins for Age
ch_train_num$agegroup <- cut(ch_train_num$Age, breaks = c(20,40,60,80,100),right = FALSE,labels = FALSE)
ch_train_num <- subset(ch_train_num, select = -c(Age))

#test1 <- subset(ch_train_num,select = c(Age, agegroup)) # for testing purpose

ch_train_fac <- as.data.frame(ch_train_fac)
ch_train_num <- as.data.frame(ch_train_num)


#-----------Exploratory Analysis-------#
# Exploring Relationships 


#Between Age and Hypertension
tbl <- table(ch_train_fac$Hypertension,ch_train_num$agegroup)
chisq.test(tbl) # Highly Related

#Between Age and Diabetes
tbl1 <- table(ch_train_final$CVD,ch_train_final$Insured)
chisq.test(tbl1)

#Between Hypertension and Activity
tbl2 <- table(Hypertension,Diabetes)
chisq.test(tbl2) #Significant Relationship but comparitively very less


#Between Hypertension and Smokers
tbl3 <- table(Hypertension,Smoker)
chisq.test(tbl3) # Not good enough r-squared

tbl4 <- table(Hypertension,Diabetes)
chisq.test(tbl4)

# Finding out correlation between numeric factors
f <- subset(ch_train_num, select = -c(agegroup))
M = cor(f)
corrplot(M, method = 'number')
#corrplot(M, method = 'color')

#Race
for (i in sort(unique(ch_train_fac$Racegrp))){
  ch_train_fac[i] <-ifelse(ch_train_fac$Racegrp == i, 1,0)
  colnames(ch_train_fac)[which(names(ch_train_fac) == i)] <- paste("RaceGrp_",i,sep = "")
}

#Care Source
for (i in sort(unique(ch_train_fac$CareSource))){
  ch_train_fac[i] <-ifelse(ch_train_fac$CareSource == i, 1,0)
  colnames(ch_train_fac)[which(names(ch_train_fac) == i)] <- paste("CareSrc_",i,sep = "")
}

library(ggplot2)
ggplot(stack(f), aes(x = ind, y = values)) +
  geom_boxplot()

#Removing Additional Column of Race and Care Source
ch_train_fac <- subset(ch_train_fac, select = -c(Racegrp, CareSource)) # Removing columns care source, racegrp and making ref cat for dummy coding (white and Dr/HMO)

#Changing categorical variables into factors
ch_train_fac <- data.frame(lapply(ch_train_fac, as.factor))


#Combining numerical and categorical data
ch_train_final <- cbind(ch_train_fac,ch_train_num,target)
ch_train_final$Height <- as.numeric(ch_train_final$Height)
ch_train_final$Waist <- as.numeric(ch_train_final$Waist)

# ------------------ Building Model on all variables ---------------------#
attach(ch_train_final)

# Not including: Female + Total Chol + Obese + Dyslipidemia + Fam.Diabetes+ BMI
formula <- target ~  Height + Waist + SBP + DBP + HDL + LDL + Activity + agegroup + Educ + Unmarried + Income + Insured + PVD + PoorVision + Smoker + Hypertension + Fam.Hypertension + Diabetes + Stroke + CVD + Fam.CVD + CHF + Anemia + RaceGrp_black + RaceGrp_white + RaceGrp_hispa + CareSrc_clinic+ CareSrc_noplace + CareSrc_DrHMO

# Get Train and Validation Set
train = ch_train_final[1:3308,]
val = ch_train_final[3308:4135,]

#produces model with all independent variables 
full=glm(formula, data = train,family=binomial(logit))	
summary(full)

detach(ch_train_final)
#---------------------------ROCR Curves-------------------------#

score <- predict(full,val,type="response")

#ROC Curve
pred_roc = prediction(score, val$target)
perf = performance(pred_roc,"tpr","fpr")
# Plotting the ROC curve
plot(perf, col = 'black', lty = 3, lwd = 3, main ="ROC Curve")
abline(a = 0, b = 1)


#PR Curve
pr_curve <- performance(pred_roc,"prec","rec")
plot(pr_curve, col = 'black', lty = 3, lwd = 3, main ="PR Curve")

#Optimal Threshold with FN cost 10 times FP cost
cost.perf <- performance(pred_roc,"cost",cost.fp = 1, cost.fn = 10)
pred_roc@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

# ------------------Model Evaluation-------------------------#
pred <- ifelse(score > 0.086 , 1,0)

#Confusion Matrix
cnf<-table(val$target,pred)
cnf

#Accuracy
accuracy <- sum(diag(cnf))/sum(cnf) *100
print (accuracy)

# Recall
recall <- cnf[2,2]/sum(cnf[2,]) *100
print(recall)

#Precision
precision <- cnf[2,2]/sum(cnf[,2]) *100
print(precision)

#f-score
f_score <- 2*precision*recall/(precision+recall)
print(f_score)

#Check for Multi-collinearity
vif(full) # variance inflation factors
sqrt(vif(full)) > 2 # problem?

#confint(full)

