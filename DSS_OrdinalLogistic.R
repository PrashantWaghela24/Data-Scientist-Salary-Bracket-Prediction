library(readxl)
library(dplyr)
library(openxlsx)
library(rio)
library(tidyr)
library(car)
library(performance)
library(see)
library(patchwork)
library(MASS)

datascientist=read_excel('~/Documents/DMML Project Documents/AnnualDataSci_Salary/AnnualSalaryDataScientists.xlsx')
summary(datascientist)
names(datascientist)

datascientist=separate(datascientist, col=experience, into=c("Exp_Lower","Exp_Higher"),sep=",")

datascientist$Exp_Lower=as.numeric(as.character(datascientist$Exp_Lower))
datascientist$Exp_Higher=as.numeric(as.character(datascientist$Exp_Higher))

datascientist=separate(datascientist, col=salary, into=c("salary_Lower","salary_Higher"),sep=",")

datascientist$salary_Lower=as.numeric(as.character(datascientist$salary_Lower))
datascientist$salary_Higher=as.numeric(as.character(datascientist$salary_Higher))
names(datascientist)
summary(datascientist)

datascientist$avg_exp=(datascientist$Exp_Lower + datascientist$Exp_Higher)/2

datascientist$cat_salary=cut(datascientist$salary_Higher, breaks=c(3,10,25,51), labels = c(1,2,3),right=FALSE)

datascientist=datascientist[c('avg_exp','key_skills_cnt','cat_salary')]
datascientist$cat_salary=as.ordered(datascientist$cat_salary)
head(datascientist)

set.seed(1234)
dataScientistpd=sample(2,nrow(datascientist), replace=TRUE, prob=c(0.7,0.3))
datascientist_train=datascientist[dataScientistpd==1,]
datascientist_test=datascientist[dataScientistpd==2,]

par(mfrow=c(2,2))

#MODEL 1: ORDINAL LOGISTIC REGRESSION-------------------------------------------------------------

datascientist_train$cat_salary=as.ordered(datascientist_train$cat_salary)
datascientist_test$cat_salary=as.ordered(datascientist_test$cat_salary)

summary(datascientist_train)
xtabs(~cat_salary+key_skills_cnt,datascientist_train)

hist(datascientist_train$key_skills_cnt)

model=polr(cat_salary~key_skills_cnt+avg_exp, datascientist_train, Hess = TRUE)
summary(model)

(ctable=coef(summary(model)))
p=pnorm(abs(ctable[,"t value"]), lower.tail=FALSE)*2
(ctable=cbind(ctable,"p value"=p))

### Model Enhancement

ci <- confint(model)
exp(coef(model))
exp(cbind(OR = coef(model), ci))

## Method - probit
model2=update(model, method = "probit", Hess = TRUE)
(ctable2=coef(summary(model2)))
p2=pnorm(abs(ctable2[,"t value"]), lower.tail=FALSE)*2
(ctable2=cbind(ctable2,"p value"=p2))
ci2 <- confint(model2)
exp(coef(model2))
exp(cbind(OR = coef(model2), ci2))
summary(model2)

## Method - logistic
model3=update(model2, method = "logistic", Hess = TRUE)
(ctable3=coef(summary(model3)))
p3=pnorm(abs(ctable3[,"t value"]), lower.tail=FALSE)*2
(ctable3=cbind(ctable3,"p value"=p3))
ci3 <- confint(model3)
exp(coef(model3))
exp(cbind(OR = coef(model3), ci3))
summary(model3)

## Method - cloglog
model4=update(model3, method = "cloglog", Hess = TRUE)
(ctable4=coef(summary(model4)))
p4=pnorm(abs(ctable4[,"t value"]), lower.tail=FALSE)*2
(ctable4=cbind(ctable4,"p value"=p4))
ci4 <- confint(model4)
exp(coef(model4))
exp(cbind(OR = coef(model4), ci4))
summary(model4)

head(predict(model4, datascientist_train, type = 'p'))
addterm(model4, ~.^2, test = 'Chisq')
model5 = stepAIC(model4, ~.^2)
summary(model5)
model5$anova

#######

(pred=predict(model5, datascientist_train))

(tab=table(pred,datascientist_train$cat_salary))
1-sum(diag(tab))/sum(tab)

(pred1=predict(model5, datascientist_test))

(tab1=table(pred1,datascientist_test$cat_salary))
1-sum(diag(tab1))/sum(tab1)

plot(datascientist_test$cat_salary, pred1, main="Bar representation of Confusion Matrix", col=c('#10A6EC','#FF8A2F','#EC4C10'))
summary(pred1)
(ctable=coef(summary(pred1)))
p=pnorm(abs(ctable[,"t value"]), lower.tail=FALSE)*2
(ctable=cbind(ctable,"p value"=p))
library(caret)
confusionMatrix(table(pred1, datascientist_test$cat_salary))
