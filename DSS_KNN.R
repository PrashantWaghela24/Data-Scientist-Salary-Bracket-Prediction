library(readxl)
datascientist=read_excel('~/Documents/DMML Project Documents/AnnualSalaryDataScientists.xlsx')
summary(datascientist)
names(datascientist)

library(tidyr)
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

library(dplyr)
library(openxlsx)
library(rio)
#export(datascientist[c(0:13863),], file="/Users/prashantwaghela/Documents/DMML Project Documents/datascientist_train.xlsx", rowNames=F)
#export(datascientist[c(0,13864:19802),], file="/Users/prashantwaghela/Documents/DMML Project Documents/datascientist_test.xlsx", rowNames=F)


library(readxl)
datascientist_train=datascientist[c(0:13863),]
summary(datascientist_train)
names(datascientist_train)

datascientist_test=datascientist[c(0,13864:19802),]
summary(datascientist_test)
names(datascientist_test)


#MODEL 2: KNN REGRESSION-------------------------------------------------------------

dssKnn_subset=datascientist_train[c('avg_exp','key_skills_cnt','cat_salary')]
head(dssKnn_subset)

dssKnn_subset_test=datascientist_test[c('avg_exp','key_skills_cnt','cat_salary')]
head(dssKnn_subset)

#Normalization Function

normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}

dssKnn_subset_n=as.data.frame(lapply(dssKnn_subset[,1:2], normalize))
head(dssKnn_subset_n)

dssKnn_subset_test_n=as.data.frame(lapply(dssKnn_subset_test[,1:2], normalize))
head(dssKnn_subset_test_n)

train_cat_sal=dssKnn_subset
test_cat_sal=dssKnn_subset_test

train_cat_sal_labels=dssKnn_subset$cat_salary
test_cat_sal_labels=dssKnn_subset_test$cat_salary

install.packages("class")
library(class)

NROW(train_cat_sal_labels)
NROW(train_cat_sal)

dim(train_cat_sal)
dim(test_cat_sal)
length(train_cat_sal_labels)

knn_1=knn(train_cat_sal, test_cat_sal, train_cat_sal_labels, k=10)
str(dssKnn_subset)

accuracy=100*sum(test_cat_sal_labels==knn_1)/NROW(test_cat_sal_labels)
table(knn_1, test_cat_sal_labels)

library(caret)
confusionMatrix(table(knn_1, test_cat_sal_labels))

i=1
k_opt=1
for(i in 1:25){
  knn_opt=knn(train_cat_sal, test_cat_sal, train_cat_sal_labels, k=i)
  k_opt[i]=100*sum(test_cat_sal_labels==knn_opt)/NROW(test_cat_sal_labels)
  k=i
  cat(k,'=',k_opt[i],'\n')
}