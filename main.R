rm(list=ls())
library(dplyr)

data=read.csv("https://raw.githubusercontent.com/RitikOnTheRoad/Statistical-Analysis-of-Stroke-Risk-Factors/refs/heads/main/healthcare-dataset-stroke-data.csv")

data=as_tibble(data)
summary(data)
head(data)


table(data$work_type)
table(data$gender)
table(data$Residence_type)
table(data$smoking_status)
table(data$ever_married)
table(data$Residence_type)
unique(data$smoking_status)

#unknown bmi values are entered as "N/A" instead of NA
data$bmi[data$bmi=="N/A"]=NA
sum(is.na(data))

#removing the gender "other" for model simplicity
data=data[data$gender!="Other",]


data=data %>% 
  mutate(
    gender=as.factor(gender),
    hypertension=as.factor(hypertension),
    heart_disease=as.factor(heart_disease),
    ever_married=as.factor(ever_married),
    Residence_type=as.factor(Residence_type),
    stroke=as.factor(stroke),
    bmi=as.double(bmi),
    smoking_status=replace(smoking_status,smoking_status=="Unknown",NA), #labeling unknown smoking status as NA
    smoking_status=factor(
      smoking_status,
      levels=c('never smoked',"formerly smoked","smokes")
    ),
    work_type=as.factor(work_type),
    age10=floor(age/10),
    avg_glucose_level=scale(avg_glucose_level),
    bmi=scale(bmi)
  ) %>% 
  select(-id,-age)
data

#imbalanced class

library(ggplot2)
ggplot(data,aes(stroke,fill=stroke))+geom_bar()+
  scale_x_discrete(labels=c("no","yes"))+
  labs(title="class distribution")


#Train test split
library(rsample)

set.seed(42)
data_split=initial_split(data,prop = 0.7)
train_data=training(data_split)
test_data=testing(data_split)

#modelling
#Base logistic Regression
lr_base=glm(stroke~.,data=train_data,family="binomial")

library(broom)

tidy(lr_base)

y_hat=predict(lr_base,test_data,type="response")
y_hat=as.factor(ifelse(y_hat>0.3,1,0))

library(caret)
cm=confusionMatrix(data=y_hat,reference=test_data$stroke)
cm

#Logistic Regression with class weights
n0=length(train_data$stroke[train_data$stroke==0])
n1=length(train_data$stroke)-n0

w0=n1/(n0+n1)
w1=n0/(n0+n1)

lr_weighted=glm(stroke~.,data=train_data,family="binomial",weights = ifelse(train_data$stroke==1,w1,w0))
tidy(lr_weighted)
y_hat=predict(lr_weighted,test_data,type="response")
y_hat=as.factor(ifelse(y_hat>0.55,1,0))
cm=confusionMatrix(data=y_hat,reference=test_data$stroke)
cm

#drop or fix work type

df_no_work=data[,-5]
data_split=initial_split(df_no_work,0.75)
train=training(data_split)
test=training(data_split)

lr_no_work=glm(stroke~.,family="binomial",data=train,weights=ifelse(train$stroke==1,w1,w0))
tidy(lr_no_work)

prediction=predict(lr_no_work,test,type="response")
prediction=as.factor(ifelse(prediction>0.5,1,0))


cm=confusionMatrix(data=prediction,reference=test$stroke)
cm$byClass
cm$table

#recall precision curve for class 1
recall=c()
precision=c()
for(i in seq(0,1,0.1)){
  prediction=predict(lr_no_work,test,type="response")
  prediction=as.factor(ifelse(prediction>i,1,0))
  
  cm=confusionMatrix(data=prediction,reference=test$stroke)
  
  recall=c(recall,cm$table[2,2]/(cm$table[2,2]+cm$table[1,2]))
  precision=c(precision,cm$table[2,2]/(cm$table[2,2]+cm$table[2,1]))
}


ggplot(data=data.frame(recall,precision),aes(x=recall,y=precision,marker=TRUE))+geom_line(col="red",lwd=2)+geom_point(col="blue",cex=2)+
  labs(title="Precision recall curve")

#Precison is lowwwwww



