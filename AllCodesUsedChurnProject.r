ba<-read.csv('D:\\BA\\Churn_Train (1).csv');
library(ISLR)
library(dplyr)
library(caret)
colMeans(is.na(ba))
library(VIM)

preproc<-preProcess(ba, method = c("knnImpute"), k=5)
ba_imputed<-predict(preproc,ba)
colMeans(is.na(ba_imputed))
summary(ba_imputed)
View(ba_imputed)
nzv<-nearZeroVar(ba_imputed)
nzv
ba_imputed$churn<-as.numeric(ba_imputed$churn)
summary(ba_imputed)
ba_imputed$number_vmail_messages<-NULL
summary(ba_imputed)
x<-ba_imputed$churn
y<-ba_imputed$international_plan
cor(x,y,method = 'pearson', use = "complete.obs")
library(FSelector) # to check importance of each attribute
information.gain(business)  #information.gain(name of your dataframe)
ba_imputed %>% group_by(churn)%>% summarise(number_customer_service_calls=n())
#gives the number of service calls for the number of people churned
str(ba_imputed) #to see the structure of data
plot(total_intl_charge ~ total_intl_calls, data=businessa, pch=as.integer(churn),col=as.integer(churn)+10)
legend("topright", legend = c("yes", "no"),pch =c(1,2))
#we use the above command and see that people who made less calls because of high intl charge churned more
pairs(businessa[,-c(1,3,4,5,19)])
subset(ba_imputed, churn %in% "yes") #subsetting people who have churned
churned<-subset(ba_imputed, churn %in% "yes") #created dataframe that shows the attributes for people who churned
View(churned)
write.csv(churned, file="D:\\BA\\project\\churned.csv" ); #saving the dataframe as a csv file
library(ggplot2)
p <- qplot(churn, total_day_calls, data=ba_imputed, facets=churn~.)
p+ theme_bw()
p1 <- qplot(churn, total_night_calls, data=ba_imputed, facets=churn~.)
p1 + theme_bw()
p2 <- qplot(churn, total_eve_calls, data=ba_imputed, facets=churn~.)
p2 + theme_bw()
p3 <- qplot(churn, total_intl_calls, data=ba_imputed, facets=churn~.)
p3 + theme_bw()

#J48 model
ctf<-read.csv('D:\\BA\\Churn_Train (1).csv');
preproc<-preProcess(ctf, method = c("medianImpute"))
ctf_Imp<-predict(preproc,ctf)
summary(ctf_Imp)
ctf_Imp$churn<-as.factor(ctf_Imp$churn)
library(RWeka)
modelv<-J48(churn~.,data=ctf_Imp)
ctf_imp1<-ctf_imp
ctf_Imp1<-  transform(ctf_Imp,Location_acclength=paste(state,area_code,account_length,sep="_"))
ctf_Imp1$state<-NULL
ctf_Imp1$area_code<-NULL
ctf_imp1$account_length<-NULL
View(ctf_Imp1)
modelv1<-J48(churn~., data=ctf_Imp1)
summary(modelv1)

#Linear Model
ctf_Imp2<-ctf_Imp1
ctf_Imp2$churn<-as.numeric(ctf_Imp2$churn)
modelv3<-lm(churn~.,data=ctf_Imp2)
summary(modelv3)
anova(modelv3)

#Logistic Regression
ctf<-read.csv('D:\\BA\\Churn_Train (1).csv');
library(ISLR)
library(dplyr)
library(caret)
preproc<-preProcess(ctf1, method = c("medianImpute"))
ctf1<-predict(preproc,ctf)
summary(ctf1)
install.packages("tree")
library(tree)
nzv<-nearZeroVar(ctf1)
nzv



ctf1$number_vmail_messages<-NULL

ctf1$state<-NULL
ctf2_imp<-ctf1
churnmodel<-glm(churn~.,family = binomial(link = "logit"),data=ctf2_imp)
summary(print(churnmodel))
Predictedd_Values<-predict (churnmodel, newdata= ctf2_imp, type="response")
roc(ctf2_imp$churn, Predictedd_Values)

#Tournament 1
ctf<-read.csv('D:\\BA\\Churn_Train (1).csv');
library(ISLR)
library(dplyr)
library(caret)
preproc<-preProcess(ctf, method = c("medianImpute"))
ctf_Imp<-predict(preproc,ctf)
summary(ctf_Imp)
install.packages("tree")
library(tree)
nzv<-nearZeroVar(ctf_Imp)
nzv



ctf_Imp$number_vmail_messages<-NULL

ctf_Imp$state<-NULL
treem <- tree(churn ~., data = ctf_Imp)
summary(treem)
plot(treem)
text(treem, pretty = 0)
ctf_Imp$predict <- predict(treem, data = ctf_Imp, type ="class")
confmatrix <- print(table(ctf_Imp$predict, ctf_Imp$churn, dnn=c("Predicted", "Actual")))
Accuracy <- print((confmatrix[2,2]+confmatrix[1,1])/sum(confmatrix) * 100)
Sensitivity<-print(confmatrix[2,2]/(confmatrix[2,2]+confmatrix[1,2])*100)
Specificity<-print(confmatrix[1,1]/(confmatrix[1,1]+confmatrix[2,1])*100)
ctf_test<-read.csv('D:\\BA\\project\\Tournament_Week1_Test.csv');
ctf_test$number_vmail_messages<-NULL
ctf_test$state<-NULL
testtt<-predict(treem, newdata = ctf_test)
summary(testtt)
View(testtt)
print(testtt)

#Tournament2
library(corrplot)
corrplot(cor(ctf_Imp[,5:17]))
ctf_Imp1<-ctf_Imp
ctf_Imp1$predict<-NULL
ctf_Imp1$total_eve_minutes<-NULL
ctf_Imp1$total_intl_minutes<-NULL
ctf_Imp1$total_night_minutes<-NULL
ctf_Imp1$total_day_minutes<-NULL
treem2 <- tree(churn ~., data = ctf_Imp1)
summary(treem2)
plot(treem2)
text(treem2)
ctf_Imp1$predict <- predict(treem, data = ctf_Imp1, type ="class")
confmatrix1 <- print(table(ctf_Imp1$predict, ctf_Imp1$churn, dnn=c("Predicted", "Actual")))
Accuracy1 <- print((confmatrix1[2,2]+confmatrix1[1,1])/sum(confmatrix1) * 100)
Sensitivity1<-print(confmatrix1[2,2]/(confmatrix1[2,2]+confmatrix1[1,2])*100)
Specificity1<-print(confmatrix1[1,1]/(confmatrix1[1,1]+confmatrix1[2,1])*100)
ctf_test1<-read.csv('D:\\BA\\project\\Tournament_Week2_Test.csv')
ctf_test1$state<-NULL
ctf_test1$number_vmail_messages<-NULL
ctf_test1$total_day_minutes<-NULL
ctf_test1$total_eve_minutes<-NULL
ctf_test1$total_intl_minutes<-NULL
ctf_test1$total_night_minutes<-NULL
testtt1<-predict(treem2, newdata = ctf_test1)
summary(testtt1)
View(testtt1)
print(testtt1)

#Tournament 3
ct<-read.csv('D:\\BA\\Churn_Train (1).csv');
preproc<-preProcess(ct, method = c("medianImpute"))

ctf_Imp<-predict(preproc,ct)
ctf_Imp$number_vmail_messages<-NULL
ctf_Imp$area_code<-NULL
ctf_Imp$total_eve_minutes<-NULL
ctf_Imp$total_intl_minutes<-NULL
ctf_Imp$total_night_minutes<-NULL
ctf_Imp$total_day_minutes<-NULL
treeee<-rpart(churn~.,data=ctf_Imp, method = "class")
summary(treeee)
plot(treeee)
text(treeee)
ctf_Imp$predict1 <- predict(treeee, data = ctf_Imp, type ="prob")
predict(treeee,ctf_Imp,method='prob')
roc(ctf_Imp$churn, ctf_Imp$predict1[,2])
testtt2<-predict(treeee, newdata = test2)
test2<-read.csv('D:\\BA\\project\\Tournament_Week3_Test (1).csv')
test2$number_vmail_messages<-NULL
test2$total_day_minutes<-NULL
test2$total_eve_minutes<-NULL
test2$total_night_minutes<-NULL

test2$total_intl_minutes<-NULL
test2$area_code<-NULL
testtt2<-predict(treeee, newdata = test2)
View(testtt2)

#Final model
ct<-read.csv('D:\\BA\\Churn_Train (1).csv');
preproc<-preProcess(ct, method = c("medianImpute"))

ctf_Imp<-predict(preproc,ct)
ctf_Imp$number_vmail_messages<-NULL
ctf_Imp$area_code<-NULL
ctf_Imp$total_eve_minutes<-NULL
ctf_Imp$total_intl_minutes<-NULL
ctf_Imp$total_night_minutes<-NULL
ctf_Imp$total_day_minutes<-NULL
treeee<-rpart(churn~.,data=ctf_Imp, method = "class")
summary(treeee)
plot(treeee)
text(treeee)

ctf_Imp$predict1 <- predict(treeee, data = ctf_Imp, type ="prob")
predict(treeee,ctf_Imp,method='prob')
roc(ctf_Imp$churn, ctf_Imp$predict1[,2])

testfinal<-read.csv('D:\\BA\\project\\FinalLeague_Test.csv')
testfinal$number_vmail_messages<-NULL
testfinal$total_day_minutes<-NULL
testfinal$total_eve_minutes<-NULL
testfinal$total_night_minutes<-NULL

testfinal$total_intl_minutes<-NULL
testfinal$area_code<-NULL
testtt3<-predict(treeee, newdata = testfinal)
View(testtt3)


