setwd("~/Research16/transplant/transplant93/SVM2")
###### load ML libraries ######
library(caret)
#names(getModelInfo())
library(e1071)

################################





alldata = read.table(file="hmp_new.txt",header=F,sep="\t",na.strings = "NA")
colnames(alldata) = c("Microbe","Similarity","Outcome")

selectBug = "Bacteroides_vulgatus"

selectSet = subset(alldata, Microbe == selectBug, select = c(Similarity,Outcome))
#selectSet1 = selectSet[1:2,]
#selectSet2 = selectSet[4:276,]
#selectSet = rbind(selectSet1,selectSet2)
#selectSet = selectSet[1:34,]
selectSet$Outcome = factor(selectSet$Outcome)
rownames(selectSet) = NULL

attach(selectSet)
table(selectSet$Outcome)

library(gmodels)
CrossTable(alldata$Microbe,alldata$Outcome)
######################################
set.seed(100)
# partition the data method 1 manual ########
rows_total = dim(selectSet)[1]
# split data into 75 25 as training and test dataset
count_pos = round(rows_total * 0.80)
count_neg = rows_total - count_pos

index = sample(rows_total,count_pos)
trainSet = selectSet[index,]

testSet = selectSet[-index,]


# method 2 using createDataPartiotion from Caret package
#index = createDataPartition(selectSet$Outcome, p=0.7, list = FALSE, times=1 )
#trainSet = selectSet[index,]
#testSet = selectSet[-index,]


rownames(trainSet) = NULL
rownames(testSet) = NULL



###### train the algorithm ##########################

cross_val = trainControl(method="repeatedcv",number=5)
set.seed(100)
model = train(Outcome ~.,data=trainSet, method = 'glm',family="binomial",trControl = cross_val)

set.seed(100)
model = train(Outcome ~.,data=selectSet, method = 'glm',family="binomial",trControl = cross_val)

#summary(model)



#model = glm(Outcome ~.,data=trainSet,family="binomial")
#predpr <- predict(model,type=c("response"))
#library(pROC)
#roccurve <- roc(trainSet$Outcome ~ predpr)
#plot(roccurve)
#auc(roccurve)


###############################

#testing on test dataset
predictions <- predict(model, newdata = testSet,decision.values = T,probability=T,type="prob")
confusionMatrix(predictions,testSet[,c("Outcome")])
confusionMatrix(predictions,testSet[,c("Outcome")],positive="1")
#table (Prediction = predictions, Truth = testSet[,c("Outcome")])


# tesing on new linear dataset from 80 to 100
testNew = as.data.frame(seq(80,100,by=0.1))
colnames(testNew) = c("Similarity")
predictions <- predict(model, newdata = testNew,decision.values = T,probability=T)
cbind(testNew,predictions)


# test on transplant dataset
transplant_data = read.table(file="transplant3.txt",header=FALSE,sep="\t")
colnames(transplant_data) = c("Microbe","Similarity","Outcome")

TestSet2 = subset(transplant_data, Microbe == selectBug , select = c(Similarity,Outcome))
rownames(TestSet2) = NULL

predictions <- predict(model, newdata = TestSet2,decision.values = T, probability=T)
cbind(TestSet2$Similarity,predictions)
#cbind(TestSet2,predictions,attr(predictions, "probabilities"))
confusionMatrix(predictions,TestSet2[,c("Outcome")])
confusionMatrix(predictions,TestSet2[,c("Outcome")],positive="1")

TestSet3 = subset(transplant_data, Microbe == selectBug & Outcome == 0, select = c(Similarity,Outcome))
TestSet3$Outcome = factor(TestSet3$Outcome)
rownames(TestSet3) = NULL

predictions <- predict(model, newdata = TestSet3,decision.values = T,probability=T)
cbind(TestSet3$Similarity,predictions)
cbind(TestSet3,predictions,attr(predictions, "probabilities"))
confusionMatrix(predictions,TestSet3[,c("Outcome")])
#cbind(TestSet3,predictions)


a= c(1,0,0,0,1,0,1,0)
b= c(1,0,0,0,1,0,0,0)
confusionMatrix(b,a)





