setwd("~/Research16/transplant/transplant93/SVM2")
###### load ML libraries ######
library(caret)
#names(getModelInfo())
library(e1071)

################################



set.seed(100)

alldata = read.table(file="hmp_new.txt",header=F,sep="\t",na.strings = "NA")
colnames(alldata) = c("Microbe","Similarity","Outcome")

# to calculate wilcox ranksum test
#bugs = read.table(file="23bugs.txt",header=FALSE,sep="\t")
#for ( i in unique(bugs$V1)){
#  selectSet = subset(alldata, Microbe == i, select = c(Similarity,Outcome))
#  a = wilcox.test(selectSet[selectSet$Outcome == 1,c("Similarity")],selectSet[selectSet$Outcome == 0,c("Similarity")])  
#  a = wilcox.test(Similarity ~ Outcome, data=selectSet)  
#  print(paste(i," ",a$p.value))
#}

selectBug = "Bacteroides_vulgatus"

selectSet = subset(alldata, Microbe == selectBug, select = c(Similarity,Outcome))
#library(outliers)
#outlier(selectSet[selectSet$Outcome == 1,c("Similarity")])

#count_1 = sum(selectSet[selectSet$Outcome == 1,c("Similarity")])
#total_row = count_1 * 2
#selectSet1 = selectSet[1:2,]  # for coprococcus comes
#selectSet[3,2] =0
#selectSet2 = selectSet[4:276,]
#selectSet = rbind(selectSet1,selectSet2)
#selectSet = selectSet[1:34,]
selectSet$Outcome = factor(selectSet$Outcome)
rownames(selectSet) = NULL

attach(selectSet)
#table(selectSet$Outcome)

######################################

# partition the data method 1 manual ########
rows_total = dim(selectSet)[1]
# split data into 75 25 as training and test dataset
count_pos = round(rows_total * 0.80)
count_neg = rows_total - count_pos

set.seed(100)
index = sample(rows_total,count_pos)
trainSet = selectSet[index,]

testSet = selectSet[-index,]


# method 2 using createDataPartiotion from Caret package
#index = createDataPartition(selectSet$Outcome, p=0.7, list = FALSE, times=1 )
#trainSet = selectSet[index,]
#testSet = selectSet[-index,]


rownames(trainSet) = NULL
rownames(testSet) = NULL

trainSet$Outcome <- ifelse(trainSet$Outcome == '1', "related", "unrelated")
testSet$Outcome <- ifelse(testSet$Outcome == '1', "related", "unrelated")

###### train the algorithm ##########################
set.seed(100)

trainSet$Outcome = factor(trainSet$Outcome)
cross_val = trainControl(method="repeatedcv",number=5,repeats = 5, classProb=T, savePred=T,summaryFunction = twoClassSummary)
model = train(Outcome ~.,data=trainSet, method = 'glm',family="binomial",trControl = cross_val,metric="ROC")



#### Testing ################

# testing training data itself

#predictions <- predict(model,newdata = selectSet,decision.values = T,probability=T)
#table(predictions)
#cm = confusionMatrix(predictions,selectSet[,c("Outcome")])
#table(trainSet[,c("Outcome")])
#cm$overall
#cm$overall['Accuracy']
#cm$byClass
#cm$byClass['Sensitivity']
#cm_overall = as.data.frame(t(cm$overall))
#cm_byClass = as.data.frame(t(cm$byClass))
#stats = cbind(selectBug,cm_overall,cm_byClass,max_value_0,min_value_1)

#print(paste(selectBug," ",cm$byClass['Sensitivity']," ",cm$byClass['Specificity']," ",cm$overall['Accuracy']))



#testing on test dataset


predictions <- predict(model, newdata = testSet,decision.values = T,probability=T)

confusionMatrix(predictions,testSet[,c("Outcome")])

table (Prediction = predictions, Truth = testSet[,c("Outcome")])
cm = confusionMatrix(predictions,testSet[,c("Outcome")])
print(paste(selectBug," ",cm$overall['Accuracy']," ",cm$byClass['Sensitivity']," ",cm$byClass['Specificity']))


#####################

set.seed(100)
selectSet$Outcome <- ifelse(selectSet$Outcome == '1', "related", "unrelated")
selectSet$Outcome = factor(selectSet$Outcome)
cross_val = trainControl(method="repeatedcv",number=5,repeats = 5, classProb=T, savePred=T,summaryFunction = twoClassSummary)
model2 = train(Outcome ~.,data=selectSet, method = 'glm',family="binomial",trControl = cross_val,metric="ROC")







# tesing on new linear dataset from 80 to 100
testNew = as.data.frame(seq(80,100,by=0.1))
colnames(testNew) = c("Similarity")

#library(class)
#x = knn(selectSet[,c("Similarity")], testNew, cl=selectSet[,c("Outcome")], k = 1, l = 0, prob = TRUE, use.all = TRUE)
#y=knn3Train(selectSet[,c("Similarity")], testNew,cl=factor(selectSet[,c("Outcome")]), k=1,  l=1, prob = TRUE, use.all=TRUE)

predictions <- predict(model2, newdata = testNew,decision.values = T,probability=T)

#cbind(testNew,predictions,attr(predictions, "probabilities"))
#cbind(testNew,x)
#cbind(testNew,y)
cbind(testNew,predictions)


# test on transplant dataset
transplant_data = read.table(file="transplant4.txt",header=FALSE,sep="\t")

colnames(transplant_data) = c("Microbe","Similarity","Outcome")

transplant_data$Outcome <- ifelse(transplant_data$Outcome == '1', "related", "unrelated")
transplant_data$Outcome = factor(transplant_data$Outcome)

TestSet2 = subset(transplant_data, Microbe == selectBug , select = c(Similarity))


rownames(TestSet2) = NULL

predictions <- predict(model, newdata = TestSet2,decision.values = T,probability=T)
cbind(TestSet2$Similarity,predictions)
#cbind(TestSet2,predictions,attr(predictions, "probabilities"))



TestSet3 = subset(transplant_data, Microbe == selectBug & Outcome == 'unrelated', select = c(Similarity,Outcome))
TestSet3$Outcome = factor(TestSet3$Outcome)
rownames(TestSet3) = NULL

predictions <- predict(model, newdata = TestSet3,decision.values = T,probability=T)
cbind(TestSet3$Similarity,predictions)
#cbind(TestSet3,predictions,attr(predictions, "probabilities"))
confusionMatrix(predictions,TestSet3[,c("Outcome")])
#cbind(TestSet3,predictions)


#for (selectBug in colnames(alldata[,3:ncol(alldata)-1])){
#  print(paste("The bug is", selectBug))
#}


