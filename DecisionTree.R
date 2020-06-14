#VU assignment 2

mushroom_data = read.csv("/Users/AmitShahi/Downloads/Datawarehousefiles/mushrooms.csv", stringsAsFactors = TRUE)
str(mushroom_data)
mushroom_data$type <- as.factor(mushroom_data$type)
str(mushroom_data)
# Data Partition
set.seed(1234)
pd <- sample(2, nrow(mushroom_data), replace = TRUE, prob=c(0.8,0.2))
train_mush <- mushroom_data[pd==1,]
train_mush


validate <- mushroom_data[pd==2,]
validate

#Decision Tree with party
library(party)
library(tree)
library(rpart)
library(rpart.plot)

tree <- ctree(type~ ., data =  train_mush)
tree # there are 15 nodes in this tree
plot(tree)

#Pruning the tree
tree <- ctree(type~ ., data =  train_mush, controls = ctree_control(mincriterion = 0.99,minsplit = 500))
tree ##now we have only 11 nodes


plot(tree)



#Prediction
predict(tree,validate,type="prob") 
#this give the probability whether the mushroom would be edible or poisonous.Take example of observation
#[979], the probability of this mushroom to be edibile is 0.605, and the probability of being poisonous
#is 0.394
#without  probability
predict(tree,validate) #if we remove probabilty ,then this gives mushroom whether
#either belongs to 'edible' or 'poisonous' class.
#Decision tree with rpart
tree1 <- rpart(type~., train_mush)
rpart.plot(tree1)
#rpart.plot(tree1,extra1)
#rpart.plot(tree1,extra2)
#rpart.plot(tree1,extra3)
#rpart.plot(tree1,extra4) # you will get the probability
#Prediction
predict(tree1,validate)# now we get probabilty of all 1602 observations in validate dataset
#absent data points belong to training data, whereas present data points   belong to validation set ..i.e
#first 3 data i.e 1,2,3 data belong to traing data and 4th, 6th, 8th, 14th, 26th belong to testing dataset.
#missing dataset belong to training dataset
#Misclassification Error for train data
#(tab1 <- table(p1, train_mush))
tab <- table(predict(tree), train_mush$type)
print(tab)
#top row edible and poisonous are reality, and column edible and poisonous are prediciton
#3372 mushroom are actually in real edible, and the model also predicts them to be edible
#38 mushroom are actually poisonous, but model predicts them edible ,this is a misclassification
#3104 mushroom are actually in real poisonous, and the model also predicts them to be poisonous




1-sum(diag(tab))/sum(tab) # 0.005833589 ~ 0.58% off training
#misclassification error in training is 0.58%


#Misclassification Error for testing data
testPred <- predict(tree, newdata=validate)
tab <-table(testPred,validate$type)
print(tab)
#836 mushroom which are edible in reality is predicted edible by model.
#10 mushroom which are poisonous in reality are predicted edible by model. (misclafficiation here)
#764 mushroom which are poisonous in reality are predicted poisonous by model.

1-sum(diag(tab))/sum(tab) # miscalculation in testing/validation is 0.00621118 ~ 0.6211%
#out of 1610, 836 edible mushroom, 764 poisonous mushroom are properly predicted, whereas 10 mushroom are 
#mispredicted.(actually, those 10 are poisonous in reality but our model predicts them
# as edible).Thus miscalculation is 0.00621118%, therefor total accuracy is 99.3789%
#accuracy on mushroom detection by decision tree is better than naive bayes




#ACUTUAL VS PREDICTED IN CONFUSION MATRIX
dtree.perf <- table(validate$type, testPred, dnn=c("Acutual","Predicted"))
dtree.perf



  



