ca<-read.csv("D:/R/submission/Projects/College_admission.csv", header = TRUE)
head(ca) 

# Missing Data 
na.omit(ca) 
summary(ca) 

<No Missing Data>

# OUTLIERS

ggplot()+geom_point(aes(x=ca$gre,y=ca$gpa),colour='red')+ggtitle('outliers')+xlab('GREScore')+ylab('GPA')

#No Outlier
#Graph shows No outliers

#Numeric to Factor and vice-versa 
ca$admit = factor(ca$admit, levels = c("0","1"), labels = c("Accepted","Rejected")) 
ca$ses = factor(ca$ses, levels = c("1","2",'3'), labels = c("Low","Medium",'High'))
ca$Gender_Male = factor(ca$Gender_Male, levels = c("0","1"), labels = c("Female","Male"))
ca$Race = factor(ca$Race, levels = c("1","2",'3'), labels = c("Hispanic","Asian",'African-American'))


#Categorising GRE Marks to Category

print(ca)
for (row in 1:nrow(ca)) {
  if(ca[row, "gre"] <= 440)
    ca[row, "gre"] ="Low"
  else if(ca[row, "gre"] > 440 && ca[row, "gre"] <= 580)
    ca[row, "gre"] = "Medium"
  else
    ca[row, "gre"] = "High"
}
print(ca)

summary(ca)
      admit         gre                 gpa            ses      Gender_Male                Race    
 Accepted:273   Length:400         Min.   :2.260   Low   :132   Female:210   Hispanic        :143  
 Rejected:127   Class :character   1st Qu.:3.130   Medium:139   Male  :190   Asian           :129  
                Mode  :character   Median :3.395   High  :129                African-American:128  
                                   Mean   :3.390                                                   
                                   3rd Qu.:3.670                                                   
                                   Max.   :4.000                                                   
      rank      
 Min.   :1.000  
 1st Qu.:2.000  
 Median :2.000  
 Mean   :2.485  
 3rd Qu.:3.000  
 Max.   :4.000  

ca_norm<-read.csv("D:/R/submission/Projects/College_admission.csv", header = TRUE)

# Nomality of Distribution

sd(ca_norm$gre) 
library(dplyr) 
library("ggpubr") 
set.seed(1234) 
dplyr::sample_n(training_set, 30) 
ggdensity(ca_norm$gre, main = "Density plot of GRE", xlab = "GRE") 
ggdensity(ca_norm$gpa, main = "Density plot of GPA", xlab = "gpa") 
ggdensity(ca_norm$ses, main = "Density plot of SES", xlab = "ses") 
ggdensity(ca_norm$Gender_Male, main = "Density plot of Gender_Male", xlab = "Gender_Male") 
ggdensity(ca_norm$Race, main = "Density plot of Race", xlab = "Race") 
ggdensity(ca_norm$rank, main = "Density plot of Rank", xlab = "rank") 

# Graphs Saved
                  
#variable reduction techniques to identify significant variables
ca_reg<-read.csv("D:/R/submission/Projects/College_admission.csv", header = TRUE)


library(e1071) 
library(caTools)

#Split the dataset into training and Test
set.seed(1234) 
split = sample.split(ca_reg$admit, SplitRatio = .08) 
trainingR_set = subset(ca_reg, split == TRUE) 
testR_set = subset(ca_reg, split == FALSE) 

#Scaling
trainingR_set[,2:7] = scale(trainingR_set[,2:7]) 
testR_set[,2:7] = scale(testR_set[,2:7]) 

cls_lm = glm(formula = admit ~ ., family = binomial(), data = trainingR_set) 
summary(cls_lm)
p_pred = predict(cls_lm, type = "response", newdata = trainingR_set[,-1]) 
trainingR_set$prob1 = p_pred

insig_lm = lm(formula = prob1 ~ gre + gpa + ses + Gender_Male + Race + rank,
              data = trainingR_set)
summary(insig_lm)
              
reg_pred = predict(insig_lm, newdata = test_set)
testR_set$prob1 = reg_pred
summary(reg_pred)

#OutPut
#Gender and Rank are insignificant Variable.

ca_cls<-read.csv("D:/R/submission/Projects/College_admission.csv", header = TRUE)


#Split the dataset into training and Test
set.seed(1234)
split = sample.split(ca_cls$admit, SplitRatio = .08) 
trainingC_set = subset(ca_cls, split == TRUE) 
testC_set = subset(ca_cls, split == FALSE) 


# Scale the Data 
trainingC_set[,2:7] = scale(trainingC_set[,2:7]) 
testC_set[,2:7] = scale(testC_set[,2:7]) 
 
cls_lm = glm(formula = admit ~ ., family = binomial(), data = trainingC_set) 
summary(cls_lm)

#predict for Test function 
p_pred = predict(cls_lm, type = "response", newdata = testC_set[,-1]) 
prob = ifelse(p_pred > 0.5, 1, 0)


#confusion Matrix 
cm_lm = table(testC_set[,1], prob)
cm_lm

#Accuracy of the Model
accuracy = (cm_lm[1,1] + cm_lm[2,2]) / (cm_lm[1,1] + cm_lm[2,2] + cm_lm[1,2] + cm_lm[2,1])
accuracy

# Accuracy 61%

#KNN Model
library(class)
p_pred_KNN = knn(train = trainingC_set[,-1],
             test = testC_set[,-1],
             cl = trainingC_set[, 1])

#confusion Matrix 
cm_knn = table(testC_set[,1], p_pred_KNN)
cm_knn

#Accuracy of the Model
accuracy = (cm_knn[1,1] + cm_knn[2,2]) / (cm_knn[1,1] + cm_knn[2,2] + cm_knn[1,2] + cm_knn[2,1])
accuracy
#Accuracy = 57%

#Decision Tree
library(rpart)
training_set$admit = factor(trainingC_set$admit)
cls_DT = rpart(formula = admit ~ .,
               data = trainingC_set)
print(cls_DT)

p_pred_DT = predict(cls_DT, newdata = test_set[-1],  type = "class")

cm_DT = table(testC_set[,1], p_pred_DT)
cm_DT
#Accuracy = 57%

#Accuracy of the Model
accuracy = (cm_DT[1,1] + cm_DT[2,2]) / (cm_DT[1,1] + cm_DT[2,2] + cm_DT[1,2] + cm_DT[2,1])
accuracy

# Naive Bayes

cls_nb = naiveBayes(x = trainingC_set[-1],
                    y = trainingC_set$admit)
p_pred_nb = predict(cls_nb, newdata = testC_set[-1],  type = "class")

cm_nb = table(testC_set[,1], p_pred_nb)
cm_nb

#Accuracy of the Model
accuracy = (cm_nb[1,1] + cm_nb[2,2]) / (cm_nb[1,1] + cm_nb[2,2] + cm_nb[1,2] + cm_nb[2,1])
accuracy

#Accuracy = 63%

#SVM Model AND VALIDATION TECHNIQUE

install.packages('caret')
library(caret)
library(e1071)
folds = createFolds(trainingC_set$admit, k = 5)
cv = lapply(folds, function(k){
  training_fold = trainingC_set[-k, ]
  test_fold = trainingC_set[k, ]
  cls_svm = svm(formula = admit ~ .,
              data = trainingC_set,
              type = 'C-classification',
              kernel = 'sigmoid')

  p_pred_svm = predict(cls_svm, newdata = test_fold[-1])
  p_pred_svm
  cm_svm = table(test_fold[, 1], p_pred_svm)
  cm_svm
  acc_svm = (cm_svm[1,1] + cm_svm[2,2]) / (cm_svm[1,1] + cm_svm[2,2] + cm_svm[1,2] + cm_svm[2,1])
  (acc_svm)
})


acc_svm = mean(as.numeric(cv))
acc_svm

#Accuracy = 69%
