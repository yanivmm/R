library(ISLR)
library(ggplot2)
library(tree)
library(ggplot2)
library(randomForest)
library(tibble)

#reading data
D = read.table("C:/Users/97250/Desktop/עבודה/titanic kaggle competition/train.csv",sep = ",",header = T)
test = read.table("C:/Users/97250/Desktop/עבודה/titanic kaggle competition/test.csv",sep = ",",header = T)

#duplicating data in order to restore important data
originalData = D

#filtering data without unfactorized data
D = D[,-(which(colnames(D) == "Name"))]
D = D[,-(which(colnames(D) == "Cabin"))]
D = D[,-(which(colnames(D) == "Ticket"))]


#deviding data to train and test
set.seed(1)
num_train_row = round(nrow(D)*0.7)
train.index = round(runif(num_train_row, 1, nrow(D)))
train.set = D[train.index,]
test.set  = D[-train.index,] 


#GGplot
ggplot(data = D) + geom_point(mapping = aes(x = D$SibSp ,y = D$Age, col = D$Survived ))+
  scale_color_gradient(low="red", high="blue")

#tree model
treefit = tree(Survived ~ ., data=train.set)
summary(treefit)
plot(treefit)
text(treefit,cex=0.7)

#logistic regression model
reg =  glm(Survived ~ ., family = binomial, data =train.set)
regPValues = summary(reg)$coefficients[,4]  
sortedRegPValues = sort(regPValues)[1:5]


#counting missing Age values
test.set[which(is.na(test.set$Age)),"PassengerId"]


#prediction by logistic Model

test.predictions = predict(reg, test.set , type = "response")
predictions = rep("No",length(test.set$PassengerId))
predictions [test.predictions > 0.5] = "Yes"
confusion1 = table(predictions,test.set$Survived)
accuricyLogisReg = (confusion1[2,2]+confusion1[1,1])/(nrow(test.set))


#prediction by tree model
test.predictions2 = predict(treefit, test.set)
predictions2 = rep("No",length(test.set$PassengerId))
predictions2 [test.predictions2 > 0.5] = "Yes"
confusion2 = table(predictions2,test.set$Survived)
accuricyTreeFit = (confusion2[2,2]+confusion2[1,1])/(nrow(test.set))


#prediction by pruned tree based on the 'elbow' method

accuricy = rep(0,9)
for (i in 1:9)
{
  prunetree = prune.tree(treefit,best=(i+1))
  test.predictions = predict(prunetree, test.set)
  predictions = rep("No",length(test.set$PassengerId))
  predictions [test.predictions > 0.5] = "Yes"
  confusion = table(predictions,test.set$Survived)
  accuricy[i] = (confusion[2,2]+confusion[1,1])/(nrow(test.set))
}
numOfLeaf = seq(from = 2, to = 10, by = 1)
plot(accuricy~numOfLeaf, type = "b")
prunetreeFinal = prune.tree(treefit,best=4)
plot(prunetreeFinal)
text(prunetreeFinal,cex=0.7)

#creating results vector by prunedTree model (80% accuricy)
test.predictions3 = predict(prunetreeFinal, test)
predictions3 = rep("No",length(test$PassengerId))
predictions3 [test.predictions3 > 0.5] = "Yes"
resultsByPrunedtreeModel =data.frame(test$PassengerId,predictions3)


# קוד ארוך המבוסס על חלוקת הדאטה המקורי
# 'לפי כמה מאפיינים בסדר לפי המאפיין:' שם
# מסודר בטבלת שכיחות 

table(originalData[which(D$SibSp<3  & D$Age<18 & D$Sex == "female"),
      c("Name","Age","Sex","Survived","Pclass","Parch","SibSp")]
      [order(originalData[which(D$SibSp<3 & D$Age<18 &
            D$Sex == "female"),]$Name),]$Survived)


# קוד המפלח אתאוכלוסיית הזוגות הצעירים לפי אחוזי הישרדות
# ומציע מדד בין נשים לגברים

couplesSurvivngTable = table(originalData[which(D$SibSp==1  & D$Parch==0),]$Survived
                             ,originalData[which(D$SibSp==1 & D$Parch==0),]$Sex)
ratioMenFromYoungCoupleSurvived = couplesSurvivngTable[1,2]/couplesSurvivngTable[2,2]
ratioWomenFromYoungCoupleSurvived = couplesSurvivngTable[2,1]/couplesSurvivngTable[1,1]
weightedRatioWomenPartMenFromYoungCoupleSurvived = ratioWomenFromYoungCoupleSurvived/
                                           ratioMenFromYoungCoupleSurvived


# קוד אחר המפלח את כלל האוכלוסיה לפי אחוזי הישרדות
# ומציע מדד בין נשים לגברים

sexSurvivngTable = table(originalData[,]$Survived,originalData[,]$Sex)
weightedRatioWomenPartMenFromAllPassengersSurvived = (sexSurvivngTable[2,1]/sexSurvivngTable[1,1])/
  (sexSurvivngTable[2,2]/sexSurvivngTable[1,2])


#פילוח שורדים לפי מספר אחים בטבלת שכיחות 

for (i in 5:0){
print(table(originalData[which(D$SibSp>=i & D$Age<18 ),]$Survived))
              }


# הקוד הזה הוא היחס בקרב ילדים בעלי כל וריאציות האחים 
# מסודרים כוקטור יחס בין מספר המתים לשורדים
# היחס הרביעי  מציין את יחס המתים הגבוה ביותר והוא בקרב 3 אחים

for (i in 5:0){
  b[i] = (table(originalData[which(D$SibSp>=i & D$Age<18 ),]$Survived)[1]
          /(table(originalData[which(D$SibSp>=i & D$Age<18 ),]$Survived)[2]))}
print(b)

# בכל מחלקה עם או בלי בני זוג,בוגרים כמה שרדו ביסח למין
 
for (i in 0:1) {
            print(table(D[which(D$Parch<3 & D$Parch>0 & D$Age>18 & D$SibSp==i ),]$
                  Survived,D[which(D$Parch<3 & D$Parch>0  & D$Age>18 & D$SibSp==i),]$Sex))
               }
 

#טבלת שכיחות של ילדים שורדים בקרב אלו שיש להם הורה אחד  או שניים עם פחות מ-3 אחים

for (i in 1:2) {
       for (j in 0:2) {
             print(table(originalData[which(D$Parch==i  & D$Age<18 &D$SibSp==j),
                  c("Name","Age","Sex","Survived","Pclass","Parch","SibSp")]$Survived))} }


# קוד שתפקידו לדלות מן עץ ההחלטה את הערך הכי משמעותי בקבלת ההחלטה

maxDev =max(treefit$frame$dev)
treefit$frame$var[which(treefit$frame$dev==maxDev)]

# הערך השני הכי משמעותי בעץ ההחלטה וכן הלאה

sortedDev =sort(treefit$frame$dev,decreasing = TRUE)
treefit$frame$var[which(treefit$frame$dev==sortedDev[2])]
treefit$frame$var[1:5]

vector = rep(NA,nrow(test.))
test = add_column(test,vector,.after = 1)
colnames(test)[which(colnames(test)=="vector")] = 'Survived'
alldata = rbind(originalData,test)
