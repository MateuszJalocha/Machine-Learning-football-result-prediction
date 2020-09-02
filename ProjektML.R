#Script with creating models

library(boot)
library(pROC)
library(xgboost) # for xgboost
library(tidyverse) # general utility functions
####Funkcje

#funkcja do wyliczania sredniej
avgFunction <- function(dane, druzyny, teamCol, homeORaway) {
  
  avgScore <- list()
  for(j in 1:length(druzyny))
  {
    suma <- 0
    #Ile poszczegolna druzyna srednio zdobywa
    avgHT <- c(0)
    
    if(homeORaway == "home"){
      htGoals <- dane[which(dane$HomeTeam == druzyny[j]),teamCol]
    }
    
    if(homeORaway == "away"){
      htGoals <- dane[which(dane$AwayTeam == druzyny[j]),teamCol]
    }
    
    for(i in 2:(length(htGoals) + 1))
    {
      suma <- suma + htGoals[i-1]
      avgHT[i]<- suma/length(avgHT)
    }
    
    avgScore[[druzyny[j]]] <- avgHT[-length(avgHT)]
  }
  
  return(avgScore)
}

#####webscrapping
library('rvest')

#Adres url
url <- 'https://www.fifaindex.com/pl/teams/fifa18_278/?league=50&order=desc'

#Odczytywanie html
webpage <- read_html(url)

#Wybranie rankingow
rating_data_html <- html_nodes(webpage,'td:nth-child(7) .r3')
name_data_html <- html_nodes(webpage, 'td+ td .link-team')

#Konwertowanie do tekstu
rating_data <- html_text(rank_data_html)
rating_data <- as.numeric(rating_data)
rating_data <- rating_data[seq(4,48, 4)]

name_data <- html_text(name_data_html)
name_data <- gsub(' [A-z ]*', '' , name_data)
name_data[2] <- "Rangers"
name_data[8] <- "St Johnstone"
name_data[7] <- "Ross County"

#ovrl
overall <- list()
for( i in 1:length(name_data)){
  overall[[name_data[i]]] <- rating_data[i]
}


####Przygotowanie danych
dane <- read.csv("ScotPrem.csv", dec = ",", sep = ";")

#Czy domownicy strzela
homeScore <- dane$FTHomeGoals

#Stworzenie tabeli
druzyny <- as.character(unlist(unique(dane$HomeTeam)))

homeOrAway <- list()
punkty <- list()
for(i in 1:length(druzyny)) {
  daneDruz <- dane[which(dane$HomeTeam == druzyny[i] | dane$AwayTeam == druzyny[i]),]
  pkt <- 0
  sumcia <- 0
  typ <- vector()
  for(j in 1:nrow(daneDruz)) {
    res <- daneDruz[j,c(3,4,7)]
    
    if(res$HomeTeam == druzyny[i] & res$FTRes == "H") {
      sumcia <- pkt[length(pkt)] + 3
      pkt[j] <- sumcia
      typ[j] <- "H"
    }
    if(res$HomeTeam == druzyny[i] & res$FTRes == "D") {
      sumcia <- pkt[length(pkt)] + 1
      pkt[j] <- sumcia
      typ[j] <- "H"
    }
    if(res$HomeTeam == druzyny[i] & res$FTRes == "A") {
      sumcia <- pkt[length(pkt)] + 0
      pkt[j] <- sumcia
      typ[j] <- "H"
    }
    if(res$AwayTeam == druzyny[i] & res$FTRes == "A") {
      sumcia <- pkt[length(pkt)] + 3
      pkt[j] <- sumcia
      typ[j] <- "A"
    }
    if(res$AwayTeam == druzyny[i] & res$FTRes == "D") {
      sumcia <- pkt[length(pkt)] + 1
      pkt[j] <- sumcia
      typ[j] <- "A"
    }
    if(res$AwayTeam == druzyny[i] & res$FTRes == "H") {
      sumcia <- pkt[length(pkt)] + 0
      pkt[j] <- sumcia
      typ[j] <- "A"
    }
  }
  punkty[[druzyny[i]]] <- c(0,pkt[-length(pkt)])
  homeOrAway[[druzyny[i]]] <- typ
}

#Wynik do postaci 0-1
wynik <- as.character(unlist(dane$FTRes))

wynik[wynik == "H"] <- 1
wynik[wynik != 1] <- 0

wynik <- as.numeric(wynik)

dane$FTRes <- wynik

#Overall
dane <- data.frame(dane, HomeOverall = rep(1,nrow(dane)),AwayOverall = rep(2,nrow(dane)))

for(i in 1:length(druzyny)) {
  dane[which(dane$HomeTeam==druzyny[i]),24] <- overall[[druzyny[i]]]
  dane[which(dane$AwayTeam==druzyny[i]),25] <- overall[[druzyny[i]]]
}

#Liczba srednia zmiennych
avgFTHGoals <- avgFunction(dane, druzyny, 5, "home")
avgHomeLost <- avgFunction(dane, druzyny, 6, "home")
avgFTAGoals <- avgFunction(dane, druzyny, 6, "away")
avgAwayLost <- avgFunction(dane, druzyny, 5, "away")
avgHTShots <- avgFunction(dane, druzyny, 12, "home")
avgATShots <- avgFunction(dane, druzyny, 13, "away")
avgHTTarget <- avgFunction(dane, druzyny, 14, "home")
avgATTarget <- avgFunction(dane, druzyny, 15, "away")
avgHomeFouls <- avgFunction(dane, druzyny, 16, "home")
avgAwayFouls <- avgFunction(dane, druzyny, 17, "away")
avgHTCorners <- avgFunction(dane, druzyny, 18, "home")
avgATCorners <- avgFunction(dane, druzyny, 19, "away")
avgHomeYellow <- avgFunction(dane, druzyny, 20, "home")
avgAwayYellow <- avgFunction(dane, druzyny, 21, "away")
avgHomeRed <- avgFunction(dane, druzyny, 22, "home")
avgAwayRed <- avgFunction(dane, druzyny, 23, "away")


#Przypisanie nowych zmiennych
for(i in 1:length(druzyny)){
  #Goals
  dane[which(dane$HomeTeam == druzyny[i]),5] <- avgFTHGoals[[druzyny[i]]]
  dane[which(dane$HomeTeam == druzyny[i]),8] <- avgHomeLost[[druzyny[i]]]
  dane[which(dane$AwayTeam == druzyny[i]),6] <- avgFTAGoals[[druzyny[i]]]
  dane[which(dane$AwayTeam == druzyny[i]),9] <- avgAwayLost[[druzyny[i]]]
  
  #Shots
  dane[which(dane$HomeTeam == druzyny[i]),12] <- avgHTShots[[druzyny[i]]]
  dane[which(dane$AwayTeam == druzyny[i]),13] <- avgATShots[[druzyny[i]]]
  dane[which(dane$HomeTeam == druzyny[i]),14] <- avgHTTarget[[druzyny[i]]]
  dane[which(dane$AwayTeam == druzyny[i]),15] <- avgATTarget[[druzyny[i]]]
  
  #Fouls
  dane[which(dane$HomeTeam == druzyny[i]),16] <- avgHomeFouls[[druzyny[i]]]
  dane[which(dane$AwayTeam == druzyny[i]),17] <- avgAwayFouls[[druzyny[i]]]
  
  #Corners
  dane[which(dane$HomeTeam == druzyny[i]),18] <- avgHTCorners[[druzyny[i]]]
  dane[which(dane$AwayTeam == druzyny[i]),19] <- avgATCorners[[druzyny[i]]]
  
  #Cards
  dane[which(dane$HomeTeam == druzyny[i]),20] <- avgHomeYellow[[druzyny[i]]]
  dane[which(dane$AwayTeam == druzyny[i]),21] <- avgAwayYellow[[druzyny[i]]]
  dane[which(dane$HomeTeam == druzyny[i]),22] <- avgHomeRed[[druzyny[i]]]
  dane[which(dane$AwayTeam == druzyny[i]),23] <- avgAwayRed[[druzyny[i]]]
}

#Add points
dane <- cbind(dane, HomePoint = rep(1, nrow(dane)), AwayPoint = rep(2,nrow(dane)))
for(i in 1:length(druzyny)) {
  dane[which(dane$HomeTeam==druzyny[i]),26] <- punkty[[druzyny[i]]][which(homeOrAway[[druzyny[i]]] == "H")]
  dane[which(dane$AwayTeam==druzyny[i]),27] <- punkty[[druzyny[i]]][which(homeOrAway[[druzyny[i]]] == "A")]
}

####Tworzenie osatecznego data frame
dane <- dane[-c(1:12),]
dane <- dane[,c(3,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20,21,24,25,26,27)]
colnames(dane) <- c("HomeTeam", "AwayTeam", "HomeGoals","AwayGoals", "Result", "HomeLost", "AwayLost","HomeShots","AwayShots",
                    "HomeTarget", "AwayTarget", "HomeFouls", "AwayFouls", "HomeCorners", "AwayCorners","HomeYellow", "AwayYellow",
                    "HomeOverall", "AwayOverall","HomePoints", "AwayPoints")

dane <- data.frame(dane$HomeTeam, dane$AwayTeam, dane$Result,HomeScore = homeScore[-c(1:12)], AVGGoals = dane$HomeGoals - dane$AwayGoals, AVGLost = dane$HomeLost - dane$AwayLost,
                   AVGShots = dane$HomeShots - dane$AwayShots, AVGTarget = dane$HomeTarget - dane$AwayTarget,
                   AVGCorners = dane$HomeCorners - dane$AwayCorners, AVGFouls = dane$HomeFouls - dane$AwayFouls,
                   OVR = 1-(dane$HomeOverall/dane$AwayOverall), PointsDiff = dane$HomePoints-dane$AwayPoints)


dane[which(dane$HomeScore >0 ),4] <- 1
dane <- dane[,-4]
ggplot(dane, aes(x = as.factor(dane.Result), y = dane$PointsDiff)) + geom_boxplot()
####Badanie
set.seed(123)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(dane))
train_ind <- sample(seq_len(nrow(dane)), size = smp_size)

train <- dane[train_ind, ]
test <- dane[-train_ind, ]

y_train1 <- train[,3]
train1 <- train[,-c(1,2,4)]

y_test1 <-test[,3]
test1 <- test[,-c(1,2,4)]


#####Porownanie
train <- train[,-c(1,2,4)]
test <- test[,-c(1,2,4)]

train[,1] <- as.factor(train[,1])
test[,1] <- as.factor(test[,1])

summary(train)
summary(test)
#Boxplots



##Liczba k
acc = sen = spec = c()
for(k in 1:30){
  train.knn1 <- knn(train = train1, test = test1, cl= y_train1,k = k,prob=TRUE)
  confM <- table(train.knn1,y_test1)
  acc[k] = (confM[2,2]+confM[1,1])/sum(confM)
  sen[k] = confM[2,2]/(confM[2,2]+confM[1,2])
  spec[k] = confM[1,1]/(confM[1,1]+confM[2,1])
  
}

accurancy <- data.frame(acc, k=1:30)
ggplot(accurancy, aes(x = k, y = acc)) + geom_line() + geom_point()
specc <- data.frame(spec, k=1:30)
ggplot(specc, aes(x = k, y = spec)) + geom_line() + geom_point()
sens <- data.frame(sen, k=1:30)
ggplot(sens, aes(x = k, y = sen)) + geom_line() + geom_point()

#Wyniki knn
train.knn1 <- knn(train = train1, test = test1, cl= y_train1,k = 12,prob=TRUE)
tab.knn.u1 <- table(train.knn1,y_test1)

acc <- sum(diag(tab.knn.u1)/sum(tab.knn.u1))

specyf <- sensitivity(tab.knn.u1)

czulosc <- specificity(tab.knn.u1)

plot(roc(y_test1, as.numeric(train.knn1)))



#Naiwna Bajesa
classifier <- naiveBayes(dane.Result~.,train)
y_pred <- predict(classifier, newdata = test[,-which(colnames(test)=="dane.Result")])

bayes_wynik <- table(test$dane.Result,y_pred)




################# Fitting Decision Tree Classification Model to the Training set
classifier_tree = rpart(dane.Result ~ ., data = train, method = 'class')

# Tree Visualization
rpart.plot(classifier_tree, extra=4)


# Predicting the Validation set results
y_pred = predict(classifier_tree, newdata = test[,-which(names(test)=="dane.Result")], type='class')

# Checking the prediction accuracy
tree_tab <- table(test$dane.Result, y_pred) # Confusion matrix

error <- mean(test$dane.Result != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))


# Applying k-Fold Cross Validation
set.seed(123)
folds = createMultiFolds(train$dane.Result, k = 12, times = 100)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(dane.Result ~ ., data = train, method = "rpart", trControl = control)

# Tree Visualization
rpart.plot(classifier_cv$finalModel, extra=4)


plot(roc(y_test1, as.numeric(y_pred)))
######################Random forest
classifier_forest = randomForest(as.factor(Result) ~ ., data = train, ntree= 24)

# Choosing the number of trees
plot(classifier_forest)

# Predicting the Validation set results
y_pred_forest = predict(classifier_forest, newdata = test[,-which(names(test)=="Result")])

# Checking the prediction accuracy
table(test$dane.Result, y_pred_forest) # Confusion matrix

plot(roc(y_test1, as.numeric(y_pred_forest)))



# Feature Importance
gini = as.data.frame(importance(classifier_forest))
gini = data.frame(Feature = row.names(gini), 
                  MeanGini = round(gini[ ,'MeanDecreaseGini'], 2))
gini = gini[order(-gini[,"MeanGini"]),]

ggplot(gini,aes(reorder(Feature,MeanGini), MeanGini, group=1)) + 
  geom_point(color='red',shape=17,size=2) + 
  geom_line(color='blue',size=1) +
  scale_y_continuous(breaks=seq(0,60,10)) +
  xlab("Feature") + 
  ggtitle("Mean Gini Index of Features") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





logistic <- glm(dane.Result~., family = "binomial", data = train[,-c(1,2)])
prob <- predict(logistic, train[,-c(1,2)], type = "response")
pred <- ifelse(prob>0.5,1,0)
confM <- table(pred, y_train1)
confM
acc = (confM[2,2]+confM[1,1])/sum(confM)
sen = confM[2,2]/(confM[2,2]+confM[1,2])
spec = confM[1,1]/(confM[1,1]+confM[2,1])
a <- roc(y_train1,pred)
plot(a, main="Regresja logistyczna ROC")
auc(a)



train.knn1 <- NULL
error.rate <- NULL

for (i in 1:30) {
  train.knn1 <- knn(train = train1, test = test1, cl= y_train1,k = k,prob=TRUE)
  error.rate[i] <- mean(train.knn1!=y_test1)
  
}

knn.error <- as.data.frame(cbind(k=1:30,error.type =error.rate))

ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:30)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')
