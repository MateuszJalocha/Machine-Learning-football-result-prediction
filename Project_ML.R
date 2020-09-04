####Libraries####
#Machine Learning
library(mlr) 
library(class)

#Plots
library(ggplot2)
library(corrplot)
library(rpart.plot)

#Webscrapping
library('rvest')

#Rmd
library(gridExtra)
library(flextable)
library(officer)
library(kableExtra)

#Generally useful libraries
library(tidyverse)
library(psych)
library(caTools)
library(reshape2)

####Functions####

#Create a table with regulartable library
frame_func <- function(frame) {
  big_b <- fp_border(color="gray70", width = 1)
  std_b <- fp_border(color="gray70")
  
  frame %>% 
    regulartable() %>% 
    autofit() %>% 
    width(width = 2) %>% 
    fontsize(part = "all", size = 15) %>% 
    align(part = "all", align = "center") %>% 
    vline(border = big_b, part = "all" ) %>%
    vline_left(border = big_b, part = "all" ) %>% 
    vline_right(border = big_b, part = "all" ) %>% 
    hline(border = std_b ) %>% 
    hline_bottom(border = big_b, part = "all") %>% 
    hline_top(border = big_b, part = "all" ) %>%
    font(part = "all",fontname = "Times") %>% 
    bold(part = "header")
}

#Calculate the average
avgFunction <- function(dane, druzyny, teamCol, homeORaway) {
  
  avgScore <- list()
  for(j in 1:length(druzyny))
  {
    suma <- 0
    #How much an individual team wins on average
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

#Select statistics such as average, standard deviation, median, min and max values
basic_stats <- function(frame) {
  describe_frame <- data.frame()
  frame <- cbind(frame, group = rep(1,nrow(frame)))
  for (i in 1:(ncol(frame) - 1)) {
    describe <- describeBy(frame[,i], frame$group)
    describe <- t(data.frame(unlist(describe[[1]])))
    describe <- round(describe[,c(3,4,5,8,9)],2)
    describe <- cbind(variable = colnames(frame)[i],t(describe))
    
    describe_frame <- rbind(describe_frame,describe)
  }
  return(describe_frame)
}

#Select statistics such as mean and median
basic_stats_md <- function(frame) {
  describe_frame <- data.frame()
  frame <- cbind(frame, group = rep(1,nrow(frame)))
  for (i in 1:(ncol(frame) - 1)) {
    describe <- describeBy(frame[,i], frame$group)
    describe <- t(data.frame(unlist(describe[[1]])))
    describe <- round(describe[,c(3,4,5)],2)
    describe <- cbind(variable = colnames(frame)[i],t(describe))
    
    describe_frame <- rbind(describe_frame,describe)
  }
  return(describe_frame)
}

#Select optimal parameters
hyperTuning <- function(data, target, method, params, resample_method = "CV", iters = 5L, predict_type = "prob", measures) {
  #Optimisation algorithm
  ctrl = makeTuneControlGrid()
  
  #Cross validation
  desc = makeResampleDesc(resample_method, iters = iters)
  
  #Create a classification task
  task <- makeClassifTask(data=data, target=target)
  
  #Create model
  model <- makeLearner(paste0('classif.',method), predict.type=predict_type)
  
  #Tuning
  tuneparams <- tuneParams(learner=model, 
                           resampling=desc, 
                           measures=measures, 
                           par.set=params, 
                           control=ctrl, 
                           task=task, 
                           show.info = TRUE)
  
  #Optimal parameters
  optimal_params <- setHyperPars(model, par.vals = tuneparams$x)
  
  return(list(optimal_params,task))
}

####Webscrapping####

#Read html
url <- 'https://www.fifaindex.com/pl/teams/fifa18_278/?league=50&order=desc'
webpage <- read_html(url)

#Select overall's
rating_data_html <- html_nodes(webpage,'td:nth-child(7) .r3')
name_data_html <- html_nodes(webpage, 'td+ td .link-team')

#Convert html to text
rating_data <- html_text(rank_data_html)
rating_data <- as.numeric(rating_data)
rating_data <- rating_data[seq(4,48, 4)]

#Correct the team names
name_data <- html_text(name_data_html)
name_data <- gsub(' [A-z ]*', '' , name_data)
name_data[2] <- "Rangers"
name_data[8] <- "St Johnstone"
name_data[7] <- "Ross County"

#Assign overall to list
overall <- list()
for( i in 1:length(name_data)){
  overall[[name_data[i]]] <- rating_data[i]
}


####Data preparation####
data <- read.csv("ScotPrem.csv", dec = ",", sep = ";")

#Create a vector with information if home teams shoot
homeScore <- data$FTHomeGoals

#Create vector with team names
teams <- as.character(unlist(unique(data$HomeTeam)))

##Calculation of how many points the teams had when they entered the matches

#Total number of corners in the match
corners <- data$HomeCorners + data$AwayCorners
#Create lists for the number of points and the information which side won the match
homeOrAway <- list()
points <- list()

#Calculation of the number of points of the teams entering the matches
for(i in 1:length(teams)) {
  teams_data <- data[which(data$HomeTeam == teams[i] | data$AwayTeam == teams[i]),]
  match_points <- 0
  sumcia <- 0
  homeOrAway_vec <- vector()
  for(j in 1:nrow(teams_data)) {
    results <- teams_data[j,c(3,4,7)]
    
    if(results$HomeTeam == teams[i] & results$FTresults == "H") {
      sumcia <- match_points[length(match_points)] + 3
      match_points[j] <- sumcia
      homeOrAway_vec[j] <- "H"
    }
    if(results$HomeTeam == teams[i] & results$FTresults == "D") {
      sumcia <- match_points[length(match_points)] + 1
      match_points[j] <- sumcia
      homeOrAway_vec[j] <- "H"
    }
    if(results$HomeTeam == teams[i] & results$FTresults == "A") {
      sumcia <- match_points[length(match_points)] + 0
      match_points[j] <- sumcia
      homeOrAway_vec[j] <- "H"
    }
    if(results$AwayTeam == teams[i] & results$FTresults == "A") {
      sumcia <- match_points[length(match_points)] + 3
      match_points[j] <- sumcia
      homeOrAway_vec[j] <- "A"
    }
    if(results$AwayTeam == teams[i] & results$FTresults == "D") {
      sumcia <- match_points[length(match_points)] + 1
      match_points[j] <- sumcia
      homeOrAway_vec[j] <- "A"
    }
    if(results$AwayTeam == teams[i] & results$FTresults == "H") {
      sumcia <- match_points[length(match_points)] + 0
      match_points[j] <- sumcia
      homeOrAway_vec[j] <- "A"
    }
  }
  points[[teams[i]]] <- c(0,match_points[-length(match_points)])
  homeOrAway[[teams[i]]] <- homeOrAway_vec
}

#Save match result as a binary variable
match_results <- as.character(unlist(data$FTresults))

match_results[match_results == "H"] <- 1
match_results[match_results != 1] <- 0

match_results <- as.numeric(match_results)

data$FTresults <- match_results

#Add overall to dataset
data <- data.frame(data, HomeOverall = rep(1,nrow(data)),AwayOverall = rep(2,nrow(data)))

for(i in 1:length(teams)) {
  data[which(data$HomeTeam==teams[i]),24] <- overall[[teams[i]]]
  data[which(data$AwayTeam==teams[i]),25] <- overall[[teams[i]]]
}

#Calculate averages for guest and host teams for variables such as number of shoots, corners, yellow cars, etc.
avgFTHGoals <- avgFunction(data, teams, 5, "home")
avgHomeLost <- avgFunction(data, teams, 6, "home")
avgFTAGoals <- avgFunction(data, teams, 6, "away")
avgAwayLost <- avgFunction(data, teams, 5, "away")
avgHTShots <- avgFunction(data, teams, 12, "home")
avgATShots <- avgFunction(data, teams, 13, "away")
avgHTTarget <- avgFunction(data, teams, 14, "home")
avgATTarget <- avgFunction(data, teams, 15, "away")
avgHomeFouls <- avgFunction(data, teams, 16, "home")
avgAwayFouls <- avgFunction(data, teams, 17, "away")
avgHTCorners <- avgFunction(data, teams, 18, "home")
avgATCorners <- avgFunction(data, teams, 19, "away")
avgHomeYellow <- avgFunction(data, teams, 20, "home")
avgAwayYellow <- avgFunction(data, teams, 21, "away")
avgHomeRed <- avgFunction(data, teams, 22, "home")
avgAwayRed <- avgFunction(data, teams, 23, "away")


#Assign new variables to dataset
for(i in 1:length(teams)){
  #Goals
  data[which(data$HomeTeam == teams[i]),5] <- avgFTHGoals[[teams[i]]]
  data[which(data$HomeTeam == teams[i]),8] <- avgHomeLost[[teams[i]]]
  data[which(data$AwayTeam == teams[i]),6] <- avgFTAGoals[[teams[i]]]
  data[which(data$AwayTeam == teams[i]),9] <- avgAwayLost[[teams[i]]]
  
  #Shots
  data[which(data$HomeTeam == teams[i]),12] <- avgHTShots[[teams[i]]]
  data[which(data$AwayTeam == teams[i]),13] <- avgATShots[[teams[i]]]
  data[which(data$HomeTeam == teams[i]),14] <- avgHTTarget[[teams[i]]]
  data[which(data$AwayTeam == teams[i]),15] <- avgATTarget[[teams[i]]]
  
  #Fouls
  data[which(data$HomeTeam == teams[i]),16] <- avgHomeFouls[[teams[i]]]
  data[which(data$AwayTeam == teams[i]),17] <- avgAwayFouls[[teams[i]]]
  
  #Corners
  data[which(data$HomeTeam == teams[i]),18] <- avgHTCorners[[teams[i]]]
  data[which(data$AwayTeam == teams[i]),19] <- avgATCorners[[teams[i]]]
  
  #Cards
  data[which(data$HomeTeam == teams[i]),20] <- avgHomeYellow[[teams[i]]]
  data[which(data$AwayTeam == teams[i]),21] <- avgAwayYellow[[teams[i]]]
  data[which(data$HomeTeam == teams[i]),22] <- avgHomeRed[[teams[i]]]
  data[which(data$AwayTeam == teams[i]),23] <- avgAwayRed[[teams[i]]]
}

#Add number of points that the teams had when they played a match to dataset
data <- cbind(data, HomePoint = rep(1, nrow(data)), AwayPoint = rep(2,nrow(data)))
for(i in 1:length(teams)) {
  data[which(data$HomeTeam==teams[i]),26] <- points[[teams[i]]][which(homeOrAway[[teams[i]]] == "H")]
  data[which(data$AwayTeam==teams[i]),27] <- points[[teams[i]]][which(homeOrAway[[teams[i]]] == "A")]
}

#Create data frame with all variables (due to a lack of points, the first two rounds were not taken into account)
data <- data[-c(1:12),]
data <- data[,c(3,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20,21,24,25,26,27)]
colnames(data) <- c("HomeTeam", "AwayTeam", "HomeGoals","AwayGoals", "Result", "HomeLost", "AwayLost","HomeShots","AwayShots",
                    "HomeTarget", "AwayTarget", "HomeFouls", "AwayFouls", "HomeCorners", "AwayCorners","HomeYellow", "AwayYellow",
                    "HomeOverall", "AwayOverall","HomePoints", "AwayPoints")

data <- data.frame(HomeTeam = data$HomeTeam, AwayTeam = data$AwayTeam, Result = data$Result,HomeScore = homeScore[-c(1:12)], AVGGoals = data$HomeGoals - data$AwayGoals, AVGLost = data$HomeLost - data$AwayLost,
                   AVGShots = data$HomeShots - data$AwayShots, AVGTarget = data$HomeTarget - data$AwayTarget,
                   AVGCorners = data$HomeCorners - data$AwayCorners, AVGFouls = data$HomeFouls - data$AwayFouls,
                   Overall = (data$HomeOverall/data$AwayOverall) - 1, PointsDiff = data$HomePoints-data$AwayPoints)


data[which(data$HomeScore >0 ),4] <- 1
data <- data[,-4]

kable(head(data), "html") %>% kable_styling("striped")

data$Corners <- corners[13:228]

####Data exploration####
data %>% 
  ggplot(aes(x=as.factor(Result), fill = as.factor(Result)))+
  geom_histogram(stat = "count")+
  labs(x = "Czy gospodarz wygra mecz")+
  geom_label(stat='count',aes(label=..count..)) +
  labs(fill = "Result")

#Basic statistics
statistics <- basic_stats(data[,-c(1,2,3)])
frame_func(statistics)

#Boxplots
df.m <- melt(data[,-c(1,2)], id.var = "Result")
df.m[,1] <- as.factor(df.m[,1])
p <- ggplot(data = df.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=Result))
p + facet_wrap( ~ variable, scales="free")

#Correlation coefficient
corrCoef <- cor(data[,-c(1,2)])

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corrCoef, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

####Training and test data####
set.seed(123)
split = sample.split(data$Result, SplitRatio = 0.75)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

X_train = train[,-c(1:3)]
y_train = train$Result
X_test = test[,-c(1:3)]
y_test = test$Result

y_train = factor(y_train, levels = c(0, 1))
y_test = factor(y_test, levels = c(0, 1))

#Scaling variables
X_train_std = scale(X_train)
X_test_std = scale(X_test)

#Combining datasets to makeClassifTask
training_set_std = data.frame(X_train_std, Result = y_train)
test_set_std = data.frame(X_test_std, Result = y_test)

training_set = data.frame(X_train, Result = y_train)
test_set = data.frame(X_test, Result = y_test)

#Compare training and test sets
set.seed(123)
plot1 <- train %>% 
  ggplot(aes(x=as.factor(Result), fill = as.factor(Result)))+
  geom_histogram(stat = "count")+
  labs(x = "Czy gospodarz wygra mecz - zbiór treningowy")+
  geom_label(stat='count',aes(label=..count..)) +
  theme(legend.title = element_blank())
plot2 <- test %>% 
  ggplot(aes(x=as.factor(Result), fill = as.factor(Result)))+
  geom_histogram(stat = "count")+
  labs(x = "Czy gospodarz wygra mecz - zbiór testowy")+
  geom_label(stat='count',aes(label=..count..)) +
  theme(legend.title = element_blank())

grid.arrange(plot1, plot2)

#Compare median and mean for variables in training and test sets
set.seed(123)
train_stat <- basic_stats_md(train[,-c(1,2)])
test_stat <- basic_stats_md(test[,-c(1,2)])

z <- cbind(train_stat,test_stat[,-1])
colnames(z) <- c("Variable", "Mean - train", "Sd - train", "Median - train", "Mean - test", "Sd - test", "Median - test")
frame_func(z)

####KNN####
#Looking for hyperparameters
#getParamSet("classif.kknn")

#The allocation of values that can be taken by parameters
knn_param <- makeParamSet(makeDiscreteParam("k", values=seq(3,6,1)),
                          makeDiscreteParam("distance", values=seq(1,3,1)),
                          makeDiscreteParam("kernel",
                                            values = list("rectangular", "triangular", "epanechnikov",
                                                          "biweight","triweight", "cos", "inv", "gaussian", "optimal"))
)

#Selection of measures based on which the optimum set of parameters will be selected
measures = list(tpr, mlr::auc,ppv, mmce, tnr, setAggregation(tpr, test.sd))

#Select optimal parameters
knn_optimal = hyperTuning(data = training_set_std, target = "Result", method = "kknn", params = knn_param,
                          resample_method = "CV", iters = 5L, predict_type = "prob", measures = measures)

#Create model
set.seed(123)
knn_train <- mlr::train(learner=knn_optimal[[1]], task=knn_optimal[[2]]) 
getLearnerModel(knn_train)

##Training set
#Prediction on training set
set.seed(123) 
knn_train_predict <- predict(knn_train, newdata = training_set_std)

#Conf matrix
conf_matrix = knn_train_predict %>% calculateROCMeasures() %>% .[1] %>% as.data.frame()
colnames(conf_matrix) = c("0", "1")

kable(conf_matrix, "html") %>% kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Przewidywanie" = 2, "Wyniki rzeczywiste" = 1))

#Measures results for the training set
performance(knn_train_predict, measures = list(tpr,mlr::auc,mmce,tnr, ppv)) %>% 
  as.data.frame(row.names = c("True Positive","Area Under Curve", "Mean Misclassification Error","True Negative","Precision")) %>%
  kable(digits = 2, format = 'html', col.names = "Result") %>%
  kable_styling(position = "center", full_width = F)

##Test set
#Prediction on test set
set.seed(123) 
knn_predict <- predict(knn_train, newdata = test_set_std)

#Conf matrix
conf_matrix = knn_predict %>% calculateROCMeasures() %>% .[1] %>% as.data.frame()
colnames(conf_matrix) = c("0", "1")

kable(conf_matrix, "html") %>% kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Przewidywanie" = 2, "Wyniki rzeczywiste" = 1))

#Measures results for the test set
performance(knn_predict, measures = list(tpr,mlr::auc,mmce,tnr, ppv)) %>% 
  as.data.frame(row.names = c("True Positive","Area Under Curve", "Mean Misclassification Error","True Negative","Precision")) %>%
  kable(digits = 2, format = 'html', col.names = "Result") %>%
  kable_styling(position = "center", full_width = F)

#How will the measures behave depending on the cut-off point
knn_threshold <- generateThreshVsPerfData(knn_predict, measures = list(tpr,ppv, mmce,tnr)) %>%
  plotThreshVsPerf() + geom_point()
knn_threshold

####Naive Bayes Classifier####
#Looking for hyperparameters
#getParamSet("classif.naiveBayes")

#The allocation of values that can be taken by parameters
bayes_param <- makeParamSet(makeDiscreteParam("laplace", values=seq(0,10,1))
)

#Selection of measures based on which the optimum set of parameters will be selected
measures = list(tpr, mlr::auc,ppv, mmce, tnr, setAggregation(tpr, test.sd))

#Select optimal parameters
bayes_optimal = hyperTuning(data = training_set_std, target = "Result", method = "naiveBayes", params = bayes_param,
                            resample_method = "CV", iters = 5L, predict_type = "prob", measures = measures)

#Create model
set.seed(123)
bayes_train <- mlr::train(learner=bayes_optimal[[1]], task=bayes_optimal[[2]]) 
getLearnerModel(bayes_train)

##Training set
#Prediction on training set
set.seed(123) 
bayes_train_predict <- predict(bayes_train, newdata = training_set_std)

#Conf matrix
conf_matrix = bayes_train_predict %>% calculateROCMeasures() %>% .[1] %>% as.data.frame()
colnames(conf_matrix) = c("0", "1")

kable(conf_matrix, "html") %>% kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Przewidywanie" = 2, "Wyniki rzeczywiste" = 1))

#Measures results for the training set
performance(bayes_train_predict, measures = list(tpr,mlr::auc,mmce,tnr, ppv)) %>% 
  as.data.frame(row.names = c("True Positive","Area Under Curve", "Mean Misclassification Error","True Negative","Precision"))%>%
  kable(digits = 2, format = 'html', col.names = "Result") %>%
  kable_styling(position = "center", full_width = F)

##Test set
#Prediction on test set
set.seed(123) 
bayes_predict <- predict(bayes_train, newdata = test_set_std)

#Conf matrix
conf_matrix = bayes_predict %>% calculateROCMeasures() %>% .[1] %>% as.data.frame()
colnames(conf_matrix) = c("0", "1")

kable(conf_matrix, "html") %>% kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Przewidywanie" = 2, "Wyniki rzeczywiste" = 1))

#Measures results for the test set
performance(bayes_predict, measures = list(tpr,mlr::auc,mmce,tnr, ppv)) %>% 
  as.data.frame(row.names = c("True Positive","Area Under Curve", "Mean Misclassification Error","True Negative","Precision"))%>%
  kable(digits = 2, format = 'html', col.names = "Result") %>%
  kable_styling(position = "center", full_width = F)

#How will the measures behave depending on the cut-off point
bayes_threshold <- generateThreshVsPerfData(bayes_predict, measures = list(tpr,ppv, mmce,tnr)) %>%
  plotThreshVsPerf() + geom_point()
bayes_threshold

####Decision Tree####
#Looking for hyperparameters
#getParamSet("classif.rpart")

#The allocation of values that can be taken by parameters
tree_param <- makeParamSet(makeDiscreteParam("minsplit", values=seq(5,10,1)),
                           makeDiscreteParam("minbucket",values=seq(round(5/3,0), round(10/3,0), 1)),
                           makeNumericParam("cp", lower = 0.001, upper = 0.05),
                           makeDiscreteParam("maxcompete", values=6),
                           makeDiscreteParam("usesurrogate", values=0),
                           makeDiscreteParam("maxdepth", values=c(10,20, 30))
)

#Selection of measures based on which the optimum set of parameters will be selected
measures = list(tpr, mlr::auc, ppv, mmce, tnr, setAggregation(tpr, test.sd))

#Select optimal parameters
tree_optimal = hyperTuning(data = training_set, target = "Result", method = "rpart", params = tree_param,
                           resample_method = "CV", iters = 5L, predict_type = "prob", measures = measures)

#Create model
set.seed(123)
tree_train <- mlr::train(learner=tree_optimal[[1]], task=tree_optimal[[2]]) 
getLearnerModel(tree_train)

#Plot
rpart.plot(tree_train$learner.model, roundint=FALSE, varlen=3, type = 3, clip.right.labs = FALSE, yesno = 2)

#Training set
#Prediction on training set
set.seed(123) 
tree_train_predict <- predict(tree_train, newdata = training_set)

#Conf matrix
conf_matrix = tree_train_predict %>% calculateROCMeasures() %>% .[1] %>% as.data.frame()
colnames(conf_matrix) = c("0", "1")

kable(conf_matrix, "html") %>% kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Przewidywanie" = 2, "Wyniki rzeczywiste" = 1))

#Measures results for the training set
performance(tree_train_predict, measures = list(tpr,mlr::auc,mmce,tnr,ppv)) %>% 
  as.data.frame(row.names = c("True Positive","Area Under Curve", "Mean Misclassification Error","True Negative", "Precision"))%>%
  kable(digits = 2, format = 'html', col.names = "Result") %>%
  kable_styling(position = "center", full_width = F)

##Test set
#Prediction on test set
set.seed(123) 
tree_predict <- predict(tree_train, newdata = test_set)

#Conf matrix
conf_matrix = tree_predict %>% calculateROCMeasures() %>% .[1] %>% as.data.frame()
colnames(conf_matrix) = c("0", "1")

kable(conf_matrix, "html") %>% kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Przewidywanie" = 2, "Wyniki rzeczywiste" = 1))

#Measures results for the test set
performance(tree_predict, measures = list(tpr,mlr::auc,mmce,tnr,ppv)) %>% 
  as.data.frame(row.names = c("True Positive","Area Under Curve", "Mean Misclassification Error","True Negative", "Precision"))%>%
  kable(digits = 2, format = 'html', col.names = "Result") %>%
  kable_styling(position = "center", full_width = F)

#How will the measures behave depending on the cut-off point
tree_threshold <- generateThreshVsPerfData(tree_predict, measures = list(tpr,ppv, mmce,tnr)) %>%
  plotThreshVsPerf() + geom_point()
tree_threshold

####Random Forest####
#Looking for hyperparameters
#getParamSet("classif.randomForest")

#The allocation of values that can be taken by parameters
random_param <- makeParamSet(makeDiscreteParam("ntree", values=seq(50,150,10)),
                             makeDiscreteParam("mtry", values=seq(1,3,1)),
                             makeDiscreteParam("nodesize", values=seq(1,5,1)),
                             makeLogicalParam("replace"),
                             makeLogicalParam("importance"),
                             makeLogicalParam("localImp"),
                             makeLogicalParam("proximity")
)

#Selection of measures based on which the optimum set of parameters will be selected
measures = list(tpr, mlr::auc,ppv, mmce, tnr, setAggregation(tpr, test.sd))

set.seed(123)
#Select optimal parameters
random_optimal = hyperTuning(data = training_set, target = "Result", method = "randomForest", params = random_param,
                             resample_method = "CV", iters = 5L, predict_type = "prob", measures = measures)

#Create model
set.seed(123)
random_train <- mlr::train(learner=random_optimal[[1]], task=random_optimal[[2]]) 
getLearnerModel(random_train)

##Training set
#Prediction on training set
set.seed(123) 
random_train_predict <- predict(random_train, newdata = training_set)

#Conf matrix
conf_matrix = random_train_predict %>% calculateROCMeasures() %>% .[1] %>% as.data.frame()
colnames(conf_matrix) = c("0", "1")

kable(conf_matrix, "html") %>% kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Przewidywanie" = 2, "Wyniki rzeczywiste" = 1))

#Measures results for the training set
performance(random_train_predict, measures = list(tpr,mlr::auc,mmce,tnr,ppv)) %>% 
  as.data.frame(row.names = c("True Positive","Area Under Curve", "Mean Misclassification Error","True Negative", "Precision"))%>%
  kable(digits = 2, format = 'html', col.names = "Result") %>% 
  kable_styling(position = "center", full_width = F)

##Test set
#Prediction on test set
set.seed(123) 
random_predict <- predict(random_train, newdata = test_set)

#Conf matrix
conf_matrix = random_predict %>% calculateROCMeasures() %>% .[1] %>% as.data.frame()
colnames(conf_matrix) = c("0", "1")

kable(conf_matrix, "html") %>% kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Przewidywanie" = 2, "Wyniki rzeczywiste" = 1))

#Measures results for the test set
performance(random_predict, measures = list(tpr,mlr::auc,mmce,tnr,ppv)) %>% 
  as.data.frame(row.names = c("True Positive","Area Under Curve", "Mean Misclassification Error","True Negative", "Precision"))%>%
  kable(digits = 2, format = 'html', col.names = "Result") %>% 
  kable_styling(position = "center", full_width = F)

#How will the measures behave depending on the cut-off point
random_threshold <- generateThreshVsPerfData(tree_predict, measures = list(tpr,ppv, mmce,tnr)) %>%
  plotThreshVsPerf() + geom_point()
random_threshold