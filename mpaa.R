set.seed(1)
Movies<- read.csv("Downloads/movies_sentiments_new.csv", header = T)
str(Movies)
Movies$year.x <- as.factor(Movies$year.x)
Movies$Twitter.Sentiment<- as.factor(Movies$Twitter.Sentiment)
hist(Movies$budget.x)
plot(Movies$rating.x~Movies$budget.x, xlab="rating", ylab="budget", main="box plot rating by budget")
# checking mising values 
fvalue <- (Movies$budget.x==0)
Movies$budget.x[fvalue] <- NA
#library(mice)
#mice_mod <- mice(Movies, method='rf')
summary(Movies)
# dropping collumns youtube view counts and budget as more the 25 percent data is missing and imputed value's can decrease varianc and give misleading result's
Movies$budget.x<- NULL
Movies$Youtube.ViewCount<- NULL
Movies$released.x<- NULL
summary(Movies)
str(Movies)
Movies$name <- as.numeric(Movies$name)
Movies$company.x <- as.numeric(Movies$company.x)
Movies$country.x <- as.numeric(Movies$country.x)
Movies$director.x <- as.numeric(Movies$director.x)
Movies$genre.x<- as.numeric(Movies$genre.x)
Movies$rating.x <- as.factor(Movies$rating.x)
Movies$star.x <- as.numeric(Movies$star.x)
Movies$writer.x <- as.numeric(Movies$writer.x)
Movies$year.x <- as.numeric(Movies$year.x)
# convert into factor to above dataset
Movies$country.x <- as.factor(Movies$country.x)
Movies$genre.x<- as.factor(Movies$genre.x)
Movies$rating.x <- as.factor(Movies$rating.x)
Movies$year.x <- as.factor(Movies$year.x)
#Movies$writer.x <- as.factor(Movies$writer.x)
#Movies$star.x <- as.factor(Movies$star.x)
#Movies$director.x <- as.factor(Movies$director.x)
#Movies$company.x <- as.factor(Movies$company.x)
#Movies$name <- as.factor(Movies$name)
Movies <- Movies[Movies$rating.x!="NOT RATED",]
Movies <- Movies[Movies$rating.x!="Not specified",]
Movies <- Movies[Movies$rating.x!="UNRATED",]
Movies$rating.x <- as.factor(as.character(Movies$rating.x))
# Write CSV in R
#write.csv(Movies, file = "MyMovies.csv")
library(caret)
index <- createDataPartition(Movies$rating.x, p = .75, list = FALSE)
MoviesTrain<- Movies[index,]
MoviesTest <- Movies[-index,]


# RANDOM FOREST CLASSIFIER
library(randomForest)
rf <- randomForest(rating.x~., MoviesTrain)#mtry=6,nodesize=7)
colnames(MoviesTest)
rfpredict <- predict(rf,MoviesTest[,-7])

cm<-confusionMatrix(MoviesTest$rating.x,rfpredict)

AccuracyRF<- cm$overall
AccuracyRF['Accuracy']
#Accuracy 
#0.6857143

#install.packages("Metrics")
# 64.07 after takin some columns as factors and removing TV-14
#64.41 percent in random forest for label encoded data all data numeric
# 65.25 after removing TV-14 on label encoded data

#rf <- randomForest(rating.x ~ ., data=training, importance=TRUE, ntree=100)
# C5.0 CLASSIFER
library(C50)
cFifty <- C5.0(MoviesTrain$rating.x~., data=MoviesTrain)
#cFifty <- C5.0(MoviesTrain[,-which(colnames(MoviesTrain)=="rating.x")],MoviesTrain$rating.x)
c <- predict(cFifty,MoviesTest[,-7])
confusionMatrix(c,MoviesTest$rating.x)
#Accuracy : 0.6232 

# Naive base 
NaiveMovies <- Movies
str(NaiveMovies)

summary(NaiveMovies)
hist(Movies$runtime.x)
NaiveMovies$BinnedRuntime <- cut(NaiveMovies$runtime.x,
                              breaks = c(0,80,100,120,140,160,180,max(NaiveMovies$runtime.x)),
                              labels=c("V1", "V2HIGH", "V3MEDIUM","V4LOW","V5","V6","V7"))
NaiveMovies$binnedscore.x <- cut(NaiveMovies$score.x,
                               breaks = c(2,4,6,max(NaiveMovies$score.x)),
                               labels=c("S1", "S2MEDIUM", "S3HIGH"))


hist(NaiveMovies$YoutubeDislike)
#AS HISTOGRAM OF GROSS.X,YOUTUBE DESLIKE,AND YOUTUBE LIKE'S HAVE NO CATOGORICAL PROPERTY THEY ARE REMOVED.
NaiveMovies$votes.x<- NULL
NaiveMovies$YoutubeDislike<-NULL
NaiveMovies$YoutubeLikes<- NULL
NaiveMovies$score.x<- NULL
NaiveMovies$gross.x <- NULL
NaiveMovies$runtime.x<- NULL
############ Naive baise implementation
library(e1071)
index2 <- sample(1:dim(NaiveMovies)[1], dim(NaiveMovies)[1] * .75, replace=FALSE)
NaiveMoviesTrain <- NaiveMovies[index2,]
NaiveMoviesTest<- NaiveMovies[-index2,]
nb <- naiveBayes(rating.x~.,NaiveMoviesTrain)
colnames(NaiveMoviesTest)
nbPredict <- predict(nb, newdata = NaiveMoviesTest[,-6])
confusionMatrix(nbPredict,NaiveMoviesTest$rating.x)

# Accuracy : 0.595 with converting only runtime and score x in binned form
#Accuracy : 0.3233 without converting the all columns in labels
############### Naive Bayes 2 all factor's ######################
NaiveMovies <- Movies
NaiveMovies$votes.x<- as.factor(NaiveMovies$votes.x)
NaiveMovies$YoutubeDislike<-as.factor(NaiveMovies$YoutubeDislike)
NaiveMovies$YoutubeLikes<- as.factor(NaiveMovies$YoutubeLikes)
NaiveMovies$score.x<- as.factor(NaiveMovies$score.x)
NaiveMovies$gross.x <- as.factor(NaiveMovies$gross.x)
NaiveMovies$runtime.x<- as.factor(NaiveMovies$runtime.x)

library(e1071)
index2 <- sample(1:dim(NaiveMovies)[1], dim(NaiveMovies)[1] * .75, replace=FALSE)
NaiveMoviesTrain <- NaiveMovies[index2,]
NaiveMoviesTest<- NaiveMovies[-index2,]
nb <- naiveBayes(rating.x~.,NaiveMoviesTrain)
colnames(NaiveMoviesTest)
nbPredict <- predict(nb, newdata = NaiveMoviesTest[,-7])
confusionMatrix(nbPredict,NaiveMoviesTest$rating.x)
#Accuracy : 0.5784
###############################################################################

# Decision tree

library(rpart)
tree <- rpart(rating.x ~.,MoviesTrain, method="class")
tree.pred<- predict(tree,newdata = MoviesTest[,-which(colnames(MoviesTest )=="rating.x")])
###################### C5.0 with K-cross Validation ##################
tuneParams <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final')
c50Tree <- train(rating.x~.,data=MoviesTrain, method="C5.0", trControl=tuneParams, tuneLength=3)
c50.pred <- predict(c50Tree, newdata = MoviesTest[,-which(colnames(MoviesTest )=="rating.x")])
confusionMatrix(c50.pred,MoviesTest$rating.x)
# Accuracy : 0.6536  by c50 tree after 10 fold on original data with numeric ans some columns as factor

###################### Random Forest K-folds####################
rfTree <- train(rating.x~.,data=MoviesTrain, method="rf", trControl=tuneParams, tuneLength=3)
rfTree.pred <- predict(rfTree, newdata = MoviesTest[,-which(colnames(MoviesTest )=="rating.x")])
confusionMatrix(rfTree.pred,MoviesTest$rating.x)

#Accuracy : 0.6357  by random forest tree after 10 fold 




#################### TUNED RANDOM FOREST ###################
# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(MoviesTrain) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(MoviesTrain) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = rating.x ~ ., 
                        data = MoviesTrain,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])
#mtry nodesize sampsize
#17    6        3   1351.2
#mtry nodesize sampsize
#21    4        5   1351.2
#mtry nodesize sampsize
#2    6        3   1250.9


##################### Random Forest Reduced OOB Error #################################
rf <- randomForest(rating.x~., MoviesTrain,mtry=6,nodesize=3,sampsize=1351)#,mtry=6,nodesize=3 date 17/04/19 2

colnames(MoviesTest)
rfpredict <- predict(rf,MoviesTest[,-7])

cm<-confusionMatrix(MoviesTest$rating.x,rfpredict)
AccuracyRF<- cm$overall
AccuracyRF['Accuracy']
#Accuracy 
#0.6892857
############################### ANN ########################################3
library(h2o)
h2o.init()
index <- createDataPartition(Movies$rating.x, p = .75, list = FALSE)
MoviesTrain2<- Movies[index,]
MoviesTest2 <- Movies[-index,]
h2o.init()
h2o.Movies2Train <- as.h2o(MoviesTrain2)
h2o.Movies2Test <- as.h2o(MoviesTest2)

res.dl <- h2o.deeplearning(x =c(1:6,8:16),
                           y = 7,
                           training_frame =h2o.Movies2Train ,
                           activation = "Tanh",
                           hidden=c(1600,800,400,200,100,50,25,5),
                           epochs = 5)

pred.dl<-h2o.predict(object=res.dl, newdata=h2o.Movies2Test[,-7])
summary(pred.dl)

predicted <- factor(as.vector(pred.dl[,1]),
                    levels = levels(MoviesTest2$rating.x),
                    labels = levels(MoviesTest2$rating.x))
confusionMatrix(predicted, MoviesTest2[, 7])

######## ANN DEEPLEARNING

validation <- createDataPartition(Movies[-index,7], p = 1/3, list = FALSE)
h2o.validation <- h2o.Movies2Test[validation, ]
hidden_opt <- list(rep(150,3), c(200, 150, 75, 50), 100)
l1_opt <- c(1e-5,1e-7)
activation <- c("Tanh", "RectifierWithDropout", "Maxout","TanhWithDropout","MaxoutWithDropout","Rectifier")
hyper_params <- list(hidden = hidden_opt,
                     
                     l1 = l1_opt, activation = activation)
h2o.grid("deeplearning",
         hyper_params = hyper_params,
         x = c(1:6,8:16),
         y = 7,
         grid_id = "ADM_ann_grid7",
         training_frame = h2o.Movies2Train,
         validation_frame = h2o.validation)


grid <- h2o.getGrid(grid_id = "ADM_ann_grid7",
                    sort_by = "accuracy",
                    decreasing = TRUE)
# Grab the top model, chosen by accuracy
best <- h2o.getModel(grid@model_ids[[1]])
print(best)

pred.dl<-h2o.predict(object=best, newdata=h2o.Movies2Test[-validation,-7])


predicted <- factor(as.vector(pred.dl[,1]),
                    levels = levels(Movies$rating.x),
                    labels = levels(Movies$rating.x))
testingLabels <- Movies[-index, 7]
testingLabels <- testingLabels[-validation]
ANNresult<-confusionMatrix(predicted, testingLabels)
# Write CSV in R
write.csv(ANNresult$overall , file = "/Users/abhishekjain/Desktop/ADM EXAM/ANNresult.csv")
#Accuracy : 0.7124 20/04/19

############################## SVM ##############################
library(e1071) 
svmfit=svm(rating.x~., data=MoviesTrain, kernel="linear", cost=10,
             scale=FALSE)
svmpred= predict(svmfit,MoviesTest[,-7])
confusionMatrix(svmpred, MoviesTest[,7])
#Accuracy : 0.5232 17/04/19 linear kernal
library(kernlab)

#we're probably going to build lots of models, so let's make a function to save time
svmPerformance <- function(svm, testing, trueValues) {
  p <- predict(svm, newdata=testing, type = "response")
  accuracy <- 1-mean(p != trueValues)
  return(accuracy)
}

svm.model <- ksvm(rating.x ~ ., data = MoviesTrain)
svmPerformance(svm.model, MoviesTest[,-7], MoviesTest$rating.x)
# [1] 0.6142857 17/04/19
### TUNE SVM
library(e1071)
tuned.svm = tune(svm, rating.x ~ ., data = MoviesTrain, ranges = list(gamma = 2^(-4:4), cost = 2^(-4:4), kernel = c("linear", "polynomial", "radial", "sigmoid"), degree=c(1:4)))
svm.model <- tuned.svm$best.model
#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost:  4 
#gamma:  0.0625 
#Number of Support Vectors:  1347 18/04/19 morning 6 am 
svmPerformance(svm.model, MoviesTest[,-7], MoviesTest$rating.x)
#[1] 0.6214286 18/04/19 morning 6 am 
###########################################################################################
################################## CCCCCCCCCCCCCCCCCCCCCCCCCC #############################
################################# creating new dataset column ############################
# Create a new column with the proper string encodings "Family Movie"
library(caret)
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)
Movies2 <-  Movies %>%
  mutate(FamilyMovie = 
           case_when(rating.x == "G" ~ "yes",
                     rating.x ==  "NC-17" ~ "no",
                     rating.x == "PG" ~ "yes",
                     rating.x == "PG-13" ~ "yes",
                     rating.x ==  "R" ~ "no"))
Movies2$rating.x <- NULL
Movies2$FamilyMovie <- as.factor(Movies2$FamilyMovie)
# Random Forest
index <- createDataPartition(Movies2$FamilyMovie, p = .75, list = FALSE)
MoviesTrain2<- Movies2[index,]
MoviesTest2 <- Movies2[-index,]

# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(MoviesTrain2) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(MoviesTrain2) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()
library(randomForest) 

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  # Train a Random Forest model
  model <- randomForest(formula = FamilyMovie~., 
                        data = MoviesTrain2,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])
#mtry nodesize sampsize
#4   10        3   1181.6  17/04/19
#mtry nodesize sampsize
#6    4        5   1181.6
# Random Forest on 3
rf <- randomForest(FamilyMovie~., MoviesTrain2,mtry=4,nodesize=5,sampsize=1181)#,mtry=6,nodesize=7
rf <- randomForest(FamilyMovie~., MoviesTrain2,mtry=10,nodesize=3,sampsize=1181)#,mtry=6,nodesize=7
colnames(MoviesTest2)
rfpredict <- predict(rf,MoviesTest2[,-16])

cm<-confusionMatrix(MoviesTest2$FamilyMovie,rfpredict)
AccuracyRF<- cm$overall
AccuracyRF['Accuracy']
#Accuracy  17/04/19
#0.7629234
#Accuracy 
#0.7700535
# K-cross
library(caret)
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
tunegrid <- expand.grid(mtry=10,nodesize=3,sampsize=1181)# 17/04/19

tunegrid <- expand.grid(mtry=4,nodesize=5,sampsize=1181)
# Fit Naive Bayes Model
model <- train(FamilyMovie~., data=MoviesTrain2, trControl=train_control, method="rf",tunegrid=tunegrid)
# Summarise Results
print(model)
rfpredict <- predict(model,MoviesTest2[,-16])

cm<-confusionMatrix(MoviesTest2$FamilyMovie,rfpredict)
cm
#Accuracy : 0.7683 17/04/19
# 75.59 balanced accuracy
# GBM on Movies3
library(gbm)
library(cvAUC)
tuneParams <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final')

glmboost <- train(FamilyMovie~.,data=MoviesTrain2, method="glmboost", trControl=tuneParams, tuneLength=5)
glmboost.pred <- predict(glmboost, newdata = MoviesTest2[,-16])
confusionMatrix(glmboost.pred,MoviesTest2$FamilyMovie)
#  Accuracy : 0.7112  17/04/19
# AdaBoost classifier on Movie 2
#library(JOUSBoost)
library(fastAdaboost)
adaboostmodel<- adaboost(FamilyMovie~.,MoviesTrain2,1000)
adapredict<- predict(adaboostmodel,newdata= MoviesTest2[,-16])
(confusionMatrix(adapredict$class,MoviesTest2$FamilyMovie))
# Accuracy : 0.7594  17/04/19
#Accuracy : 0.7451 
# Feature selection on Movie3 By Infogain
library(CORElearn)
library(RWeka)
library(FSelector)

IG.CORElearn <- attrEval(FamilyMovie ~ ., data=Movies2,  estimator = "InfGain")
IG.RWeka     <- InfoGainAttributeEval(FamilyMovie ~ ., data=Movies2)
IG.FSelector <- information.gain(FamilyMovie ~ ., data=Movies2,unit="log2")
order(IG.CORElearn)#MoviesTrain3[,c(6,5,3,2,14,10,13,16)]
order(IG.RWeka)
order(IG.FSelector)
# classification by RandomForest after feature selection
# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(MoviesTrain2[,c(6,5,3,2,14,10,13,16)]) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(MoviesTrain2[,c(6,5,3,2,14,10,13,16)]) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()
library(randomForest) 

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  # Train a Random Forest model
  model <- randomForest(formula = FamilyMovie~., 
                        data = MoviesTrain2[,c(6,5,3,2,14,10,13,16)],
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])
#mtry nodesize sampsize 17/04/19
#3    4        5   1181.6
# Random Forest on 3
rf <- randomForest(FamilyMovie~., MoviesTrain2[,c(6,5,3,2,14,10,13,16)],mtry=4,nodesize=5,sampsize=1181)#,mtry=6,nodesize=7
rfpredict <- predict(rf,MoviesTest2[,-16])

cm<-confusionMatrix(MoviesTest2$FamilyMovie,rfpredict)
AccuracyRF<- cm$overall
AccuracyRF['Accuracy']
#75 percent
#ANN
h2o.init()
h2o.FamilyTrain<- as.h2o(Movies2[index,])
h2o.FamilyTest <- as.h2o(Movies2[-index,])
validation <- createDataPartition(Movies2[-index,16], p = 1/3, list = FALSE)
h2o.validation2 <- h2o.FamilyTest[validation, ]
hidden_opt <- list(rep(150,5), c(1600,800,400,200, 150, 75, 50), 100)
l1_opt <- c(1e-5,1e-7)
activation <- c("Tanh", "RectifierWithDropout", "Maxout")
hyper_params <- list(hidden = hidden_opt,
                     
                     l1 = l1_opt, activation = activation)
h2o.grid("deeplearning",
         hyper_params = hyper_params,
         x = c(1:15),
         y = 16,
         grid_id = "ADM_ann_grid3",
         training_frame = h2o.FamilyTrain,
         validation_frame = h2o.validation2)
grid2 <- h2o.getGrid(grid_id = "ADM_ann_grid3",
                    sort_by = "accuracy",
                    decreasing = TRUE)
# Grab the top model, chosen by accuracy
best2 <- h2o.getModel(grid2@model_ids[[1]])
print(best2)

pred.dl<-h2o.predict(object=best2, newdata=h2o.FamilyTest[-validation,-16])


predicted <- factor(as.vector(pred.dl[,1]),
                    levels = levels(Movies2$FamilyMovie),
                    labels = levels(Movies2$FamilyMovie))
testingLabels <- Movies2[-index, 16]
testingLabels <- testingLabels[-validation]
confusionMatrix(predicted, testingLabels)
