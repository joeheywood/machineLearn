require(caret)
# first step load the data
getData <- function(){
    if(file.exists('trimmed.rds')){
        return(readRDS('trimmed.rds'))
    } else {
        dt <- read.csv('pml-training.csv')
        # now get rid of all the variables we don't need
        blanks <- grepl(
            '^(stddev|skewness|avg|kurtosis|min|max|var|amplitude|total)', j)
        trimmedVars <- j[!blanks]
        # more metadata variables at beginning not really needed.
        trimmedVars <- trimmedVars[-(1:7)]
        dt <- dt[,trimmedVars]
        # save for future use.
        saveRDS(dt, file='trimmed.rds')
        return(dt)
    }
}

removeBlanks <- function(dt){
    blanks <- grepl(
        '^(stddev|skewness|avg|kurtosis|min|max|var|amplitude|total)', 
        colnames(dt))
    trimmedVars <- colnames(dt)[!blanks]
    # more metadata variables at beginning not really needed.
    trimmedVars <- trimmedVars[-(1:7)]
    dt <- dt[,trimmedVars] 
}

dt <- getData()
set.seed(42)
inTrain <- createDataPartition(dt$classe, p=0.6, list=FALSE)
training <- dt[inTrain,]
testing <- dt[-inTrain,]
modFit <- train(classe~., data=training, trControl=trControl, method='rf',
                prox=TRUE)
print(modFit)

pred <- predict(modFit, testing)
testing$predRight <- testing$classe == pred
table(pred, testing$classe)
table(testing$predRight)


#modFit <- train(classe~., data=training[1:500], method='rf', prox=TRUE)



