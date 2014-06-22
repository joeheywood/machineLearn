Machine Learning Assignment
========================================================

During this project, I came up with a quite reliable prediction model. The data
used was from the Human Activity Recognition project. This data includes 
variables from accelerometers on the belt, forearm, arm, and dumbell of 6 
participants, which I used to predict one of five activities.

Data Cleaning
-------------

The data included a lot of null values, that (judging by their name) should have
been derived variables, but ended up being `NA` values. 

I noticed that these values tended to have uniform names that made theme quite
straightforward to remove using regular expressions. I also removed the more 
meta-type data: the name of the participant, etc. For some models, I imagine 
that these would be of some use. But for here, I thought it would hinder, 
rather than help, separating the signal from the noise.


```r
require(caret)
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
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
dt <- getData()
```

Creating the training/testing sets
----------------------------------

I've used the `caret` package's `createDataPartition` function to separate
two thirds of the data, which will act as the training set, from the remaining
40%, on which we will test the model.


```r
set.seed(42)
inTrain <- createDataPartition(dt$classe, p=0.6, list=FALSE)
training <- dt[inTrain,]
testing <- dt[-inTrain,]
```

Creating the model
------------------

I then created a model (see below). I considered using three different models:

*   Rpart
*   Random Forest
*   Naive Bayes
*   GBM (boosting with trees)

I looked at each one and found that the random forest was the best model. This 
model is best suited to the data set. The other kinds of models only predicted
around 78% of rows accurately. As we can see below, the accuracy of this model
is just over 98% with standard deviation of less than 1%. I also avoided using
principal components analysis, since I assume that each variable should play 
a role, so I prefer to keep them in the model. I used the `trContrl` attribute
to speed up creating the model.


```r
trControl = trainControl(method = "cv", number = 2)
modFit <- train(classe~., data=training, trControl=trControl, method='rf')
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
modFit
```

```
## Random Forest 
## 
## 11776 samples
##    48 predictors
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (2 fold) 
## 
## Summary of sample sizes: 5887, 5889 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##   2     1         1      1e-04        2e-04   
##   20    1         1      0.003        0.004   
##   50    1         1      0.004        0.005   
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 25.
```

Testing on the model
--------------------

Given that there is a bit of a tendancy to overfit in these kinds of models, it
is also worth cross-validating on the part of the training set that we set aside
earlier.


```r
pred <- predict(modFit, testing)
testing$predRight <- testing$classe == pred
table(pred, testing$classe)
```

```
##     
## pred    A    B    C    D    E
##    A 2230    9    0    0    0
##    B    2 1508    6    0    3
##    C    0    1 1357   19    2
##    D    0    0    5 1266   10
##    E    0    0    0    1 1427
```

```r
table(testing$predRight)
```

```
## 
## FALSE  TRUE 
##    58  7788
```

```r
7788 / (7788 + 58)
```

```
## [1] 0.9926
```

As we can see, the model performs a little better than expected. Only 58 cases
were not correct out of 7,846. Unless there is something particularl about the
training in this data set, we would expect the model to predict accurately in 
the testing set.

Included in this repo is the model based on the entire training set. The 
accuracy for this model was just 99%. So I expect to get all, or maybe 19 of the
20 test cases correctly.
