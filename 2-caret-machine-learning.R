# Apply caret machine learning methods to Human Activity Recognition Using Smartphones Dataset
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# efg, 2014-09-05

################################################################################
### Setup

setwd("C:/Users/efg/Desktop/UCI/")          ##### Modify as appropriate
sink("2-caret-machine-learning.txt", split=TRUE)

library(caret)   # See http://topepo.github.io/caret/index.html
set.seed(19937)

################################################################################
### Setup parallel processing

# Let's use 6 cores on an i7-4770K 3.50 GHz processor with 32 GB RAM

library(doParallel)
rCluster <- makePSOCKcluster(6)  # Use 6 cores
registerDoParallel(rCluster)

################################################################################
### Load Samsung data

Samsung <- read.csv("Samsung-Human-Activity.csv")
dim(Samsung)

# Extract training set
rawTrain  <- Samsung[Samsung$source == "train",-1:-2]
dim(rawTrain)

# Extract "final" test set only to be used once at the end of all modeling
finalTest <- Samsung[Samsung$source == "test", -1:-2]
dim(finalTest)

################################################################################
# Remove near-zero variance variables from rawTrain.
nzv <- nearZeroVar(rawTrain, saveMetrics=TRUE)
count.nzv <- sum(nzv$nzv)
count.nzv
if (count.nzv > 0)
{
  rawTrain  <- rawTrain[,  !nzv$nzv]
  finalTest <- finalTest[, !nzv$nzv]
}

################################################################################
# Remove variables with high correlation to others
HIGH.CORRELATION.CUTOFF <- 0.90
cor.matrix <- cor(rawTrain[,-1])
cor.high   <- findCorrelation(cor.matrix, HIGH.CORRELATION.CUTOFF)

high.cor.remove <- row.names(cor.matrix)[cor.high]
high.cor.remove
length(high.cor.remove)

rawTrain <- rawTrain[,   -cor.high]
finalTest <- finalTest[, -cor.high]

names(rawTrain)[-1]

################################################################################
# Partition raw training data into a training and validation set.

TRAIN.PERCENT <- 0.75
inTrainSetIndex <- createDataPartition(y=rawTrain$activity, p=TRAIN.PERCENT, list=FALSE)

training   <- rawTrain[ inTrainSetIndex,]
dim(training)

validation <- rawTrain[-inTrainSetIndex,]
dim(validation)

################################################################################
# Apply specified caret method (lda) with specified preprocessing

PREPROCESS <- c("center", "scale")
METHOD <- "lda"

time.1 <- Sys.time()
fit <- train(activity ~ ., data = training,
             preProcess=PREPROCESS, method=METHOD)
OutOfSample  <- predict(fit, newdata=validation)
confusion <- confusionMatrix(validation$activity, OutOfSample)
time.2 <- Sys.time()
round(time.2 - time.1, 2)
pdf("dotplot-lda.pdf")
dotPlot(varImp(fit), main="lda:  Dotplot of variable importance values")
dev.off()

sink()
################################################################################
# Use function to apply to a number of caret methods

# Use "sink" to save info to .txt file.
use.caret <- function(INFO, METHOD, PREPROCESS=NULL)
{
  sink(paste0("caret-", INFO, "-", METHOD, ".txt"), split=TRUE)
  print(INFO)
  print(METHOD)
  print(PREPROCESS)
  time.1 <- Sys.time()
  fit <- train(activity ~ ., data = training,
               preProcess=PREPROCESS, method=METHOD)
  OutOfSample  <- predict(fit, newdata=validation)
  confusion <- confusionMatrix(validation$activity, OutOfSample)
  print(confusion)
  time.2 <- Sys.time()
  print(round(time.2 - time.1, 2))
  print(varImp(fit))
  sink()
  invisible(fit)
}

fit.lda       <- use.caret("Linear-Discriminant-Analysis",        "lda", PREPROCESS=c("center", "scale"))
fit.rpart     <- use.caret("Classification-and-Regression-Trees", "rpart")
fit.treebag   <- use.caret("BaggedCART",                          "treebag")

fit.rf        <- use.caret("Random-Forest",                       "rf")
fit.svmPoly   <- use.caret("Support-Vector-Machine-Polynomial",   "svmPoly")
fit.gbm       <- use.caret("Stochastic-Gradient-Boosting",        "gbm")

fit.bagFDA    <- use.caret("Bagged-Flex-Discriminant-Analysis",   "bagFDA")

fit.nb        <- use.caret("Naive-Bayes",                         "nb")

