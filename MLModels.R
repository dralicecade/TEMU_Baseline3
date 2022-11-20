library(tidyverse)   # data processing and visualisation
library(caret)       # the ML kits
library(MLeval)      # for comparing models
library(GGally)      # for cross correlations
library(cowplot)     # plotgrid

## Ingest data ----
load("AllTestsMeans.Rda")
df <- AllTestsMeans
retrainModels <- T

# Plotting cosmesis
ordCols      <- c("#173F5F", "#3CAEA3", "#F6D55C")
CondOrdered  <- c( "Control"  ,  "mTBI"   ,  "PPCS")

# Tidy data
df$OC        <- factor(df$Group, labels = CondOrdered, ordered = T)

# https://topepo.github.io/caret/train-models-by-tag.html#two-class-only 
#df$OC[df$OC == "PPCS"] <- "mTBI" #  force to be mTBI / Control
#df$OC <- droplevels(df$OC) # note some vis elements assume three factors
#CondOrdered <- CondOrdered[-3]

dfIO         <- df[c(-1,-2)]
ss           <- 1  # force random seed - ensures all ML models get the same Training/Testing groups while hypertuning
set.seed(ss)

modelsToTest <- c("svmPoly", "knn", "lda", "loclda", "sda", "treebag",  "ordinalNet") #"rf",
## Some different machine learning techniques -----
if (retrainModels){

trains <- dfIO$OC %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data <- dfIO[trains,]
test.data <- dfIO[-trains,]

# set common parameters for all ML models
trc   <- trainControl("cv", number = 10, savePredictions = T, classProbs=T)
metr  <- "Accuracy"
train.data.pp <- preProcess(train.data, method='knnImpute') # 
predict(train.data.pp, newdata = train.data)

trainModel <- function(x){ set.seed(ss)
  train(OC ~., data = train.data, method = x, trControl = trc,  metric = metr) }

ModStore <- list()
for (i in 1:length(modelsToTest)) {
  tictoc::tic()
  print(paste("Training", modelsToTest[i]))
  ModStore[[i]] <- trainModel(modelsToTest[i])
  tictoc::toc()
}
save(list = ls(all.names = T), file = "MLmodelStore.RData")
} else {load("MLmodelStore.RData")}

## Summarize accuracy of ML models -----
results <- resamples(ModStore, modelNames = modelsToTest)
modComDF <- summary(results)
dotplot(results)
modComDF <- (results$values)
accCols <- str_detect(names(modComDF), "~Accuracy")
modComDF <- modComDF[accCols]
names(modComDF) <- str_remove(names(modComDF), "~Accuracy")

# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
modComDFsum <- data.frame(Model = names(modComDF), Accuracy = colMeans(modComDF), sd = sapply(modComDF, sd))
accOrd<-order(modComDFsum$Accuracy, decreasing = T)
modComDFsum$Model <- factor(modComDFsum$Model, levels = modComDFsum$Model[accOrd])
ggplot(modComDFsum, aes(Model, Accuracy, fill = Model)) + geom_bar(stat = "identity", show.legend = F) +
  geom_errorbar(aes(ymin = Accuracy-(sd /sqrt(nrow(modComDF)))*1.96, ymax = Accuracy+(sd /sqrt(nrow(modComDF)))*1.96), width = 0.5) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 300))

# Then get top model and see ROC performance
modInd <- which.max(modComDFsum$Accuracy)
Pred <- predict(ModStore[[modInd]], newdata = test.data)
cmML<-confusionMatrix(Pred, test.data$OC[c(1:length(Pred))])
res <- evalm(ModStore[[modInd]], gnames = modelsToTest[[modInd]], silent = TRUE, cols = "black")

# check variable importance
varWei <- varImp(ModStore[[modInd]])
#str(varImpSor)

varImpOrd <- order(rowMeans(varWei$importance), decreasing = T)
varImpSor <- sort(rowMeans(varWei$importance), decreasing = T)
#varImpSor <- varImpSor[-c(1,7)]
varDF <- data.frame(dfIO[names(varImpSor)], OC=dfIO$OC)
varImpDF <- data.frame(Variable = names(varImpSor), Importance = varImpSor)
varImpDF$Variable <- factor(varImpDF$Variable, levels = names(varImpSor))
ggplot(varImpDF, aes(Variable, Importance, fill = Variable)) + geom_col(show.legend = F)+
  theme_minimal() + theme(axis.text.x = element_text(angle = 300)) + coord_flip()

# Can visualise differences in class performance for top n variables with
ggpairs(varDF[1:4], aes(color = varDF$OC, fill = varDF$OC, alpha = 0.2), axisLabels = "none", switch = "y") +
  scale_fill_manual(values = ordCols) + scale_color_manual(values = ordCols)

rocDF <- res$roc$data

# plot pretty confusion matrix and accuracy
mlAcc <- data.frame(Condition = CondOrdered, Accuracy = cmML$byClass[,11])# has to be [,11] for three classes
mlAcc$Condition <- factor(mlAcc$Condition, ordered=T, levels = CondOrdered)

p1 <- cm.ML <- ggplot(as.data.frame(cmML$table), aes(Reference, Prediction, colour = Reference, size = Freq)) +
  geom_abline(slope = 1, intercept = 0, size = 2, colour = "grey") + geom_point(show.legend = F)  +
  theme_minimal() + scale_size_area(max_size = 20) + scale_color_manual(values = ordCols) + coord_fixed()

p2 <- acc.ML <- ggplot(mlAcc, aes(Condition, Accuracy, fill = Condition)) + geom_col(show.legend = F) +
  scale_fill_manual(values = ordCols) + theme_minimal() 
plot_grid(p1,p2)








## Time for some deep learning -----
library(keras)
#load("MLmodelStore.RData")

## need to create plain matrix [nparticipants, variables], scaled and normalised. Add class to end, 0 based.
## Tune model layers.

dfUnscaled <- cbind((dfIO[,1:31]), as.numeric(dfIO$OC)-1) 
dfScaled <- cbind(scale(dfIO[,1:31]), as.numeric(dfIO$OC)-1) 
dfScaled2 <- cbind(keras::normalize(dfIO[,1:31]), as.numeric(dfIO$OC)-1) # zero index class

#dfScaled <- keras::normalize(dmR) 
#hist(rowSums(dmR, na.rm = T)) # note not very balanced ## Fixed after scale above

#dmR      <- dfScaled2[,1:31]
#dmR      <- dfUnscaled[,1:31]
dmR      <- dfScaled[,1:31]
colnames(dmR) <- NULL
rownames(dmR) <- NULL
dmR <- data.matrix(dmR)

set.seed(1)
training_id  <- sample.int(nrow(dmR), size = nrow(dmR)*0.8)
training     <- dmR[training_id,1:31]
test         <- dmR[-training_id,1:31]
trainingtarget  <- dfScaled[training_id,32]
testtarget   <- dfScaled[-training_id,32]
trainLables <- to_categorical(trainingtarget)
testLables <-  to_categorical(testtarget)

# the keras_model_sequential consists of a linear stack of layers (in some sequential linear order)
kMod <- keras_model_sequential() %>%
  layer_dense(units=40, activation = 'relu', input_shape = 31) %>%     # this is for independent variables
  layer_dense(units=400, activation = 'relu') %>%     # this is for independent variables
  layer_dense(units=3, activation = 'softmax')    # map to classes

kMod %>% keras::compile(loss='categorical_crossentropy',
                        optimizer='adam',
                        metrics='accuracy')
#summary(kMod)
history <- kMod%>%
  fit(training, # input
      trainLables,
      epoch=8,
      batch=64,
      validation_split = 0.2)

## Evaluate the DL model -----
kMod%>%
  keras::evaluate(test,testLables)

pred<-kMod%>%
  predict(test) %>% k_argmax()

table(Predicted = as.vector(pred), Actual=testtarget)
DLdf <- as.data.frame(table(Prediction = as.vector(pred), Reference=testtarget))
levels(DLdf$Prediction) <- CondOrdered
levels(DLdf$Reference) <- CondOrdered

ggplot(DLdf, aes(Reference, Prediction, colour = Reference, size = Freq)) +
  geom_abline(slope = 1, intercept = 0, size = 2, colour = "grey") + geom_point(show.legend = F)  +
  theme_minimal() + scale_size_area(max_size = 20) + scale_color_manual(values = ordCols) + coord_fixed()
