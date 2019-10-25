

rm(list=ls())
# Load all the libraries
source('src/R_libraries.R')
source('src/correlation_functions.R')
source('src/Information_Gain.R')

dt.data <- fread('data/PruebaDSBigData_conjuntoParaHacerDescriptivo.csv')

indexes  <- sample(1:nrow(dt.data), size = nrow(dt.data)*.70)
dt.train <- dt.data[indexes, ][, -c('user_id')]
dt.test  <- dt.data[-indexes, ][, -c('user_id')]

dt.train.class <- dt.train[, .(se_hizo_premium_0_1)]
dt.test.class  <- dt.test[, .(se_hizo_premium_0_1)]

dt.train <- dt.train[, se_hizo_premium_0_1 := NULL]
dt.test  <- dt.test[, se_hizo_premium_0_1 := NULL]

cat("\n")
cat("[INFO] Number of instances in Train Data:      ", nrow(dt.train), 
    "Positives: ", nrow(dt.train.class[se_hizo_premium_0_1 == 1]), "Negatives: ", nrow(dt.train.class[se_hizo_premium_0_1 == 0]), "\n")


n_size   <- 10
max_nodes <- 10
cat("[INFO] Training model...\n")
model.name <- 'train.RF'
rf.train  <- randomForest(dt.train, y = as.factor(dt.train.class$se_hizo_premium_0_1), 
                          nodesize = n_size, #minimum size of terminal nodes, , cutoff  classwt
                          # cutoff works like a cost function
                          #cutoff = c(nrow(dt.train.class[se_hizo_premium_0_1 == '1',])/nrow(dt.train.class), nrow(dt.train.class[se_hizo_premium_0_1 == '0',])/nrow(dt.train.class)), 
                          #maxnodes =  max_nodes, #Maximum number of terminal nodes
                          #sampsize = 1, #Size(s) of sample to draw. 
                          method = "class", 
                          ntree = 5000, #number.trees,
                          #mtry = sqrt(ncol(dt.train)),      # number of var sampled
                          #replace = T,
                          do.trace = T #,
                          # na.action = na.omit
)

pred.rf.train <- predict(rf.train, dt.train, type = "prob")
pred <- pred.rf.train[,2]
label <- as.numeric(as.vector(as.matrix(dt.train.class$se_hizo_premium_0_1)))
dt.predicted <- as.data.table(cbind(label, pred)) 

# #MATRIZ DE CONFUSIÓN
source("src/matrizdeconfusion.R")
real <- as.vector(as.matrix(label))
g <- pred
predicted.test <- pred
result <- rep(0, length(g))
result[g >= 0.5] = 1

dt.results.training <- as.data.table(cbind(result, real))
names(dt.results.training) <- c('result', 'real')
save(dt.results.training, file = 'data/dt.results.training.RData')
cat(" [INFO] Confusion Matrix: \n")
mc(dt.results.training$result, dt.results.training$real, imprimir = 1)

dt.importance <- melt(as.matrix(rf.train$importance))
d <- ggplot(dt.importance, aes(y = value, x = reorder(Var1, value))) +  geom_bar(stat = "identity", color = 'blue') + coord_flip()
d <- d + ylab('MeanDecreaseGini') + xlab('Features') 
d <- d + theme(axis.title.y = element_text(size = rel(1.5)))
d <- d + theme(axis.text.y = element_text(size = rel(1.8)))
d <- d + theme(axis.text.x = element_text(size = rel(1.8)))
d <- d + theme(axis.title.x = element_text(size = rel(1.5)))
d <- d + theme(legend.text = element_text(size = 15))
d <- d + theme(legend.title = element_text(size = 15))
d <- d +  theme(
  panel.background = element_rect(fill="white") ,
  panel.grid.minor.y = element_line(size=3),
  panel.grid.major = element_line(colour = "lightgray"),
  plot.background = element_rect(fill="white")
)

d


#------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------

cat("\n")
cat("[INFO] Number of instances in Test Data:      ", nrow(dt.test), 
    "Positives: ", nrow(dt.test.class[se_hizo_premium_0_1 == '1']), "Negatives: ", nrow(dt.test.class[se_hizo_premium_0_1 == '0']), "\n")

cat("predict with type = prob\n")
pred.rf.test.1 <- predict(rf.train, dt.test, type = "prob")
pred.1 <- prediction(pred.rf.test.1[,2], dt.test.class$se_hizo_premium_0_1)

perf.1 <- performance(pred.1, "tpr", "fpr")
plot(perf.1, main = "Positive Instances ROC Curve")

perf.2 <- performance(pred.1, "tnr", "fnr")
plot(perf.2, main = "Negative Instances ROC Curve")
# 
pred  <- pred.rf.test.1[,2]
label <- dt.test.class$se_hizo_premium_0_1
dt.predicted <- as.data.table(cbind(label, pred))

#MATRIZ DE CONFUSIÓN
source("src/matrizdeconfusion.R")
real <- as.vector(as.matrix(label))
g <- pred
predicted.test <- pred
result <- rep(0, length(g))
test.threshold <- 0.5
result[g >= test.threshold] = 1
cat(" [INFO] Confusion Matrix: \n")

dt.results.test <- as.data.table(cbind(result, real))
names(dt.results.test) <- c('result', 'real')
save(dt.results.test, file = 'data/dt.results.test.RData')
cat(" [INFO] Confusion Matrix: \n")
mc(dt.results.test$result, dt.results.test$real, imprimir = 1)







