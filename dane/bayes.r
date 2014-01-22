spamnumericdata <- read.csv("SpamBase/spambase.csv", header= T, sep = ",", row.names = NULL)
spamliteraldata <- read.csv("SpamCollection/SMSSpamCollection.csv", header= TRUE, sep = "\t", row.names = NULL, quote = NULL)


# Instalacja i załadowanie pakietów RWeka i RTextTools
 options(install.packages.check.source = FALSE)
#install.packages("tm", dependencies = TRUE)
#library(RWeka)
# RTextTools - konieczna instalacja ręczna ze źródeł.
library(class)
library(e1071)
sum(spamnumericdata$class==1)

# Przygotowanie danych treningowych i testowych.
niters <- 5
knn_k <- 2
results <- c()
for (i in 1:niters) {
  n <- nrow(spamnumericdata)
  spamnumericdata <- spamnumericdata[sample(nrow(spamnumericdata)),] # Losowa kolejność wierszy.
  train_data_size <- floor(0.7*n)
  train_data_indices <- sample(train_data_size)
  train <- spamnumericdata[train_data_indices, (1:ncol(spamnumericdata))] # Bez ostatniej kolumny, czyli decyzji.
  test <- spamnumericdata[-train_data_indices, (1:ncol(spamnumericdata))] # Pozostałe wiersze, też bez ostatniej kolumny.
  train_class <- factor(spamnumericdata$spam[train_data_indices])
  test_class <- factor(spamnumericdata$spam[-train_data_indices])
  model <- naiveBayes(spam ~ ., data = train)
  pred <- predict(model, test)
  table(pred, test$spam)
  ##classifier <-naiveBayes(test,train) # knn(train, test, train_class, k = knn_k, prob=TRUE)
  
  #ntest <- nrow(test)
  #correct <- sum(result == test_class)
  #correct_ratio <- correct / ntest
  #results <- c(results, correct_ratio)
}
#print(mean(results))


