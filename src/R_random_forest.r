spamnumericdata <- read.csv("SpamBase/spambase.csv", header= T, sep = ",", row.names = NULL)
spamliteraldata <- read.csv("SpamCollection/SMSSpamCollection.csv", header= TRUE, sep = "\t", row.names = NULL, quote = NULL)


# Instalacja i załadowanie pakietu e1071
library(randomForest)

sum(spamnumericdata$class==1)

# Przygotowanie danych treningowych i testowych.
niters <- 5

results <- c()
for (i in 1:niters) {
  

  n <- nrow(spamnumericdata)
  spamnumericdata <- spamnumericdata[sample(nrow(spamnumericdata)),] # Losowa kolejność wierszy.
  train_data_size <- floor(0.4*n)
  train_data_indices <- sample(train_data_size)
  train <- spamnumericdata[train_data_indices, (1:ncol(spamnumericdata))] # Bez ostatniej kolumny, czyli decyzji.
  test <- spamnumericdata[-train_data_indices, (1:ncol(spamnumericdata))] # Pozostałe wiersze, też bez ostatniej kolumny.
  
  train_class <- factor(spamnumericdata$class[train_data_indices])
  test_class <- factor(spamnumericdata$class[-train_data_indices])
  
   model <- randomForest( as.factor(class) ~ ., data = train)
   pred <- predict(model, test)
   
 
  
   ntest <- nrow(test)
   correct <- sum(pred == test_class)
   correct_ratio <- correct / ntest
  results <- c(results, correct_ratio)
}
print(mean(results))