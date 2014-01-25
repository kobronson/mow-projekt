source("knn.r")
source("tfidf.r")

spamnumericdata <- read.csv("../dane/SpamBase/spambase.csv", header= TRUE, sep = ",", row.names = NULL)
spamliteraldata <- read.csv("../dane/SpamCollection/SMSSpamCollection.csv", header= TRUE, sep = "\t", row.names = NULL, quote = NULL)
spamliteraldata$class <- ifelse(spamliteraldata$category == "spam", 1, 0)

train <- spamnumericdata[train_data_indices, (1:ncol(spamnumericdata))] # Bez ostatniej kolumny, czyli decyzji.
test <- spamnumericdata[-train_data_indices, (1:ncol(spamnumericdata))] # Pozostałe wiersze, też bez ostatniej kolumny.

n <- nrow(spamnumericdata)
#train_data_size <- floor(0.7*n)
#test_data_size <- n - train_data_size
train_data_size <- 100
test_data_size <- 20


# Lista testowanych klasyfikatorów
classifiers <- c(make_knn_classifier(3), make_knn_classifier(2), tf_idf_classify)
classifiers_names <- c("knn_3", "knn_2", "tfidf")

# Przygotowanie danych treningowych i testowych.
niters <- 5
for (c_id in 1:length(classifiers)){
  classifier <- classifiers[[c_id]]
  results <- c()
  true_positives <- c()
  true_negatives <- c()
  false_positives <- c()
  false_negatives <- c()
  for (i in 1:niters) {
    spamnumericdata <- spamnumericdata[sample(n),] # Losowa kolejność wierszy.
    train_data_indices <- 1:train_data_size
    test_data_indices <- 1:test_data_size + train_data_size
    train <- spamnumericdata[train_data_indices, (1:ncol(spamnumericdata)-1)] # Bez ostatniej kolumny, czyli decyzji.
    test <- spamnumericdata[test_data_indices, (1:ncol(spamnumericdata)-1)] # Pozostałe wiersze, też bez ostatniej kolumny.
    train_class <- factor(spamnumericdata$class[train_data_indices])
    test_class <- factor(spamnumericdata$class[test_data_indices])
    
    result <- classifier(train, train_class, test)
    ntest <- nrow(test)
    correct <- sum(result == test_class)
    correct_ratio <- correct / ntest
    true_positive <- sum(result == test_class & result == 1)
    true_negative <- sum(result == test_class & result == 0)
    false_positive <- sum(result != test_class & result == 1)
    false_negative <- sum(result != test_class & result == 0)
    results <- c(results, correct_ratio)
    true_positives <- c(true_positives, true_positive)
    true_negatives <- c(true_negatives, true_negative)
    false_positives <- c(false_positives, false_positive)
    false_negatives <- c(false_negatives, false_negative)
  }
  TP <- sum(true_positives)
  TN <- sum(true_negatives)
  FP <- sum(false_positives)
  FN <- sum(false_negatives)
  
  recall = TP/(TP + FN)
  precision = TP/(FP + TP)
  
  print(classifiers_names[[c_id]])
  print(recall)
  print(precision)
}
