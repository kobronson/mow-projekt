source("knn.r")
source("tfidf.r")
source("naive_bayes.r")
source("R_knn.r")
source("R_random_forest.r")
source("R_svm.r")

run_experiment <- function(train_data_size, test_data_size, niters){
  spamnumericdata <- read.csv("../dane/SpamBase/spambase.csv", header= TRUE, sep = ",", row.names = NULL)
  spamliteraldata <- read.csv("../dane/SpamCollection/SMSSpamCollection.csv", header= TRUE, sep = "\t", row.names = NULL, quote = NULL, stringsAsFactors = FALSE)
  spamliteraldata$class <- ifelse(spamliteraldata$category == "spam", 1, 0)
  spamliteraldata <- spamliteraldata[, 2:3]
  
  train <- spamnumericdata[train_data_indices, (1:ncol(spamnumericdata))] # Bez ostatniej kolumny, czyli decyzji.
  test <- spamnumericdata[-train_data_indices, (1:ncol(spamnumericdata))] # Pozostałe wiersze, też bez ostatniej kolumny.
  
  n <- nrow(spamnumericdata)
  #train_data_size <- floor(0.7*n)
  #test_data_size <- n - train_data_size
  
  # Lista testowanych klasyfikatorów
  classifiers <- c(make_knn_classifier(3), make_r_knn_classifier(3), r_random_forest_classify, tf_idf_classify, naive_bayes_classify, r_svm_classify)
  classifiers_names <- c("knn_3", "rknn_3", "rrandom_forest", "tfidf", "naive bayes", "rsvm")
  classifiers_types <- c("numeric", "numeric", "numeric", "literal", "literal", "numeric")
  
  # Przygotowanie danych treningowych i testowych.
  cat("Test run: ", niters, "iterations. train_data_size=", train_data_size, " test_data_size=", test_data_size, "\n", sep="")
  for (c_id in 1:length(classifiers)){
    classifier <- classifiers[[c_id]]
    results <- c()
    true_positives <- c()
    true_negatives <- c()
    false_positives <- c()
    false_negatives <- c()
    
    if(classifiers_types[[c_id]] == "numeric"){
      data <- spamnumericdata
    }
    else{
      data<- spamliteraldata
    }
    
    for (i in 1:niters) {
      data <- data[sample(n),] # Losowa kolejność wierszy.
      train_data_indices <- 1:train_data_size
      test_data_indices <- 1:test_data_size + train_data_size
      train <- data[train_data_indices, (1:ncol(data)-1)] # Bez ostatniej kolumny, czyli decyzji.
      test <- data[test_data_indices, (1:ncol(data)-1)] # Pozostałe wiersze, też bez ostatniej kolumny.
      train_class <- factor(data$class[train_data_indices])
      test_class <- factor(data$class[test_data_indices])
      
      result <- classifier(train, train_class, test)
      correct <- sum(result == test_class)
      correct_ratio <- correct / train_data_size
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
    correct_ratio = sum(TP+TN)/sum(TP+TN+FP+FN)
    
    cat("Classifier=", classifiers_names[[c_id]],
        " on datatype=", classifiers_types[[c_id]],
        " --> correct_ratio=", correct_ratio,
        ", recall=", recall,
        ", precision=", precision, "\n", sep="")
  }
}
