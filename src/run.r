source("knn.r")
source("tfidf.r")
source("naive_bayes.r")
source("tfidf.r")

source("R_knn.r")
source("R_naive_bayes.r")
source("R_random_forest.r")
source("R_svm.r")

run_experiment <- function(train_data_size, test_data_size, niters){
  spamnumericdata <- read.csv("../dane/SpamBase/spambase.csv", header= TRUE, sep = ",", row.names = NULL)
  spamliteraldata <- read.csv("../dane/SpamCollection/SMSSpamCollection.csv", header= TRUE, sep = "\t", row.names = NULL, quote = NULL, stringsAsFactors = FALSE)
  spamliteraldata$class <- ifelse(spamliteraldata$category == "spam", 1, 0)
  # Wyrownanie licznosci klas w literal data.
  hams_lit <- sum(spamliteraldata$class == 0)
  spams_lit <- sum(spamliteraldata$class == 1)
  nc_lit <- min(hams_lit, spams_lit)
  indices <- c(which(spamliteraldata$class == 0)[1:nc_lit],
               which(spamliteraldata$class == 1)[1:nc_lit])
  spamliteraldata <- spamliteraldata[indices, 2:3]
  
  
  #train_data_size <- floor(0.7*n)
  #test_data_size <- n - train_data_size
  
  # Lista testowanych klasyfikatorów
  classifiers <- c(make_knn_classifier(3), make_r_knn_classifier(3), r_random_forest_classify, tf_idf_classify, naive_bayes_classify, r_naive_bayes_classify, r_svm_classify)
  classifiers_names <- c("knn_3", "rknn_3", "rrandom_forest", "tfidf", "naive bayes", "r naive bayes","rsvm")
  classifiers_types <- c("numeric", "numeric", "numeric", "literal", "literal", "literal", "numeric")
  
  # Przygotowanie danych treningowych i testowych.
  cat("Test run: ", niters, " iterations. train_data_size=", train_data_size, " test_data_size=", test_data_size, "\n", sep="")
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
    total_time <- c()
    for (i in 1:niters) {
      n <- nrow(data)
      data <- data[sample(n),] # Losowa kolejność wierszy.
      train_data_indices <- 1:train_data_size
      test_data_indices <- 1:test_data_size + train_data_size
      train <- data[train_data_indices, (1:ncol(data)-1)] # Bez ostatniej kolumny, czyli decyzji.
      test <- data[test_data_indices, (1:ncol(data)-1)] # Pozostałe wiersze, też bez ostatniej kolumny.
      train_class <- factor(data$class[train_data_indices])
      test_class <- factor(data$class[test_data_indices])
      
      ptm <- proc.time()["elapsed"]
      result <- classifier(train, train_class, test)
      total_time <- c(total_time, proc.time()["elapsed"] - ptm["elapsed"])
      
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
    correct_ratio = (TP+TN)/(TP+TN+FP+FN)
    
    cat("Classifier=", classifiers_names[[c_id]],
        " on datatype=", classifiers_types[[c_id]],
        " --> correct_ratio=", correct_ratio,
        ", recall=", recall,
        ", precision=", precision, ", run_time=", mean(total_time)*1000, "ms\n", sep="")
  }
}

# Przykład uruchomienia eksperymentu: 100 przykładów uczących, 20 przykładów testowych, 5 iteracji wykonywania klasyfikacji na losowych danych
run_experiment(100, 40, 5)