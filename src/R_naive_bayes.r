r_naive_bayes_classify <- function(train_data, train_class, test_data){
  # Załadowanie pakietu e1071
  library(e1071)
  
  #To trzeba zmienić bo jest dla klasyfikacji tekstu bez sensu.
  model <- naiveBayes(train_class ~ ., data = train_data)
  predict(model, test_data)   
}