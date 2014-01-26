# Instalacja i załadowanie pakietu class zawierającego podstawową wersję knn
#install.packages("class", dependencies = TRUE)

make_r_knn_classifier <- function(k){
  
  library(class)
  
  r_knn_classify <- function(train_data, train_class, test_data){
    knn(train_data, test_data, train_class, k, prob=TRUE)
  }
  
  r_knn_classify
}