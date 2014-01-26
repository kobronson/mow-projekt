tf_idf_classify <- function(train_data, train_class, test_data){
  
  # Wybiera unikatowe słowa wystepujace w wiadomosci.
  get_words <- function(message){
    words <- strsplit(message, "[^A-Za-z0-9]")[[1]]
    lowered <- tolower(words)
    lowered[which(lowered != "")]
  }
  
  # Tworzy wektor wszystkich termów występujących w zbiorze danych treningowych
  get_all_words <- function(train_data){
    all_words <- paste(train_data, collapse = ' ')
    unique(get_words(all_words))
  }
  
  calculate_tf <- function(word, message){
    if(length(message)==0){
      print("zeero")
    }
    sum(word == message)/length(message)
  }
  
  words_in_train_messages <- sapply(train_data, get_words, USE.NAMES=FALSE)
  D <- length(train_data)
  
  calculate_idf <- function(word)
  {
    log(D/sum(sapply(words_in_train_messages, function(words_in_message) { word %in% words_in_message})))
  }
  
  all_words <- get_all_words(train_data)
  all_words_idfs <- sapply(all_words, calculate_idf)
  
  calculate_tfidf <- function(word, message){
    calculate_tf(word,message) * all_words_idfs[word]
  }
  
  calculate_mes_tfidfs_for_all_words <- function (message){
    message <- get_words(message)
    sapply(all_words, function(word) {calculate_tfidf(word, message)}, USE.NAMES = FALSE)
  }
  
  train_data_table <- t(sapply(train_data, calculate_mes_tfidfs_for_all_words))
  test_data_table <- t(sapply(test_data, calculate_mes_tfidfs_for_all_words))
  
  knn(train_data_table, test_data_table, train_class, k=3)
}

