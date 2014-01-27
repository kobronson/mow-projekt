library('e1071');
library('tm');

r_naive_bayes_classify <- function(train_data, train_class, test_data){

  trainvector <- as.vector(train_data);
  testvector <- as.vector(test_data);
  print(test_data)
  
  trainsource <- VectorSource(trainvector);
  testsource <- VectorSource(testvector);
  
  
  traincorpus <- Corpus(trainsource)
  testcorpus <- Corpus(testsource)
  
  # Stem
  traincorpus <- tm_map(traincorpus,stripWhitespace)
  traincorpus <- tm_map(traincorpus,tolower)
  traincorpus <- tm_map(traincorpus, removeWords,stopwords("english"))
  
  testcorpus <- tm_map(testcorpus,stripWhitespace)
  testcorpus <- tm_map(testcorpus,tolower)
  testcorpus <- tm_map(testcorpus, removeWords,stopwords("english"))
  
  
  # tm matrix
  trainmatrix <- t(TermDocumentMatrix(traincorpus));
  testmatrix <- t(TermDocumentMatrix(testcorpus));
  
  
  model <- naiveBayes(as.matrix(trainmatrix),as.factor(train_class));
  
            
  predict(model,as.matrix(testmatrix));

  
}