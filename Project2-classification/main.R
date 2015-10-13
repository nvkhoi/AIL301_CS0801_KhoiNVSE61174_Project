rm(list = ls());
library(lattice);
library(jpeg);
library(caret); 
library(e1071);
library(nnet);
library(rpart);

#recursive function for partial color averaging (if targetlayer is 1 then it return the color averaging)
colorAveraging <- function(img, r1, c1, r2, c2, currentlayer, targetlayer){
  retVal <- c();
  r = 0.0; g = 0.0; b = 0.0;
  area = (r2 - r1 + 1) * (c2 - c1 + 1);
  for (i in r1: r2){
    for (j in c1: c2){
      r <- r + img[i, j, 1] * 255;
      g <- g + img[i, j, 2] * 255;
      b <- b + img[i, j, 3] * 255;
    }
  }
  retVal <- c(retVal, c(r / area, g / area, b / area));
  
  if (currentlayer < targetlayer) {
    rmid <- (r1 + r2) / 2;
    cmid <- (c1 + c2) / 2;
    retVal <- c(retVal, colorAveraging(img, r1, c1, rmid, cmid, currentlayer + 1, targetlayer));
    retVal <- c(retVal, colorAveraging(img, rmid + 1, c1, r2, cmid, currentlayer + 1, targetlayer));
    retVal <- c(retVal, colorAveraging(img, r1, cmid + 1, rmid, c2, currentlayer + 1, targetlayer));
    retVal <- c(retVal, colorAveraging(img, rmid + 1, cmid + 1, r2, c2, currentlayer + 1, targetlayer));
  } 
  return(retVal);
}

#recursive function for partial color histogram (if targetlayer is 1 then it return the color histogram)
colorHistogram <- function(img, r1, c1, r2, c2, currentlayer, targetlayer, color) {
  retVal <- c();
  h <- 0 * c(1: 16);
  for (i in r1: r2) {
    for (j in c1: c2) {
      x <- floor(255 * img[i, j, color] / 16.0) + 1;
      h[x] <- h[x] + 1;
    }
  }
  retVal <- c(retVal, h);
  rmid <- (r1 + r2) / 2;
  cmid <- (c1 + c2) / 2;
  if (currentlayer < targetlayer) {
    retVal <- c(retVal, colorHistogram(img, r1, c1, rmid, cmid, currentlayer + 1, targetlayer, color));
    retVal <- c(retVal, colorHistogram(img, rmid + 1, c1, r2, cmid, currentlayer + 1, targetlayer, color));
    retVal <- c(retVal, colorHistogram(img, r1, cmid + 1, rmid, c2, currentlayer + 1, targetlayer, color));
    retVal <- c(retVal, colorHistogram(img, rmid + 1, cmid + 1, r2, c2, currentlayer + 1, targetlayer, color));
  }
  return(retVal);
}

#output the classification to html file, 
#the method to name the file, 
#setName to separate between feature selection dataset, 
#result produced by method, 
#expected is real tag
#fileList is jpeg file paths
#fileIndex is the index of file in result set
outputToFile <- function(method, setName, result, expected, fileList, featureType, fileIndex = NULL) {
  imgType <- c("coast", "forest", "highway", "insidecity", "mountain", "opencountry", "street", "tallbuilding");
  #output will be in the input folder
  e1 <- abs(result != expected);
  result <- as.data.frame(unclass(t(result)));
  expected <- as.data.frame((unclass(t(expected))));
  sink(file = paste("output", method, ".html", sep =""), append = TRUE, type = "output");
  cat("<style>
	.cell { display:inline-block; border: solid 1px #aeaeae; padding: 5px; margin: 3px; border-radius: 3px; background-color: lavender; }
      .small { width:64px; }
      .tag { color: red; }
      </style>");
  cat("<h1> ", setName ,"error for ",featureType, ": ", sum(e1) * 100.0/ncol(result), "%<h1>\n");

  
  for (j in 1 : ncol(result)) {
    for (i in 1: length(imgType)){
      if (result[j] == i) {
        cat("<div class=cell>");
        cat("<!--", colnames(result)[j], "-->");
        imgIndex <- 0;
        #my implementation make SVM produce the result set a little difference than others
        if (grepl("SVM", method)){
          imgIndex = as.numeric(colnames(result)[j]);
        } else {
          imgIndex = as.numeric(fileIndex[j]);
        }
        
        if (result[j] == expected[j]){
          cat ("<img class=small src = ", fileList[imgIndex], ">\n");
        } else {
          cat ("<img src = ", fileList[imgIndex], " border=5>\n");
        }
        cat("<br><span class=tag><font size=3>", imgType[i]);
        cat("</font></span></div>");
        break;
      }
    }
    
  }
  cat("<hr>\n");
  sink(file = NULL, type = "output");
}

#use SVM
trainAndTestSVM <- function(trainingset, testset, fileList, featureType){
  tryDegree = 10;
  chosenDegree = 0;
  tmpErr = 1.0;
  for (i in 1: tryDegree) {
    x <- subset(trainingset, select=-c(Category));
    y <- subset(trainingset, select=c(Category));
    M <- svm(data.matrix(x), y, type='C', kernel='polynomial', degree=i);
    y <- trainingset[, "Category"]; 
    y1 <- predict(M, data.matrix(x));
    e1 <- abs(y1 != y);
    if (sum(e1)/nrow(trainingset) < tmpErr){
      tmpErr = sum(e1)/nrow(trainingset);
      chosenDegree = i;
    }
  }
  
  x <- subset(trainingset, select=-c(Category));
  y <- subset(trainingset, select=c(Category));
  M <- svm(data.matrix(x), y, type='C', kernel='polynomial', degree=chosenDegree);
  x <- subset(trainingset, select=-c(Category));
  y <- trainingset[, "Category"]; 
  y1 <- predict(M, data.matrix(x));
  e1 <- abs(y1 != y);
  trainErr <- sum(e1)/nrow(trainingset);
  #print(head(data.frame(y, y1, e1)));
  #table(y, y1);
  
  outputToFile(paste("SVMdeg", chosenDegree, sep=""),"Training ", y1, y, fileList, featureType);

  x <- subset(testset, select=-c(Category));
  y <- testset[, "Category"]; 
  y1 <- predict (M, data.matrix(x)); 
  e1 <- abs(y1 != y);
  testErr <- sum(e1)/nrow(testset);
  outputToFile(paste("SVMdeg", chosenDegree, sep=""), "Testing ", y1, y, fileList, featureType);
  return(c(trainErr, testErr));
}

#use logistic regression
trainAndTestLogisticRegression <- function(trainingset, testset, fileList, featureType){
  x <- subset(trainingset, select=-c(Category));
  y <- trainingset[, "Category"];
  fileIndex <- rownames(trainingset);
  M <- multinom(y ~ data.matrix(x), data = trainingset);
  x <- subset(trainingset, select=-c(Category));
  y <- trainingset[, "Category"]; 
  
  y1 <- predict (M, newdata = trainingset); 
  e1 <- abs(y1 != y);
  trainErr <- sum(e1)/nrow(trainingset);
  outputToFile("LogisticRegression","Training", y1, y, fileList, featureType, fileIndex);
  
  fileIndex <- rownames(testset);
  x <- subset(testset, select=-c(Category));
  y <- testset[, "Category"]; 
  y1 <- predict (M, data.matrix(x)); 
  e1 <- abs(y1 != y);
  testErr <- sum(e1)/nrow(testset);
  outputToFile("LogisticRegression", "Testing", y1, y, fileList, featureType, fileIndex);
  return(c(trainErr, testErr));
}

#use random forest
trainAndTestRandomForest <- function(trainingset, testset, fileList, featureType){
  fileIndex <- rownames(trainingset);
  M <- train(Category ~ ., method = 'rf', data = trainingset);
  print(M);
  x <- subset(trainingset, select=-c(Category));
  y <- trainingset[, "Category"]; 
  y1 <- predict (M, trainingset); 
  e1 <- abs(y1 != y);
  trainErr <- sum(e1)/nrow(trainingset);
  outputToFile("RandomForest","Training", y1, y, fileList, featureType, fileIndex);
  
  fileIndex <- rownames(testset);
  x <- subset(testset, select=-c(Category));
  y <- testset[, "Category"]; 
  y1 <- predict (M, data.matrix(x)); 
  e1 <- abs(y1 != y);
  testErr <- sum(e1)/nrow(testset);
  outputToFile("RandomForest", "Testing", y1, y, fileList, featureType, fileIndex);
  return(c(trainErr, testErr));
}

#use boosting
trainAndTestBoosting <- function(trainingset, testset, fileList, featureType){
  fileIndex <- rownames(trainingset);
  x <- subset(trainingset, select=-c(Category));
  y <- trainingset[, "Category"]; 
  M <- train(Category ~ ., method = 'gbm', data = trainingset);
  x <- subset(trainingset, select=-c(Category));
  y <- trainingset[, "Category"]; 
  y1 <- predict (M, newdata = trainingset); 
  e1 <- abs(y1 != y);
  trainErr <- sum(e1)/nrow(trainingset);
  outputToFile("Boosting","Training", y1, y, fileList, featureType, fileIndex);
  
  fileIndex <- rownames(testset);
  x <- subset(testset, select=-c(Category));
  y <- testset[, "Category"]; 
  y1 <- predict (M, as.matrix(x)); 
  e1 <- abs(y1 != y);
  testErr <- sum(e1)/nrow(testset);
  outputToFile("Boosting", "Testing", y1, y, fileList, featureType, fileIndex);
  rm(M);
  return(c(trainErr, testErr));
}

#generate feature vectors and store them to CSV file
generateCSVData <- function(featureType, layer, files, numfile) {
  dataset <- rbind();
  outFileName <- paste(featureType, "Layer", layer, ".csv", sep = "");
  
  for (i in 1 : numfile) {
    img <- readJPEG(files[i]);
    m <- nrow(img[ , , 1]);
    n <- ncol(img[ , , 1]);
    thisType <- sapply(strsplit(sapply(strsplit(files[i], "_"), "[", 1), "/"), "[", 3);
    x <- c();
    if (featureType == "avg") {
      x <- colorAveraging(img, 1, 1, m, n, 1, layer);
    }
    else {
      for (color in 1 : 3) {
        x <- c(x, colorHistogram(img, 1, 1, m, n, 1, layer, color));
      }
    }
    
    x <- c(x, thisType);
    dataset <- rbind(dataset, x);
  }
  colnames(dataset)[ncol(dataset)] <- 'Category';
  for (i in 1: ncol(dataset) - 1)
    colnames(dataset)[i] <- paste("feature", i, sep="_");
  print(head(dataset));
  write.csv(dataset, file = outFileName);
  return (outFileName);
}

#run all method for inputData
trainAndTestData <- function(inputData) {
  Err <- c();
  dataset <- read.table(file=inputData, header=TRUE, sep=",");
  dataset <- as.data.frame(dataset);
  set.seed(103);
  datapart <- createDataPartition(dataset$Category, p = 0.7, list = FALSE);
  trainingset <- dataset[datapart, ];
  testset <-dataset[-datapart, ];
  for (i in  1 : ncol(trainingset) - 1) {
    colnames(trainingset)[i] <- i;
    colnames(testset)[i] <- i;
  }
  
  Err <- c(Err, trainAndTestSVM(trainingset, testset, files, inputData));
  if (ncol(dataset) <= 200){
    Err <- c(Err, trainAndTestLogisticRegression(trainingset, testset, files, inputData));
  }
    
  if (ncol(dataset) <= 1000){
    Err <- c(Err, trainAndTestRandomForest(trainingset, testset, files, inputData));
    Err <- c(Err, trainAndTestBoosting(trainingset, testset, files, inputData));
  }
  return(Err);
}

#read the feature table, enable fromCSV to read from existing CSV, disable fromCSV to read from jpeg files
readInput <- function(fromCSV = TRUE, files){
  n <- length(files);
  inputData <- rbind();
  if (fromCSV){
    inputData <- rbind("avgLayer1.csv", "avgLayer2.csv", "avgLayer3.csv", "histogramLayer1.csv", "histogramLayer2.csv", "histogramLayer3.csv");
  } else {

    
    #define additional data
    for (layer in 1: 3) {
      inputData <- rbind(inputData, generateCSVData("avg", layer, files, n));
      inputData <- rbind(inputData, generateCSVData("histogram", layer, files, n));
    }
    
  }
  return(inputData);
}

#read files, please don't change the relative path to absolute, the category will get wrong 
files <- list.files("/mit8-images-64x64", full.name = TRUE);
inputData <- readInput(fromCSV = TRUE, files);

ErrTable <- rbind();
for (dataIndex in 1: length(inputData)) {
  ErrTable <- rbind(ErrTable, trainAndTestData(inputData[dataIndex, 1]));
  gc();
}

write.csv(ErrTable, "Err.csv")
