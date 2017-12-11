#'Square a number
#'works with 'iris' data set
#'@param x A data set
#'@return returns squared number
#'@export




perceptron<-function(x){
  data<-x
  library(rgl)
  library(ggplot2)

  Random.Unit <-function(data) {
    irissubdf <- data[0:100, c(1, 3, 5)]
    n<-nrow(irissubdf)
    names(irissubdf) <- c("sepal", "petal", "species")
    irissubdf[, 4] <- 1
    irissubdf[irissubdf[, 3] == "setosa", 4] <- -1
    irissubdf<-irissubdf[,c(4,2,1)]
    names(irissubdf) <- c( "label","sepal", "petal")
    irissubdf
    x0<-rep(1,n)
    irissubdf<-cbind(x0 , irissubdf)
    irissubdf<-irissubdf[,c(2,1,3,4)]
    return(as.matrix(irissubdf))
  }

  Classify <- function(x, weights) {
    return(sign(x %*% weights))
  }

  Perceptron <- function(data, threshold) {
    w <- c(-threshold, runif(ncol(data) - 2))
    n <- nrow(data)
    label <- data[ , 1]
    obs <- data[ , 2:ncol(data)]
    misclassfied <- TRUE
    while (misclassfied) {
      misclassfied <- FALSE
      for (i in 1:n) {
        if (label[i] * Classify(obs[i , ], w) <= 0) {
          w <- w + label[i] * obs[i , ]
          misclassfied <- TRUE
        }
      }
    }
    return(w)
  }



  Plot2D1 <- function(points, a, b) {
    plot(points[, 3:4], xlab = "Length", ylab = "Length",
         pch = ifelse(points[, 1] == 1, 2, 4),
         col = ifelse(points[, 1] == 1, "green", "red"))
    abline(a, b)
 }


  ## All good upto this point

  pts <- Random.Unit(data)
  THRESHOLD <-0.30

  Plot2D1(pts, THRESHOLD, -1)

  w <- Perceptron(pts, THRESHOLD)
  Plot2D1(pts, -w[1]/w[3], -w[2]/ w[3])


}

