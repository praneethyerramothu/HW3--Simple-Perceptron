#'Classify two classes using Perceptron
#'works with 'iris' data set
#'@param x A data set
#'@return returns classified objects 
#'@export




perceptron<-function(x){
  data<-x
 
  
  Classify <- function(x, weight) {
    return(sign(x %*% weight))
  }
  DataC <-function(data) {
    irissubdf <- data[0:100, c(1, 3, 5)]
    n<-nrow(irissubdf)
    names(irissubdf) <- c("sepal", "petal", "species")
    irissubdf[, 4] <- 1
    irissubdf[irissubdf[, 3] == "setosa", 4] <- -1
    irissubdf<-irissubdf[,c(4,2,1)]
    names(irissubdf) <- c( "label","sepal", "petal")
    irissubdf
    p<-rep(1,n)
    irissubdf<-cbind(p , irissubdf)
    irissubdf<-irissubdf[,c(2,1,3,4)]
    return(as.matrix(irissubdf))
  }

  

  Perceptron <- function(data, Limit) {
    w <- c(-Limit, runif(ncol(data) - 2))
    n <- nrow(data)
    label <- data[ , 1]
    obs <- data[ , 2:ncol(data)]
    wrongClass <- TRUE
    while (wrongClass) {
      wrongClass <- FALSE
      for (i in 1:n) {
        if (label[i] * Classify(obs[i , ], w) <= 0) {
          w <- w + label[i] * obs[i , ]
          wrongClass <- TRUE
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

  pts <- DataC(data)
  Limit <-0.25

  Plot2D1(pts, Limit, -1)

  w <- Perceptron(pts, Limit)
  Plot2D1(pts, -w[1]/w[3], -w[2]/ w[3])


}

