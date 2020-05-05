#' Linear Regression/Covariate Exploration
#'
#' This package takes in a vector of outcomes, a matrix of covariates, and the number of rows of the independent variable. It then takes the linear regression of the given X and Y variables and returns the corresponding p-values and coefficients. The function also returns graphs of each of the covariates. 
#' 
#' @param Y A vector of outcomes.
#' @param X a matrix of covariates.
#' @param sub: a list of subjects (i.e. a set of integers corresponding to rows in X).
#' @return p value of \code{x}, coefficients of \code{x}, and graphs of covariates of \code{x}.
#' @export
#' @examples
#' myLinearRegression(myData[,1],myData[,2:6],1:30)
#' myLinearRegression(myData[,1],myData[,2:4],1:40)


myLinearRegression <-  function(Y,X,sub){
  if (ncol(X) > 5) {
    print("Too many variables to plot.")
    return(NULL)
  }
  xcs = c()
  if (ncol(X) == 1) {
    model = lm(Y[sub] ~ X[sub,1])
  }
  if (ncol(X) == 2) {
    model = lm(Y[sub] ~ X[sub,1] + X[sub,2])

  }
  if (ncol(X) == 3) {
    model = lm(Y[sub] ~ X[sub,1] + X[sub,2] +  X[sub,3])

  }
  if (ncol(X) == 4) {
    model = lm(Y[sub] ~ X[sub,1] + X[sub,2] +  X[sub,3] +  X[sub,4])

  }
  else {
    model = lm(Y[sub] ~ X[sub,1] + X[sub,2] +  X[sub,3] +  X[sub,4] +  X[sub,5])

  }
  return(list( "Coefficients" =  model$coefficients,
               "P-Values" = summary(model)$coefficients[,4],
               GGally::ggpairs(X)))
}

