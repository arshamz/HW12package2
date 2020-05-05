#' Takes the linear regression of X and Y variables and results in p-values, coefficients, and graphs of covariates.
#'
#' This is truly a great and much-needed function
#'
#' @param Y A vector of outcomes.
#' @param X a matrix of covariates.
#' @param sub: a list of subjects (i.e. a set of integers corresponding to rows in X).
#' @return The sum of \code{x} and \code{y}.
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

