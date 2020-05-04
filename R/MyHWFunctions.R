#' Add together two numbers.
#'
#' This is truly a great and much-needed function
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @examples
#' aGreatFunction(1, 1)
#' aGreatFunction(10, 1)

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

