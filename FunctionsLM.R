# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, where epsilon is normal zero with variance sigma^2 independent across samples. Seed should be set at the beginning of the function
# X - design matrix
# beta - given parameter vector
# sigma - standard deviation of the noise
# seed  - starting seed value
generateY <- function(X, beta, sigma, seed = 5832652){
  
  #(exception) X should be 2-dimensional.
  
  if(length(dim(X))>2) {
    stop("X is more than 2-dimensional") 
  }else if(length(dim(X))>2) {
    stop("X is less than 2-dimensional")
  }
  
  #(exception) X and beta has to be compatible for matrix multiplication.
  
  if(dim(X)[2]!=length(beta)) {
    stop("the dimensions of X and beta are not compatible for matrix multiplication.") 
  }
  
  #[ToDo] Set seed and generate Y following linear model
  set.seed(seed)
  epsilon <- sigma*rnorm(dim(X)[1])
  Y <- X%*%beta + epsilon

  # Return Y
  return(Y)
}

# Calculate beta_LS - least-squares solution, do not use lm function
# X - design matrix
# Y -response
calculateBeta <- function(X, Y){
}

# Calculate MSE
calculateMSE <- function(beta, beta_LS){
}