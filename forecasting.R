

library(caret)

# estimator for forecasting time series.
# uses knn and linear regression to build a
# model which combines global and local trends

forecaster <- function(period, k){
  self <- list(
    get = function(x) self[[x]]
    ,set = function(x, value) self[[x]] <<- value
    ,period = period
    ,k = k
  )
  
  self$gen_df <- function(ts, fit=FALSE){
    df <- data.frame(dt = strptime(ts, '%Y-%m-%d'))
    if (fit){
      self$set('min_dt', min(df$dt) - 86400)
    }
    
    df$ordinal <- as.numeric((df$dt - self$min_dt)) 
    df$o.sin <- sin(df$ordinal %% self$period / self$period * 2 * pi)
    df$o.cos <- cos(df$ordinal %% self$period / self$period * 2 * pi)
    return(df)
  }
  
  self$fit.knn <- function(X_train, y_train){
    knnreg(X_train, y_train, k = self$k)
  }
  
  self$fit <- function(ts_train, y_train){
    df <- self$gen_df(ts_train, fit=TRUE)
    self$set('knn', self$fit.knn(subset(df, select=c('o.sin','o.cos')), y_train))
    df$knn.estimate <- predict(self$knn, subset(df, select=c('o.sin','o.cos')))
    df$y <- y_train
    self$set('lm', lm(y ~ ordinal + knn.estimate, data = df))
  }
  
  self$predict <- function(ts_test){
    df <- self$gen_df(ts_test)
    df$knn.estimate <- predict(self$knn, subset(df, select=c('o.sin','o.cos')))
    yh <- predict(self$lm, newdata = df)
    return(yh)
  }
  
  class(self) <- 'forecaster'
  return(self)
}


