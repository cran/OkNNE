OKNNE <-
function(xtrain, ytrain, xtest = NULL, ytest = NULL, k = 10,
                  B = 100, direction = "forward", q = trunc(sqrt(ncol(xtrain))),
                  algorithm=c("kd_tree", "cover_tree", "CR", "brute")){

   pred1 <- c(); pred2 <- list();
   xtest1 <- if(is.null(xtest)){xtrain}else{xtest}
  n <- nrow(xtest1); m <- nrow(xtrain); p <- ncol(xtrain)

  for (b in 1:B) {
    boot <- sample(1:m, replace = F)
    fset <- sample(1:p, q, replace = F)
    x.train <- xtrain[boot, fset]
    y.train <- ytrain[boot]
    x.test <- xtest1[,fset]
    fn <- if(is.null(xtest)){get.knn(x.train, k=k, algorithm=algorithm)$nn.index}else{
      get.knnx(x.train, x.test, k=k, algorithm=algorithm)$nn.index}

    for (i in 1:n) {
      kdata <- cbind(x.train[fn[i,],], y=y.train[fn[i,]])
      mod=if(direction=="forward"){y~1} else {y~.}
      lm <- lm(mod, data = kdata)
      model <- step(lm, direction = direction, trace = 0)
      pred1[i] <- predict(model, x.test[i,])
    }

    pred2[[b]] <- pred1
  }
  res <- list(call = match.call())
  pred3 <- do.call(cbind, pred2)

  if(is.null(xtest)){
      res$PREDICTIONS <- rowMeans(pred3)
      res$RMSE <- sqrt(mean((res$PRED-ytrain)^2))
      res$MAE <- mean(abs(res$PRED-ytrain))
      res$R.SQUARE <- 1-(sum((res$PRED-ytrain)^2)/sum((ytrain-mean(ytrain))^2))
      res$CORRELATION <- cor(res$PRED, ytrain)
      class(res) <- "OKNNE"
      return(res)}else {if(is.null(ytest)){
    res$PREDICTIONS <- rowMeans(pred3)
    class(res) <- "OKNNE"
    return(res)}else{
    res$PREDICTIONS <- rowMeans(pred3)
    res$RMSE <- sqrt(mean((res$PRED-ytest)^2))
    res$MAE <- mean(abs(res$PRED-ytest))
    res$R.SQUARE <- 1-(sum((res$PRED-ytest)^2)/sum((ytest-mean(ytest))^2))
    res$CORRELATION <- cor(res$PRED, ytest)
    class(res) <- "OKNNE"
    return(res)
    }
  }
}
