\name{OKNNE}
\alias{OKNNE}
\title{
Optimal k-Nearest Neighbours Ensemble
}
\description{
Optimal k-Nearest Neighbours Ensemble "OkNNE" is an ensemble of base k-NN models each constructed on a bootstrap sample with a random subset of features. k closest observations are identified for a test point "x" (say), in each base k-NN model to fit a stepwise regression to predict the output value of "x". The final predicted value of "x" is the mean of estimates given by all the models. OkNNE takes training and test datasets and trains the model on training data to predict the test data.
}
\usage{
OKNNE(xtrain, ytrain, xtest = NULL, ytest = NULL, k = 10, B = 100,
direction = "forward", q = trunc(sqrt(ncol(xtrain))), algorithm =
c("kd_tree", "cover_tree", "CR", "brute"))
}
\arguments{
  \item{xtrain}{
The features space of the training dataset.
}
  \item{ytrain}{
The response variable of training dataset.
}
  \item{xtest}{
The test dataset to be predicted.
}
  \item{ytest}{
The response variable of test dataset.
}
  \item{k}{
The maximum number of nearest neighbors to search. The default value is set to 10.
}
  \item{B}{
The number of bootstrap samples.
}
  \item{direction}{
Method used to fit stepwise models. By default \code{forward} procedure is used.
}
  \item{q}{
The number of features to be selected for each base k-NN model.
}
  \item{algorithm}{
Method used for searching nearest neighbors.
}
}
\value{
 \item{PREDICTIONS }{Predicted values for test data response variable}
  \item{RMSE}{Root mean square error estimate based on test data}
  \item{R.SQUARE}{Coefficient of determination estimate based on test data}
  \item{CORRELATION}{Correlation estimate based on test data}
}
\references{
A. Ali et al., "A k-Nearest Nieghbours Based Ensemble Via Optimal Model Selection For Regression," in IEEE Access, doi: 10.1109/ACCESS.2020.3010099.

Li, S. (2009). Random KNN modeling and variable selection for high dimensional data.

Shengqiao Li, E James Harner and Donald A Adjeroh. (2011). Random KNN feature selection - a fast and stable alternative to Random Forests. BMC Bioinformatics , 12:450.

Alina Beygelzimer, Sham Kakadet, John Langford, Sunil Arya, David Mount and Shengqiao Li (2019). FNN: Fast Nearest Neighbor Search Algorithms and Applications. R package version 1.1.3.

Venables, W. N. and Ripley, B. D. (2002). Modern Applied Statistics with S. New York: Springer (4th ed).
}
\author{
Amjad Ali, Muhammad Hamraz, Zardad Khan

Maintainer: Amjad Ali <aalistat1@gmail.com>
}
\examples{
 data(SMSA)

 anyNA(SMSA)
 #[1] FALSE

 dim(SMSA)
 #[1] 59 15

 n=nrow(SMSA)

 X <- SMSA[names(SMSA)!="NOx"]
 Y <- SMSA[names(SMSA)=="NOx"]

 set.seed(55225)
 train.obs <- sample(1:n, 0.7*n, replace = FALSE)
 test.obs <- (1:n)[-train.obs]
 xtrain <- X[train.obs,]; ytrain <- Y[train.obs,];
 xtest <- X[test.obs,]; ytest <- Y[test.obs,]

 OkNNE.MODEL = OKNNE(xtrain, ytrain, xtest = xtrain, ytest = ytrain,
 k = 10, B = 5, q = trunc(sqrt(ncol(xtrain))), direction = "both",
 algorithm=c("kd_tree", "cover_tree", "CR", "brute"))

 OkNNE.MODEL

}
\keyword{ OkNNE }
\keyword{ Optimal k-NN }
\keyword{ Bootstraping }
\keyword{ Regression }
