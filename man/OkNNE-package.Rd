\name{OkNNE-package}
\alias{OkNNE-package}
\alias{OkNNE}
\docType{package}
\title{
A k-Nearest Neighbours Ensemble via Optimal Model Selection for Regression
}
\description{
Optimal k-Nearest Neighbours Ensemble "OkNNE" is an ensemble of base k-NN models each constructed on a bootstrap sample with a random subset of features. k closest observations are identified for a test point "x" (say), in each base k-NN model to fit a stepwise regression to predict the output value of "x". The final predicted value of "x" is the mean of estimates given by all the models. OkNNE takes training and test datasets and trains the model on training data to predict the test data.
}
\details{
\tabular{ll}{
Package: \tab OkNNE\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2020-07-22\cr
License: \tab GPL-3\cr
}
}
\author{
Amjad Ali, Muhammad Hamraz, Zardad Khan

Maintainer: Amjad Ali <aalistat1@gmail.com>
}
\references{
A. Ali et al., "A k-Nearest Nieghbours Based Ensemble Via Optimal Model Selection For Regression," in IEEE Access, doi: 10.1109/ACCESS.2020.3010099.

Li, S. (2009). Random KNN modeling and variable selection for high dimensional data.

Shengqiao Li, E James Harner and Donald A Adjeroh. (2011). Random KNN feature selection- a fast and stable alternative to Random Forests. BMC Bioinformatics , 12:450.

Alina Beygelzimer, Sham Kakadet, John Langford, Sunil Arya, David Mount and Shengqiao Li (2019). FNN: Fast Nearest Neighbor Search Algorithms and Applications. R package version 1.1.3.

Venables, W. N. and Ripley, B. D. (2002). Modern Applied Statistics with S. New York: Springer (4th ed).
}
\keyword{ OkNNE }
\keyword{ Optimal k-NN }
\keyword{ Bootstraping }
\keyword{ Regression }
