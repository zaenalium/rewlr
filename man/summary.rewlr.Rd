% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_rewlr.R
\name{summary.rewlr}
\alias{summary.rewlr}
\title{Summarizing the rare event weighted logistic regreression}
\usage{
\method{summary}{rewlr}(model, digits = max(3L, getOption("digits") -
  3L), ...)
}
\arguments{
\item{model}{an object of class "rewlr"}

\item{digit}{the number of significant digits to use when printing}
}
\value{
rewlr returns output like glm, use function summary() to obtain the summary coefficients and others. The detail are shown in the following list:
\itemize{
\item coefficients - a named vector of coefficients.
\item fitted.values - return the prediction using the training data resulting probablity.
\item deviance - up to a constant, minus twice the maximized log-likelihood. Where sensible, the constant is chosen so that a saturated model has deviance zero.
\item AIC - A version of Akaike's An Information Criterion, minus twice the maximized log-likelihood plus twice the number of parameter.
\item null.deviance - The deviance for the null model, comparable with deviance. The null model will include the offset, and an intercept if there is one in the model.
\item df.residual - the residual degrees of freedom.
\item df.null - the residual degrees of freedom for the null model.
\item auc - an area under ROC curve
}
}
\description{
summary method for class "rewlr".
}
\examples{
library(rewlr)
data <- iris[1:55, ]
data$Species <- ifelse(data$Species == "setosa",0,1)
#Supposed that current sample data has 9 percent of rare event data, and the population has 2 percent of those rare event data.
(weight0 = (1 - 0.09)/(1-0.02))
(weight1 = (0.09)/(0.02))
iter = 1000; tol = 0.00001

fit <- rewlr(Species, data = data, weight0 = weight0, weight1 = weight1)
summary(fit)
p <- predict(fit, newdata = iris[c(1:22,61) , ])
}
