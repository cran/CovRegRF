## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(CovRegRF)
data(data)
xvar.names <- colnames(data$X)
yvar.names <- colnames(data$Y)
data1 <- data.frame(data$X, data$Y)

set.seed(4567)
smp <- sample(1:nrow(data1), size = round(nrow(data1)*0.6), replace = FALSE)
traindata <- data1[smp,, drop=FALSE]
testdata <- data1[-smp, xvar.names, drop=FALSE]

## -----------------------------------------------------------------------------
formula <- as.formula(paste(paste(yvar.names, collapse="+"), ".", sep=" ~ "))
globalsig.obj <- significance.test(formula, traindata, params.rfsrc = list(ntree = 200), 
                                   nperm = 10, test.vars = NULL)
globalsig.obj$pvalue

## -----------------------------------------------------------------------------
covregrf.obj <- covregrf(formula, traindata, params.rfsrc = list(ntree = 200))
pred.oob <- covregrf.obj$predicted.oob
head(pred.oob, 2)

## ---- fig.show='hold', fig.width=5, fig.height=3, fig.align='center'----------
vimp.obj <- vimp(covregrf.obj)
vimp.obj$importance
plot.vimp(vimp.obj)

## -----------------------------------------------------------------------------
partialsig.obj <- significance.test(formula, traindata, params.rfsrc = list(ntree = 200), 
                                    nperm = 10, test.vars = "x3")
partialsig.obj$pvalue

## -----------------------------------------------------------------------------
pred.obj <- predict(covregrf.obj, testdata)
pred <- pred.obj$predicted
head(pred, 2)

## -----------------------------------------------------------------------------
sessionInfo()

