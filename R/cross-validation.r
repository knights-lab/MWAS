#' runs cross-validation Random Forests classification
#'
#' @param x A matrix of numeric predictors
#' @param y A factor or logical of responses
#' @param nfolds number of cross-validation folds (nfolds==-1 means leave-one-out)
#' @param folds optional, if not \code{NULL} then uses these fold assignments (one fold index for each row in \code{x})
#' @param verbose Use verbose output
#' @param ... additional parameters for randomForest
#'
#' @return
#'  \item{y}{Observed values}
#'  \item{predicted}{CV predicted values}
#'  \item{probabilities}{CV predicted class probabilities (or NULL if unavailable)}
#'  \item{confusion.matrix}{Confusion matrix (true-by-predicted)}
#'  \item{nfolds}{Number of folds used (or -1 for leave-one-out)}
#'  \item{params}{List of additional parameters, if any}
#'  \item{importances}{Feature-by-fold matrix of importances of features (mean decrease in accuracy) across folds}
#'  \item{final.model}{Final random forests model trained on whole data set}
#' @examples
#' # generate fake data: 50 samples, 20 variables
#' x <- matrix(rnorm(2000),100)
#' rownames(x) <- sprintf('Sample%02d',1:100)
#' colnames(x) <- sprintf('Feature%02d',1:20)
#' # generate fake response variable, function of columns 1-5 plus noise
#' y <- rowSums(sweep(x[,1:5],2,runif(5),'*')) + rnorm(100,sd=.5)
#' # y is now binary response variable
#' y <- factor(y > median(y))
#' names(y) <- rownames(x)
#'
#' # 10-fold cross validation, returning predictions
#' res.rf <- rf.cross.validation(x,y2)
#'
#' # plot importance of top ten variables
#' sorted.importances <- sort(rowMeans(res.rf$importances), decreasing=TRUE)
#' barplot(rev(sorted.importances[1:10]),horiz=TRUE, xlab='Mean Decrease in Accuracy')
#'
#' # Plot classification ROC curve
#' roc <- probabilities.to.ROC(res.rf$probabilities[,2], y2,plot=TRUE)
#'
#' # Report ROC AUC
#' cat('ROC AUC was:',roc$auc,'\n')
#'

"rf.cross.validation" <- function(x, y, nfolds=10, folds=NULL, verbose=FALSE, ...){
    require('randomForest')
    if(nfolds==-1) nfolds <- length(y)
    if(is.null(folds)) folds <- balanced.folds(y,nfolds=nfolds)
    result <- list()
    result$y <- as.factor(y)
    result$predicted <- result$y
    result$probabilities <- matrix(0, nrow=length(result$y), ncol=length(levels(result$y)))
    rownames(result$probabilities) <- rownames(x)
    colnames(result$probabilities) <- levels(result$y)
    result$importances <- matrix(0,nrow=ncol(x),ncol=nfolds)
    result$errs <- numeric(length(unique(folds)))

    # K-fold cross-validation
    for(fold in sort(unique(folds))){
        if(verbose) cat(sprintf('Fold %d...\n',fold))
        foldix <- which(folds==fold)
        model <- randomForest(x[-foldix,], factor(result$y[-foldix]), importance=TRUE, do.trace=verbose, ...)
        newx <- x[foldix,,drop=F]
        if(length(foldix)==1) newx <- matrix(newx,nrow=1)
        result$predicted[foldix] <- predict(model, newx)
        probs <- predict(model, newx, type='prob')
        result$probabilities[foldix,colnames(probs)] <- probs
        result$errs[fold] <- mean(result$predicted[foldix] != result$y[foldix])
        result$importances[,fold] <- model$importance[,'MeanDecreaseAccuracy']
    }
    rownames(result$importances) <- colnames(x)
    result$err <- mean(result$predicted != result$y)
    result$nfolds <- nfolds
    result$params <- list(...)
    result$confusion.matrix <- t(sapply(levels(y), function(level) table(result$predicted[y==level])))

    # train one final model on the whole data set
    result$final.model <- randomForest(x,result$y)

    return(result)
}

#' runs cross-validation of LARS regression
#'
#' @param x A matrix of numeric predictors
#' @param y A factor or logical of responses
#' @param nfolds number of cross-validation folds (nfolds==-1 means leave-one-out)
#' @param folds optional, if not \code{NULL} then uses these fold assignments (one fold index for each row in \code{x})
#' @param verbose Use verbose output
#' @param ... additional parameters for lars
#'
#' @return:
#'  \item{y}{Observed values}
#'  \item{predicted}{CV predicted values}
#'  \item{fractions}{Vector of LASSO fractions used across folds}
#'  \item{best.fraction}{Best LASSO fraction used}
#'  \item{MSE}{Mean squared error}
#'  \item{RMSE}{Root mean squared error}
#'  \item{nfolds}{Number of folds used (or -1 for leave-one-out)}
#'  \item{params}{List of additional parameters, if any}
#'  \item{coefficients}{Feature-by-fold matrix of coefficients of features across folds}
#'  \item{final.model}{Final lars model trained on whole data set}
#' @examples
#' # generate fake data: 50 samples, 20 variables
#' x <- matrix(rnorm(2000),100)
#' rownames(x) <- sprintf('Sample%02d',1:100)
#' colnames(x) <- sprintf('Feature%02d',1:20)
#' # generate fake response variable, function of columns 1-5 plus noise
#' y <- rowSums(sweep(x[,1:5],2,runif(5),'*')) + rnorm(100,sd=.5)
#' names(y) <- rownames(x)
#'
#' # 10-fold cross validation with predictions
#' res.lars <- lars.cross.validation(x,y)
#' # plot predictions
#' plot(res.lars$y, res.lars$predicted, xlab="Observed", ylab="Predicted (hold-out)")
#'
#' # correlation
#' cat('Pearson correlation of predictions with truth?\n')
#' print(cor.test(res.lars$y, res.lars$predicted))
#'
#' # plot fraction of times each variable was included in a fold
#' sorted.importances <- sort(rowMeans(res.lars$coefficients > 0), decreasing=TRUE)
#' # bar plot of top ten variables
#' par(mar=c(4.5,6,2,1),las=2) # larger margin on left, perpendicular axis labels
#' barplot(rev(sorted.importances[1:10]),horiz=TRUE, xlab='Frequency of selection')
#'

"lars.cross.validation" <- function(x, y, nfolds=10, folds=NULL, verbose=FALSE, ...){
    require('lars')
    if(nfolds==-1) {
        nfolds <- length(y)
        folds <- 1:length(y)
    } else{
        folds <- sample(rep(1:nfolds,times=ceiling(length(y)/nfolds))[1:length(y)])
    }
    result <- list()
    result$y <- as.numeric(y)
    result$predicted <- result$y
    result$coefficients <- matrix(0,nrow=ncol(x),ncol=nfolds)
    result$fractions <- numeric(length(y))

    # K-fold cross-validation
    for(fold in sort(unique(folds))){
        if(verbose) cat(sprintf('Fold %d...\n',fold))
        foldix <- which(folds==fold)

        # do cv.lars on the training set to choose the best penalty
        cv.res <- cv.lars(x[-foldix,], result$y[-foldix], plot.it=FALSE, ...)
        threshold <- cv.res$index[which.min(cv.res$cv)]
        res <- lars(x[-foldix,], result$y[-foldix])

        # predict on test set
        newx <- x[foldix,,drop=F]
        yhat <- predict(res, newx, mode='fraction',type='fit',s=threshold)$fit

        # extract results
        result$predicted[foldix] <- yhat
        result$coefficients[,fold] <- coef(res,mode='fraction',s=threshold)
        result$fractions[fold] <- threshold
    }
    rownames(result$coefficients) <- colnames(x)
    result$MSE <- mean((result$predicted - result$y)^2)
    result$RMSE <- sqrt(result$MSE)
    result$nfolds <- nfolds
    result$params <- list(...)
    result$best.fraction <- mean(result$fractions)

    # train one final model on the whole data set
    result$final.model <- lars(x,result$y)
    return(result)
}


#' Get balanced folds where each fold has close to overall class ratio. This only works for non-numeric inputs.
#'
#' @param y Vector of classes; should be a factor, logical, or character
#' @param nfolds Number of folds. -1 means leave-one-out
#' @return Numeric vector of fold indices, one for each element of \code{y}
"balanced.folds" <- function(y, nfolds=10){
    if(is.numeric(y)) stop("MWAS balanced.folds function only accepts non-numeric inputs.")
    folds = rep(0, length(y))
    y <- as.factor(y)
    classes = levels(y)
    # size of each class
    Nk = table(y)
    # -1 or nfolds = len(y) means leave-one-out
    if (nfolds == -1 || nfolds == length(y)){
        invisible(1:length(y))
    }
    else{
    # Can't have more folds than there are items per class
    nfolds = min(nfolds, max(Nk))
    # Assign folds evenly within each class, then shuffle within each class
        for (k in 1:length(classes)){
            ixs <- which(y==classes[k])
            folds_k <- rep(1:nfolds, ceiling(length(ixs) / nfolds))
            folds_k <- folds_k[1:length(ixs)]
            folds_k <- sample(folds_k)
            folds[ixs] = folds_k
        }
        invisible(folds)
    }
}




#' Performs ROC analysis (and plot) on probability vector
#'
#' @param p Vector or probabilities of TRUE
#' @param y Logical, factor, or character of TRUE/FALSE
#' @param step.size Increments at which to calculate false and true positive rates
#' @param plot If \code{TRUE} then make a simple ROC curve plot.
#' @param ... Additional parameters to pass to \code{plot} function
#' @return
#'  \item{fprs}{Vector of false positive rates}
#'  \item{tprs}{Vector of true positive rates}
#'  \item{auc}{Area under the curve}
"probabilities.to.ROC" <- function(p,y,step.size=0.01, plot=TRUE,...){
    require('flux')
    y <- as.logical(y)
    fprs <- sapply(seq(1,0,-abs(step.size)), function(xx) mean(p[!y] >= xx))
    tprs <- sapply(seq(1,0,-abs(step.size)), function(xx) mean(p[y] >= xx))
    tprs[fprs > tprs] <- fprs[fprs > tprs]
    auc.res <- auc(fprs, tprs)
    plot(fprs,tprs,type='l', xlab='False Positive Rate',ylab='True Positive Rate',...)

    return(list(fprs=fprs, tprs=tprs, auc=auc.res))
}
