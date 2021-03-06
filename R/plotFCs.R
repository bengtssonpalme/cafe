#' Create a scatter plot of how genes compare between two (or more) conditions
#'
#' This function generates a scatter plot of fitness coefficients from one or more objects created with the fitCof function.
#' The first condition in the list of fitness coefficient objects will be treated as the control condition.
#'
#' @param fitCofList A list of fitness coefficient objects generated by the fitCof function.
#' @param colorList A list of colors to use for the testing conditions.
#' @param conditionA Label for the control condition axis.
#' @param conditionB Label for the test conditions axis.
#' @param lim The limits of the x and y axes.
#' 
#' @export
plotFCs = function(fitCofList, colorList = NULL, conditionA = "Fitness coefficient", conditionB = "Fitness coefficient", lim = c(-11,11)) {
    fc_ctrl = fitCofList[[1]]
    if (max(fc_ctrl[,3]) > 1 || min (fc_ctrl[,3]) < 0) {
        runRanks = TRUE
    } else {
        runRanks = FALSE
    }
    if (is.null(colorList)) {
        colorList = 1:length(fitCofList)
    }

    if (runRanks) {
        fc_ctrl = rankByP(fitCofList[[1]],toplist = FALSE)
    }
    
    plot(fc_ctrl[,1], fc_ctrl[,1], type = "n", ylim = lim, xlim = lim, xlab = conditionA, ylab = conditionB)
    for (i in 2:length(fitCofList)) {
        if (runRanks) {
            fc_test = rankByP(fitCofList[[i]],toplist = FALSE)
        } else {
            fc_test = fitCofList[[i]]
        }
        points(fc_ctrl[,1], fc_test[,1], col = colorList[i-1], cex = 0.2)
    }
    for (i in 2:length(fitCofList)) {
        if (runRanks) {
            fc_test = rankByP(fitCofList[[i]],toplist = FALSE)
        } else {
            fc_test = fitCofList[[i]]
        }
        points(fc_ctrl[fc_test[,3] < 0.05,1], fc_test[fc_test[,3] < 0.05,1], col = colorList[i-1], cex = 0.4)
    }
    for (i in 2:length(fitCofList)) {
        xlm = lm(fitCofList[[i]][,1] ~ fc_ctrl[,1])
        abline(xlm, lty = 2, col = colorList[i-1])
    }
}
