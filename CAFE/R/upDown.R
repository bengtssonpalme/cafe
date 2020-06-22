#' Displays the number of genes with positive and negative fitness coefficients
#'
#' This function shows the number of genes with a significantly positive and negative fitness coefficient between a test condition and the control condition. Most of the options are the same as for the fitCof function.
#' By default, the significance threshold is set to 0.05. This function uses the uncorrected p-values.
#' The upDown function can also draw a plot showing the fitness coefficients of each gene in the dataset, ranked by p-value, by setting drawPlot to TRUE.
#' 
#' @param testCondition The name of the testing condition.
#' @param control The name of the control condition that the test condition should be compared to.
#' @param correction Method for correcting for uneven count distribution between samples (can be "undefined", "median" or "none").
#' @param p The p-value cutoff to be included in the significance count.
#' @param drawPlot If TRUE, the function draws a plot showing the fitness coefficients of each gene, ranked by p-value.
#' @return Returns three numbers: the number of genes with a positive fitness coefficient, the number of genes with a negative fitness coefficient, and the expected number of genes with a significant positive and negative fitness coefficient based on the p-value cutoff and the number of genes. 
#' 
#' @export
upDown = function(testCondition, control, p = 0.05, dataTable = NULL, undefCountTable = NULL, correction = "undefined", drawPlot = FALSE) {
    if (is.null(dataTable)) {
        dataTable = countData
    }
    if (is.null(undefCountTable)) {
        undefCountTable = undefCounts
    }
    fitCofs = fitCof(testCondition,control, dataTable, undefCountTable, correction)
    rankedList = rankByP(fitCofs)
    if (drawPlot == TRUE) {
        plot(rankedList[,1], ylab = "Selection coefficient", main = paste(testCondition,"vs",control))
    }
    up = sum(rankedList[rankedList[,3] <= p,1] > 0)
    down = sum(rankedList[rankedList[,3] <= p,1] < 0)
    expected = floor(p * nrow(dataTable) / 2)
    return(c(up,down,expected))
}
