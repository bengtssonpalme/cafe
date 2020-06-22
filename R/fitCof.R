#' Generate fitness coefficients for a pair of conditions (test vs. control)
#'
#' This function generates a fitness coefficient object comparing a test condition and a control condition.
#'
#' @param testCondition The name of the testing condition.
#' @param control The name of the control condition that the test condition should be compared to.
#' @param correction Method for correcting for uneven count distribution between samples (can be "undefined", "median" or "none").
#' @return Returns a fitness coefficient object (a matrix with a fitness coefficient for every gene and replicate).
#'
#' @export
fitCof = function(testCondition, control, dataTable = NULL, undefCountTable = NULL, correction = "undefined") {
    if (is.null(dataTable)) {
        dataTable = countData
    }
    if (is.null(undefCountTable)) {
        undefCountTable = undefCounts
    }
    testData = dataTable[,grep(paste("^",testCondition,"_[0-9]+$",sep = ""),sampleNames)]
    controlData = dataTable[,grep(paste("^",control,"_[0-9]+$",sep = ""),sampleNames)]
    undefTest = undefCountTable[grep(paste("^",testCondition,"_[0-9]+$",sep = ""),sampleNames)]
    undefControl = undefCountTable[grep(paste("^",control,"_[0-9]+$",sep = ""),sampleNames)]
    pseudoCount = generatePseudoCount(testData, undefTest, controlData, undefControl)

    normData = sizeNormalize(dataTable, undefCountTable)

    totalCounts = colSums(as.matrix(countData)) + as.vector(undefCounts)
    normUndef = as.vector(undefCounts) / as.numeric(totalCounts)
    
    testData = normData[,grep(paste("^",testCondition,"_[0-9]+$",sep = ""),sampleNames)]
    controlData = normData[,grep(paste("^",control,"_[0-9]+$",sep = ""),sampleNames)]
    undefTest = normUndef[grep(paste("^",testCondition,"_[0-9]+$",sep = ""),sampleNames)]
    undefControl = normUndef[grep(paste("^",control,"_[0-9]+$",sep = ""),sampleNames)]

    testData = testData + pseudoCount
    controlData = controlData + pseudoCount

    #print(apply(testData,2,median))
    #print(apply(controlData,2,median))
    
    if (ncol(testData) != ncol(controlData)) {
        stop("Numbers of replicates does not match between conditions!")
    }
    fitCofs = matrix(0,nrow(controlData),ncol(controlData))
    for (r in 1:ncol(controlData)) {
        if (correction == "undefined") {
            fitCofs[,r] = ((as.numeric(testData[,r]) / as.numeric(controlData[,r])) / (as.numeric(undefTest[r]) / as.numeric(undefControl[r])))
        }
        if (correction == "median") {
            fitCofs[,r] = ((as.numeric(testData[,r]) / as.numeric(controlData[,r])) / (median(testData[,r]) / median(controlData[,r])))
        }
        if (correction == "none") {
            fitCofs[,r] = ((as.numeric(testData[,r]) / as.numeric(controlData[,r])))
        }
        #print(dim((as.numeric(testData[,r]) / as.numeric(controlData[,r]))))
        #print(dim((undefTest[r] / undefControl[r])))
        #print(median(testData[,r]))
        #print(median(controlData[,r]))
        #print(undefTest[r])
        #print(undefControl[r])
        
        #print(-log2(median(as.numeric(testData[,r]) / as.numeric(controlData[,r])) / (undefTest[r] / undefControl[r])))
    }
    row.names(fitCofs) = geneNames
    colnames(fitCofs) = colnames(testData)
    return(-log2(fitCofs))
}
