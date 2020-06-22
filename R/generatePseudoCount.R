generatePseudoCount = function(countDataTable = NULL, undefCountTable = NULL, controlDataTable = NULL, controlUndefTable = NULL, includeAdapt = FALSE) {
    if (is.null(countDataTable)) {
        countDataTable = countData
    }
    if (is.null(undefCountTable)) {
        undefCountTable = undefCounts
    }
    if (is.null(controlDataTable) || is.null(controlUndefTable)) {
        if (includeAdapt) {
            totalCounts = colSums(as.matrix(countDataTable)) + as.vector(undefCountTable)
            pseudoCount = 1 / min(as.numeric(totalCounts + 1))
        } else {
            totalCounts = colSums(as.matrix(countDataTable[,grep("Adapt",sampleNames,ignore.case = T, invert = T)])) + as.vector(undefCountTable[grep("Adapt",sampleNames,ignore.case = T, invert = T)])
            pseudoCount = 1 / min(as.numeric(totalCounts + 1))
        }
    } else {
        allData = cbind(as.matrix(countDataTable),as.matrix(controlDataTable))
        undefData = c(as.numeric(undefCountTable),as.numeric(controlUndefTable))
        totalCounts = colSums(as.matrix(allData)) + as.vector(undefData)
        pseudoCount = 1 / min(as.numeric(totalCounts + 1))
    }
    return(pseudoCount)
}
