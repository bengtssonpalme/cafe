sizeNormalize = function(countDataTable = NULL, undefCountTable = NULL, returnUndef = FALSE) {
    if (is.null(countDataTable)) {
        countDataTable = countData
    }
    if (is.null(undefCountTable)) {
        undefCountTable = undefCounts
    }
    if (returnUndef == FALSE) {
        totalCounts = colSums(as.matrix(countDataTable)) + as.vector(undefCountTable)
        normalizedCountTable = t(t(as.matrix(countDataTable)) / as.numeric(totalCounts))
        return(normalizedCountTable)
    } else {
        totalCounts = colSums(as.matrix(countDataTable)) + as.vector(undefCountTable)
        normalizedCountTable = t(t(as.vector(undefCountTable)) / as.numeric(totalCounts))
        return(normalizedCountTable)
    }
}
