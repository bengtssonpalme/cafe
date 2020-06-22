#' Compares fitness coefficients between multiple conditions
#'
#' This function generates a fitCofComp object, comparing any number of test conditions to the same control condition.
#'
#' @param control The name of the control condition that the test condition should be compared to.
#' @param treatmentList A list of treatments (in array format) that should be compared to the control condition.
#' @param correction Method for correcting for uneven count distribution between samples (can be "undefined", "median" or "none").
#' @return Returns a fitCofComp object (a matrix with fitness coefficients for every gene and replicate in each tested condition).
#'
#' @export
fitCofComp = function(control, treatmentList, dataTable = NULL, undefCountTable = NULL, correction = "undefined") {
    if (is.null(dataTable)) {
        dataTable = countData
    }
    if (is.null(undefCountTable)) {
        undefCountTable = undefCounts
    }

    controlData = dataTable[,grep(paste("^",control,"_[0-9]+$",sep = ""),sampleNames)]
    fullCofTable = matrix(0,nrow(controlData),1)
    for (t in 1:length(treatmentList)) {
        testCondition = treatmentList[t]
        fitCofsTreatment = fitCof(testCondition, control, dataTable, undefCountTable, correction)
        fullCofTable = cbind(fullCofTable, fitCofsTreatment)
    }
    fullCofTable = fullCofTable[,-1]
    return(fullCofTable)
}
