#' Loads a TnSeq/INSeq data set
#'
#' Loads a specified data set from the cafe-map and cafe-annotate tools (or equivalent) and prepares it for analysis using the CAFE R package.
#' The function returns nothing, but creates the following objects that can be used by the user (or other functions): countData, undefCounts, unmappedCounts, countMatrix, geneNames, sampleNames, treatmentClasses, treatments
#'
#' @param inputFileName cafe-annotate output file to use for analysis.
#'
#' @export
loadData = function(inputFileName = "") {
    if (inputFileName == "") {
        ## Bring up file name dialog...
        inputFileName = "all_rep_inseq.txt"
    }
    countData <<- read.table(inputFileName, row.names = 1, header = T, quote = "", sep = "\t")
    countData <<- within(countData, rm("X"))
    undefCounts <<- t(countData["undef",])
    unmappedCounts <<- t(countData["unmapped",])
    countData <<- countData[1:(nrow(countData)-2),]
    countMatrix <<- as.matrix(countData)
    geneNames <<- row.names(countData)
    sampleNames <<- colnames(countData)
    treatmentClasses <<- factor(sub("_[0-9]+$","",sampleNames))
    treatments <<- grep("Adapt",levels(treatmentClasses),ignore.case = TRUE, invert = TRUE, value = TRUE)
    #replicates = factor(sub(".*(_[0-9]+)$","\1",sampleNames))
}
