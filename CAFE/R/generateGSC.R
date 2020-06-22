#' Generate a Gene Set Collection
#'
#' Loads a gene set collection from a tab-separated. The function will use the specified columns, by default column 1 and 3. The first column will be assumed to be gene names and the other column will be assumed to contain gene sets, e.g. pathways. By default, no header row is assumed in the input file.
#'
#' @param file Tab-separated table containing associations between genes and, e.g., pathways.
#' @param columns A pair of columns to be included in the GSC (1 and 3 by default).
#' @param header If TRUE, the table file is assumed to have a header line before the data.
#' @return Returns a gene set collection (GSC) to be used with the genesetAnalysis function.
#'
#' @export
generateGSC = function(file, columns = c(1,3), header = FALSE) {
    mappingTable = as.matrix(read.table(file, sep = "\t", quote = "", header = header))
    geneNames = unique(mappingTable[,columns[1]])
    pathways = unique(mappingTable[,columns[2]])
    pathwayList = list()
    #GSCobject = list()
    for (i in 1:nrow(mappingTable)) {
        gene = mappingTable[i,columns[1]]
        pathway = mappingTable[i,columns[2]]
        pathwayList[[`pathway`]] = c(pathwayList[[`pathway`]],as.character(gene))
    }
    #GSCobject$gsc = pathwayList
    #GSCobject$addinfo = "none"
    return(pathwayList)
}
