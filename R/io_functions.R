#' Exports a cohort set adding metadata info
#' @param df dataframe with classification
#' @param outfile output file
#' @param sourceDate string with original dataset date
#' @param analysisDate string with analysis date
#' @export
#' @return void
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
writeClassificationTable <- function(df, outfile, sourceDate = NULL,
                                    analysisDate = NULL){
    # Prepare data
    metadata <- "#"
    if(!is.null(sourceDate)){
        metadata <- paste0(metadata, " Source: ", sourceDate, ";")
    }
    if(!is.null(analysisDate)){
        metadata <- paste0(metadata, " Analysis: ", analysisDate)
    }

    # Export
    write(metadata, file = outfile)
    write.table(df, file = outfile, sep = ",", quote = FALSE, col.names = TRUE,
                row.names = FALSE, append = TRUE)
}