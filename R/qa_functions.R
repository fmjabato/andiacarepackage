#' Checks a patient(s) entry in order to check if complies QA tests
#' @param data to be checked
#' @param algorithms that will be executed
#' @param remove.ends if TRUE, 0% and 100% will be removed
#' @return TRUE if everything is ok or FALSE in other cases
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
checkEntryQA <- function(data, algorithms = c("Andiacare", "GRI"),
                        remove.ends = FALSE){
    # Mandatory columns
    toCheck <- c()
    if("Andiacare" %in% algorithms) toCheck <- c("TIR","TBR","TAR")
    if("GRI" %in% algorithms) toCheck <- c("TIR","TBR","TAR","TBRS","TARS")
    # Check main columns
    checks <- as.data.frame(do.call(cbind,lapply(toCheck,function(col){
        toReturn <- rep(TRUE, nrow(data))
        if(col %in% colnames(data)){
            boolNAs <- is.na(data[, col])
            boolNumeric <- !is.numeric(data[, col])
            toReturn <- boolNAs | boolNumeric
            if(remove.ends){
                boolEnds <- data[, col] <= 0 | data[, col] >= 100
                toReturn <- toReturn | boolEnds
            }
        }
        return(!toReturn)
    })))
    colnames(checks) <- toCheck
    # Special case one is zero and other not 100 
    if(all(c("TAR", "TBR") %in% toCheck)){
        checks$TXR <- checks$TAR | checks$TBR
        checks <- checks[,-match(c("TAR", "TBR"), colnames(checks))]
    }
    return(rowSums(checks) == ncol(checks))
}


#' Correct missing TXR column if possible
#' @param df data frame to be checked
#' @param tirCol (Optional) TIR colname
#' @param tbrCol (Optional) TBR colname
#' @param tarCol (Optional) TAR colname
#' @export
#' @return df corrected
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
inferTXR <- function(df, tirCol = "TIR", tbrCol = "TBR", tarCol = "TAR"){
    txrCols <- c(tirCol, tbrCol, tarCol)
    names(txrCols) <- c("TIR","TBR","TAR")
    if(sum(txrCols %in% colnames(df)) == length(txrCols) - 1){
        toAdd <- which(!txrCols %in% colnames(df))
        df[,names(txrCols)[toAdd]] <- rep(NA, nrow(df))
    }

    fixedDF <- df
    if(all(txrCols %in% colnames(df))){
        checks <- data.frame(TIR = is.na(df$TIR),
                            TBR = is.na(df$TBR),
                            TAR = is.na(df$TAR))
        fixable <- which(rowSums(checks) == 1)
        if(length(fixable) > 0){
            invisible(lapply(fixable,function(indx){
                # Prepare columns
                fixableCol <- which(unlist(checks[indx,]))
                correctCols <- txrCols[-fixableCol]
                fixableCol <- txrCols[fixableCol]
                # Fixe
                fixedValue <- 100 - (df[indx,correctCols[1]] + 
                                    df[indx,correctCols[2]])
                fixedDF[indx,fixableCol] <<- fixedValue
            }))
        }
    }
    return(fixedDF)
}
inferTXR(head(df))