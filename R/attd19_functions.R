#' Checks a lower/greater condition for a numeric vector
#' @param values vector to be checked
#' @param target to be checked
#' @param lower if TRUE, checks if values are lower than target
#' @param equals if TRUE, checks if values are X or equal than target
#' @return TRUE if complains or FALSE in other cases
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
generalNumericVectorChecker <- function(values, target, lower = TRUE, 
    equals = FALSE) {
    checks <- sapply(values, function(v) {
        if (is.na(v)) return(NA)
        if (!is.numeric(v)) return(FALSE)
        if(lower & equals) return(v <= target)
        if(lower & !equals) return(v < target)
        if(!lower & equals) return(v >= target)
        if(!lower & !equals) return(v > target)
        return(NA) # Should be unreachable
    })
    return(checks)

}

#' Checks TIR objective for ATTD19 consensus of TIR > 70% for
#' DM1 patients
#' @param tir vector of TIR values
#' @return TRUE if complains or FALSE in other cases
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
attd19TIRTargetDM1 <- function(tir){
    return(generalNumericVectorChecker(tir, 70, FALSE, FALSE))
}


#' Checks TBR objective for ATTD19 consensus of TBR < 4% for
#' DM1 patients
#' @param tbr vector of TBR values
#' @return TRUE if complains or FALSE in other cases
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
attd19TBRTargetDM1 <- function(tbr){
    return(generalNumericVectorChecker(tbr, 4, TRUE, FALSE))
}


#' Checks TBRS objective for ATTD19 consensus of TBRS <= 1% for
#' DM1 patients
#' @param tbrs vector of TBRS values
#' @return TRUE if complains or FALSE in other cases
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
attd19TBRSTargetDM1 <- function(tbrs){
    return(generalNumericVectorChecker(tbrs, 1, TRUE, TRUE))
}


#' Checks TAR objective for ATTD19 consensus of TAR < 25% for
#' DM1 patients
#' @param tar vector of TAR values
#' @return TRUE if complains or FALSE in other cases
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
attd19TARTargetDM1 <- function(tar){
    return(generalNumericVectorChecker(tar, 25, TRUE, FALSE))
}


#' Checks TARS objective for ATTD19 consensus of TARS < 5% for
#' DM1 patients
#' @param tars vector of TARS values
#' @return TRUE if complains or FALSE in other cases
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
attd19TARSTargetDM1 <- function(tars){
    return(generalNumericVectorChecker(tars, 5, TRUE, FALSE))
}


#' Checks CV objective for ATTD2019 DM1 consensus of CV < 36%
#' @param cv vector of CV values
#' @return TRUE if complains or FALSE in other cases
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
attd19CVTargetDM1 <- function(cv){
    return(generalNumericVectorChecker(cv, 36, TRUE, FALSE))
}


#' Checks if Glucose MEAN and SD complains ATTD2019 DM1 consesun of 
#' Mean < 154 mg/dl and SD < 29%
#' @param means vector of Mean Glucose (mg/dl)
#' @param sd vector of Standar Deviation of patients (%)
#' @return TRUE if complains or FALSE in other cases
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
attd19GlucoseTargetDM1 <- function(means, sd){
    check_means <- generalNumericVectorChecker(means, 154, TRUE, FALSE)
    check_sd <- generalNumericVectorChecker(sd, 29, TRUE, FALSE)
    return( check_means & check_sd)
}


#' Check ATTD19 targets and returns a table with checks
#' @param df to be checked (must contain normalized names)
#' @return dataframe with checks
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
andiacareATTDTargets <- function(df) {
    checks <- data.frame(Dummy = rep(NA, nrow(df)))
    auxN <- colnames(df)
    if("TARS" %in% auxN) checks$ATTD19_TARS <- attd19TARSTargetDM1(df$TARS)
    if("TAR" %in% auxN) checks$ATTD19_TAR <- attd19TARTargetDM1(df$TAR)
    if("TIR" %in% auxN) checks$ATTD19_TIR <- attd19TIRTargetDM1(df$TIR)
    if("TBR" %in% auxN) checks$ATTD19_TBR <- attd19TBRTargetDM1(df$TBR)
    if("TBRS" %in% auxN) checks$ATTD19_TBRS <- attd19TBRSTargetDM1(df$TBRS)
    checks <- checks[,-1]
    allfun <- function(x){all(x,na.rm = TRUE)}
    if(ncol(checks) > 1) checks$AllTargets <- apply(checks,1,allfun)
    return(checks)
}