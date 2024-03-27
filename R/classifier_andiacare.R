#' Checks that necessary cols are available to perform classification
#' @param cohort dataframe to be checked
#' @param cnames (Optional) list with columnames relations
#' @return TRUE if Andiacare algorithm can be applied or FALSE in other cases
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
checkAndiacare <- function(cohort, cnames = NULL){
    auxNames <- colnames(cohort)
    if(!is.null(cnames)){
        invisible(lapply(seq_along(cnames),function(i){
            if(cnames[[i]] %in% auxNames){
                auxNames[match(cnames[[i]], auxNames)] <<- names(cnames)[i]
            }
        }))
    }

    mandatoryCols <- c("TIR", "TBR", "TAR")
    return(all(mandatoryCols %in% auxNames))
}


#' Returns, ordered, from most to less severe, all classifier functions 
#' implemented for Andiacare.
#' @return vector with ordered allowed class leveles
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
andiacareClassifiers <- function(){
  return(c(isRedAndiacare, isOrangeAndiacare, isYellowAndiacare,
            isGreenAndiacare))
}


#' Returns, ordered, from most to less severe, all class levels implemented 
#' @param meta if TRUE, returns also specified metainfo per class
#' @return vector with ordered allowed class leveles
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
andiacareClasses <- function(meta = FALSE){
  dummy <- data.frame(A = numeric(0), B = numeric(0))
  classes <- as.data.frame(do.call(rbind,lapply(andiacareClassifiers(),
    function(classifier){
      res <- classifier(dummy,meta = TRUE)
      return(data.frame(Class = res$value,
                        Color = res$color))
  })))
    classes <- rbind(classes,data.frame(Class = "Unclassified",
                                      Color = "violet"))
    classes <- rbind(classes,data.frame(Class = "Not allowed",
                                      Color = "grey"))
    if(!meta) classes <- classes$Class
  return(classes)
}


#' Classifies a dataframe of patients using Andiacare algorithm 
#' @param df of patients to be classified
#' @param cnames (Optional) list with columnames relations
#' @param type (Optional) select between classify (default) or "extra"
#' @return vector with patients classes
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
andiacareWrapper <- function(df, cnames=NULL, type = "classify"){
  TIRcname <- "TIR"
  TBRcname <- "TBR"
  TARcname <- "TAR"
  if(!is.null(cnames)){
    TIRcname <- cnames$TIR
    TBRcname <- cnames$TBR
    TARcname <- cnames$TAR
  }

  info <- NULL
  if(type == "classify"){
    info <- andiacare(df, TIRcol = TIRcname,
                        TBRcol = TBRcname,
                        TARcol = TARcname)
  } #else if(type == "extra") info <- NULL
  return(info)
}


#' Classifies a dataframe of patients using Andiacare algorithm 
#' @param df of patients to be classified
#' @param TIRcol TIR info column name
#' @param TBRcol TBR info column name
#' @param TARcol TAR info column name
#' @return vector with patients classes
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
andiacare <- function(df, TIRcol = 'TIR', TBRcol = 'TBR', TARcol = 'TAR'){
    givenColnames <- list(TIRcol,TBRcol,TARcol)
    innerfunColnames <- c("TIR","TBR","TAR")
    names(givenColnames) <- innerfunColnames
    finalClasses <- rep("Not allowed",nrow(df))

    if(checkAndiacare(df, givenColnames)){
        finalClasses <- rep("Unclassified",nrow(df))
        mainInfo <- df[,unlist(givenColnames)]
        colnames(mainInfo) <- innerfunColnames
        alreadyClassified <- rep(FALSE, nrow(mainInfo))
        # Handle erroneous reads
        erroneousReadsIndex <- which(mainInfo$TIR < 1 &
                                    mainInfo$TBR < 1)
        if(length(erroneousReadsIndex) > 0){
        alreadyClassified[erroneousReadsIndex] <- TRUE # Unclassified
        }

        # Classify
        classFuncs <- andiacareClassifiers()
        classStats <- sapply(classFuncs,function(classifier){
        toClassify <- which(!alreadyClassified)
        if(length(toClassify) > 0 ){
            classification <- classifier(mainInfo[toClassify,])
            auxIndex <- which(classification$class)
            if(length(auxIndex) > 0){
            finalClasses[toClassify[auxIndex]] <<- rep(classification$value,
                                                        length(auxIndex))
            alreadyClassified[toClassify] <<- alreadyClassified[toClassify] | 
                                                classification$class
            }
        }
        })
    }
    return(finalClasses)
}


#' Checks if patients info given is classified as Green class.
#' @param info dataframe to be checked
#' @param meta if TRUE, add meta entries to result object
#' @return logical vector, if TRUE, this patient is Green class
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
isGreenAndiacare <- function(info, meta = FALSE){
  # TIR > 70% and TBR < 4% and TAR < 25%
  classification <- info$TIR > 70 & info$TBR < 4 & info$TAR < 25
  classification[is.na(classification)] <- FALSE
  res <- list(value = "Green", class = classification)
  if(meta){
    res$color <- "forestgreen"
  }
  return(res)
}


#' Checks if patients info given is classified as Yellow class.
#' @param info dataframe to be checked
#' @param meta if TRUE, add meta entry to result object
#' @return logical vector, if TRUE, this patient is Green-Ambar class
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
isYellowAndiacare <- function(info, meta = FALSE){
  # 40% < TIR <= 70% or 4% <= TBR < 11% or 25 <= TAR < 50
  classification <- (info$TIR > 40 & info$TIR <= 70) |
                    (info$TBR >= 4 & info$TBR < 11) |
                    (info$TAR >= 25 & info$TAR < 50)
  res <- list(value = "Yellow", class = classification)
  classification[is.na(classification)] <- FALSE
  if(meta){
    res$color <- "yellow"
  }
  return(res)
}


#' Checks if patients info given is classified as Orange class.
#' @param info dataframe to be checked
#' @param meta if TRUE, add meta entry to result object
#' @return logical vector, if TRUE, this patient is Ambar-Red class
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
isOrangeAndiacare <- function(info, meta = FALSE){
  # 25% <= TIR < 40% or 11% <= TBR <= 20% or 50 <= TAR < 75
  classification <- (info$TIR >= 25 & info$TIR < 40) |
                    (info$TBR >= 11 & info$TBR <= 20) |
                    (info$TAR >= 50 & info$TAR < 75)
  res <- list(value = "Orange", class = classification)
  classification[is.na(classification)] <- FALSE 
  if(meta){ 
    res$color <- "orange"
  }
  return(res)
}


#' Checks if patients info given is classified as Red class.
#' @param info dataframe to be checked
#' @param meta if TRUE, add meta entry to result object
#' @return logical vector, if TRUE, this patient is Red class
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
isRedAndiacare <- function(info, meta = FALSE){
  # TIR < 25% or TBR > 20% or 75 <= TAR
  classification <- info$TIR < 25 | info$TBR > 20 | info$TAR >= 75
  res <- list(value = "Red", class = classification)
  classification[is.na(classification)] <- FALSE 
  if(meta){
    res$color <- "orangered3"
  } 
  return(res)
}


#' Checks if patients info given is classified as Blue flag.
#' @param info dataframe to be checked
#' @param readsCol mean read column name
#' @param thr threshold to marks as blue flag
#' @param logical if TRUE returns logical values, if FALSE returns labels
#' @return logical vector, if TRUE, this patient is Blue flagged
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
isBlueAndiacare <- function(info, readsCol = "MeanReads", thr = 4,
                            logical = TRUE){
  blueFlag <- rep(NA,nrow(info))
  if(readsCol %in% colnames(info)){
    if(grepl("^[0-9]*$",info[1,readsCol])){
      blueFlag <- info[,readsCol] < thr
    }    
  }
  if(!logical){
    blueFlag[blueFlag == TRUE] <- "Bad ussage (Blue Flag)"
    blueFlag[blueFlag == "FALSE"] <- "Correct usage"
  }
  return(blueFlag)
}