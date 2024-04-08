#' Checks that necessary cols are available to perform classification
#' @param cohort dataframe to be checked
#' @param cnames (Optional) list with columnames relations
#' @return TRUE if GRI algorithm can be applied or FALSE in other cases
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
checkGRI <- function(cohort, cnames = NULL){
    auxNames <- colnames(cohort)
    if(!is.null(cnames)){
        invisible(lapply(seq_along(cnames),function(i){
            if(cnames[[i]] %in% auxNames){
                auxNames[match(cnames[[i]],auxNames)] <<- names(cnames)[i]
            }
        }))
    }

    mandatoryCols <- c("TIR","TBR","TAR","TBRS","TARS")
    return(all(mandatoryCols %in% auxNames))
}


#' Returns, ordered, from most to less severe, all classifier functions 
#' implemented for Glycemia Risk Index (GRI).
#' Note: doi:10.1177/19322968221085273
#' @return vector with ordered allowed class leveles
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
griClassifiers <- function(){
  return(c(isBrownGRI,isRedGRI,isOrangeGRI,isYellowGRI,isGreenGRI))  
}


#' Returns, ordered, from most to less severe, all class levels implemented 
#' @param meta if TRUE, returns also specified metainfo per class
#' @return vector with ordered allowed class leveles
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
griClasses <- function(meta = FALSE){
  dummy <- data.frame(A = numeric(0), B = numeric(0), C = numeric(0),
                      D = numeric(0), E = numeric(0))
  classes <- as.data.frame(do.call(rbind,lapply(griClassifiers(),
    function(classifier){
      res <- classifier(dummy,meta = TRUE)
      return(data.frame(Class = res$value, 
                        Color = res$color,
                        HypoCmin = res$HypoC[1],
                        HypoCmax = res$HypoC[2],
                        HyperCmin = res$HyperC[1],
                        HyperCmax = res$HyperC[2]))
  })))
  classes <- rbind(classes,data.frame(Class = "Unclassified",
                                      Color = "plum",
                                      HypoCmin = 0,
                                      HypoCmax = 100,
                                      HyperCmin = 0,
                                      HyperCmax = 100))
  classes <- rbind(classes,data.frame(Class = "Not allowed",
                                      Color = "plum",
                                      HypoCmin = 0,
                                      HypoCmax = 100,
                                      HyperCmin = 0,
                                      HyperCmax = 100))
  if(!meta) classes <- classes$Class
  return(classes)
}


#' Classifies a dataframe of patients using GRI algorithm 
#' @param df of patients to be classified
#' @param cnames (Optional) list with columnames relations
#' @param type (Optional) select between classify (default) or "extra"
#' @return vector with patients classes
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
griWrapper <- function(df, cnames=NULL, type = "classify"){
  TIRcname <- "TIR"
  TBRcname <- "TBR"
  TBRScname <- "TBRS"
  TARcname <- "TAR"
  TARScname <- "TARS"
  if(!is.null(cnames)){
    TIRcname <- cnames$TIR
    TBRcname <- cnames$TBR
    TBRScname <- cnames$TBRS
    TARcname <- cnames$TAR
    TARScname <- cnames$TARS
  }

  info <- NULL
  if(type == "classify"){
    info <- gri(df, 
                TIRcol = TIRcname, 
                TBRcol = TBRcname,
                TARcol = TARcname,
                TBRScol = TBRScname,
                TARScol = TARScname)

  }else if(type == "extra"){
    info <- calculatesGRI(df, 
                          TIRcol = TIRcname, 
                          TBRcol = TBRcname,
                          TARcol = TARcname,
                          TBRScol = TBRScname,
                          TARScol = TARScname)
  }

  return(info)
}


#' Classifies a dataframe of patients using GRI algorithm 
#' @param df of patients to be classified
#' @param TIRcol TIR info column name
#' @param TBRcol TBR info column name
#' @param TBRScol TBRS info column name
#' @param TARcol TAR info column name
#' @param TARScol TARS info column name
#' @export
#' @return vector with patients classes
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
gri <- function(df, TIRcol = 'TIR', TBRcol = 'TBR', TARcol = 'TAR',
                            TBRScol = 'TBRS', TARScol = 'TARS'){
    givenColnames <- list(TIRcol,TBRcol,TARcol,TBRScol,TARScol)
    innerfunColnames <- c("TIR","TBR","TAR","TBRS","TARS")
    names(givenColnames) <- innerfunColnames
    finalClasses <- rep("Not allowed",nrow(df))

    if(checkGRI(df, givenColnames)){
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

        # Calculates Glycemia Risk Index (GRI)
        mainInfo <- cbind(mainInfo, calculatesGRI(mainInfo))

        # Classify
        classFuncs <- griClassifiers()
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


#' Calculates Glycemia Risk Index (GRI) and its components
#' @param df of patients to be classified
#' @param TIRcol TIR info column name
#' @param TBRcol TBR info column name
#' @param TBRScol TBRS info column name
#' @param TARcol TAR info column name
#' @param TARScol TARS info column name
#' @return dataframe with extra info
#' @export
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
calculatesGRI <- function(df, TIRcol = 'TIR', TBRcol = 'TBR', TARcol = 'TAR',
                            TBRScol = 'TBRS', TARScol = 'TARS'){
    givenColnames <- list(TIRcol,TBRcol,TARcol,TBRScol,TARScol)
    innerfunColnames <- c("TIR","TBR","TAR","TBRS","TARS")
    names(givenColnames) <- innerfunColnames

    auxNAs <- rep(NA,nrow(df))
    toReturn <- data.frame(GRIVal = auxNAs,
                            HypoglycemiaC = auxNAs,
                            HyperglycemiaC = auxNAs)
    if(checkGRI(df,givenColnames)){
        mainInfo <- df[,unlist(givenColnames)]
        colnames(mainInfo) <- innerfunColnames

        toReturn <- as.data.frame(do.call(rbind,lapply(seq(nrow(mainInfo)),function(i){
            tbr <- mainInfo$TBR[i]
            tbrs <- mainInfo$TBRS[i]
            tar <- mainInfo$TAR[i]
            tars <- mainInfo$TARS[i]
            hypo <- NA
            hyper <- NA
            griVal <- NA
            if(!any(is.na(c(tbr,tbrs)))) hypo <- tbrs + 0.8*(tbr - tbrs)
            if(!any(is.na(c(tar,tars)))) hyper <- tars + 0.5*(tar - tars)
            if(!any(is.na(c(hyper,hypo)))) griVal <- 3*hypo + 1.6*hyper
            return(data.frame(GRIVal = griVal,
                            HypoglycemiaC = hypo,
                            HyperglycemiaC = hyper))
        })))
    }

    return(toReturn)
}


#' Checks if patients info given is classified as Green class.
#' @param info dataframe to be checked
#' @param meta if TRUE, add meta entries to result object
#' @return logical vector, if TRUE, this patient is Green class
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
isGreenGRI <- function(info, meta = FALSE){
  # GRI <= 20
  classification <- info$GRIVal <= 20
  classification[is.na(classification)] <- FALSE 
  res <- list(value = "Green", class = classification)
  if(meta){
    res$color <- "forestgreen"
    res$GRIVal <- c(0,20)
    ranges <- calculatesMetaPolygonsGRI(res$GRIVal[1],res$GRIVal[2])
    res$HypoC <- ranges$HypoC
    res$HyperC <- ranges$HyperC
  }
  return(res)
}


#' Checks if patients info given is classified as Yellow class.
#' @param info dataframe to be checked
#' @param meta if TRUE, add meta entry to result object
#' @return logical vector, if TRUE, this patient is Green-Ambar class
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
isYellowGRI <- function(info, meta = FALSE){
  # 20 < GRI <= 40
  classification <- info$GRIVal > 20 & info$GRIVal <= 40
  res <- list(value = "Yellow", class = classification)
  classification[is.na(classification)] <- FALSE 
  if(meta){
    res$color <- "yellow"
    res$GRIVal <- c(20,40)
    ranges <- calculatesMetaPolygonsGRI(res$GRIVal[1],res$GRIVal[2])
    res$HypoC <- ranges$HypoC
    res$HyperC <- ranges$HyperC
  }
  return(res)
}


#' Checks if patients info given is classified as Orange class.
#' @param info dataframe to be checked
#' @param meta if TRUE, add meta entry to result object
#' @return logical vector, if TRUE, this patient is Ambar-Red class
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
isOrangeGRI <- function(info, meta = FALSE){
  # 40 < GRI <= 60
  classification <- info$GRIVal > 40 & info$GRIVal <= 60
  res <- list(value = "Orange", class = classification)
  classification[is.na(classification)] <- FALSE 
  if(meta){ 
    res$color <- "orange"
    res$GRIVal <- c(40,60)
    ranges <- calculatesMetaPolygonsGRI(res$GRIVal[1],res$GRIVal[2])
    res$HypoC <- ranges$HypoC
    res$HyperC <- ranges$HyperC
  }
  return(res)
}


#' Checks if patients info given is classified as Red class.
#' @param info dataframe to be checked
#' @param meta if TRUE, add meta entry to result object
#' @return logical vector, if TRUE, this patient is Red class
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
isRedGRI <- function(info, meta = FALSE){
  # 60 < GRI <= 80
  classification <- info$GRIVal > 60 & info$GRIVal <= 80
  res <- list(value = "Red", class = classification)
  classification[is.na(classification)] <- FALSE 
  if(meta){
    res$color <- "darkred"
    res$GRIVal <- c(60,80)
    ranges <- calculatesMetaPolygonsGRI(res$GRIVal[1],res$GRIVal[2])
    res$HypoC <- ranges$HypoC
    res$HyperC <- ranges$HyperC

  } 
  return(res)
}


#' Checks if patients info given is classified as Brown class.
#' @param info dataframe to be checked
#' @param meta if TRUE, add meta entry to result object
#' @return logical vector, if TRUE, this patient is Brown class
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
isBrownGRI <- function(info, meta = FALSE){
  # GRI > 80
  classification <- info$GRIVal > 80
  res <- list(value = "Brown", class = classification)
  classification[is.na(classification)] <- FALSE 
  if(meta){
    res$color <- "peru"
    res$GRIVal <- c(80,100)
    ranges <- calculatesMetaPolygonsGRI(res$GRIVal[1],res$GRIVal[2])
    res$HypoC <- ranges$HypoC
    res$HyperC <- ranges$HyperC
  } 
  return(res)
}


#' Internal function used to calculate area polygons for each class 
#' @param GRImin minimum GRI of class range
#' @param GRImax maximum GRI of class range
#' @return list with derised polygon corners
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com>
calculatesMetaPolygonsGRI <- function(GRImin, GRImax){
  hypo <- function(gri){return(gri/3)}
  hyper <- function(gri){return(gri/1.6)}
  # Calculates min/max Hypo
  hyporange <- c(hypo(GRImin),hypo(GRImax))
  hyperrange <- c(hyper(GRImin),hyper(GRImax))
  return(list(HypoC = hyporange, HyperC = hyperrange))
}