#' Returns allowed classification algorithms
#' @return list with allowed algorithms and its classifiers
#' @export
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com
allowedDiabetesClassifiers <- function(){
  return(list(
    Andiacare = list(Classifier = andiacare,
                     Classes = andiacareClasses,
                     Wrapper = andiacareWrapper,
                     Extra = NULL),
    GRI = list(Classifier = gri,
               Classes = griClasses,
               Wrapper = griWrapper,
               Extra = calculatesGRI)
    ))
}


#' Classifies a patient using a given algorithm
#' @param patient dataframe with patient(s) to be classified
#' @param algorithm algorithm to be used
#' @param cnames (Optional) list with columnames needed to clasify. If its NULL,
#'        default algorithm colnames will be used
#' @param type (Optional) select between classify (default) or (extra)
#' @return vector with classes or NULL if classifier is not available
#' @export
#' @author Fernando Moreno Jabato <jabato(at)uma(dot)com
classifyPatient <- function(patient, algorithm = "Andiacare", cnames = NULL, 
                            type = "classify"){
  classes <- NULL
  classifiers <- allowedDiabetesClassifiers()
  if(!algorithm %in% names(classifiers)){
    warning(paste0("Algorithm specified (",algorithm,
      ") is not allowed. NULL will be returned."))
  }else{
      classes <- classifiers[[algorithm]]$Wrapper(df = patient, 
                                                  cnames = cnames, 
                                                  type = type)
  }
  return(classes)
}


#' Checks if an algorithm can generate extra info
#' @param algorithm to be checked
#' @return TRUE if algorithm has extra or false in other cases
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
hasExtra <- function(algorithm){
  algs <- allowedDiabetesClassifiers()
  check <- FALSE
  if(algorithm %in% names(algs)){
    check <- !is.null(algs[[algorithm]]$Extra)
  }
  return(check)
}