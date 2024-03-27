#' Returns SWEET hospital size ranges colors vector
#' @return SWEET size ranges colors vector
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
sizeRangesColorsSWEET <- function(){
    sweet_colors <- c("Small" = "khaki3",
                    "Medium" = "tan2", 
                    "Large" = "tomato3")
    return(sweet_colors)
}

#' Classify a numeric column of ages using SWEET ages classification
#' @param ages vector of ages
#' @param pediatric if TRUE, marks non-pediatric patients as Adults
#' @param factors if TRUE, return vector of factors. Default: TRUE
#' @return SWEET age ranges vector
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
ageRangeSWEET <- function(ages, pediatric = FALSE, factors = TRUE){
    # Define ranges
    age_ranges <- list("Not specified" = c(-Inf, -1),
                        "Age 0-6 years" = c(0, 5),
                        "Age 6-12 years" = c(6, 11),
                        "Age 12-18 years" = c(12, 17))
    if(!pediatric){
        age_ranges <- append(age_ranges, list("Age 18-25 years" = c(18, 24),
                        "Age 25-35 years" = c(25,34),
                        "Age 35-45 years" = c(35,44),
                        "Age 45-55 years" = c(45,54),
                        "Age 55-65 years" = c(55,64),
                        "Age 65-75 years" = c(65,74),
                        "Age 75 years or more" = c(75, Inf)))
    }else{
        age_ranges <- append(age_ranges, list("Adult (>18 years)" = c(18,Inf)))
    }

    # Apply ranges
    agesRanged <- unlist(sapply(ages,function(age) {
        rnge <- sapply(seq_along(age_ranges), function(j) {
            if(is.na(age)) return(FALSE)
            return(age >= age_ranges[[j]][1] &&
                    age <= age_ranges[[j]][2])
        })
        rnge_label <- "Not specified"
        if(any(rnge)) rnge_label <- names(age_ranges)[rnge]
        return(rnge_label)
    }))

    if(factors) agesRanged <- factor(agesRanged, levels = names(age_ranges))

    return(agesRanged)
}


#' Classify a column of hospitals using SWEET sizes ranges
#' @param hospitals vector to be checked
#' @param factors if TRUE, return vector of factors. Default: TRUE
#' @return size ranges by SWEET
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
sizeRangeSWEET <- function(hospitals, factors = TRUE){
    # Define Ranges
    sweet_ranges <- list(
        "Small" = c(0, 49),
        "Medium" = c(50, 150),
        "Large" = c(151, Inf)
    )

    # Check sizes
    auxHospitals <- table(hospitals)
    hospitalSizes <- lapply(auxHospitals, function(hsize){
        rnge <- sapply(seq_along(sweet_ranges), function(j) {
            if (is.na(hsize)) {
                return(FALSE)
            }
            return(hsize >= sweet_ranges[[j]][1] &&
                hsize <= sweet_ranges[[j]][2])
        })
        rnge_label <- NA
        if (any(rnge)) rnge_label <- names(sweet_ranges)[rnge]
        return(rnge_label)
    })
    names(hospitalSizes) <- names(auxHospitals)
    hospitalsRanged <- as.vector(unlist(hospitalSizes[hospitals]))

    if(factors) hospitalsRanged<- factor(hospitalsRanged, 
                                    levels = names(sweet_ranges))

    return(hospitalsRanged)
}