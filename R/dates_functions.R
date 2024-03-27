#' Calculates age from birthdate
#' @param birthdate date
#' @param format (optional) date format
#' @return age
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
getAge <- function(birthdate, format = "%d-%m-%Y"){
    if(is.na(birthdate)) return(NA)
    bd <- as.Date(birthdate, format = format)
    days <- as.numeric(difftime(Sys.Date(),bd, units="days"))
    age <- floor(days/365)
    return(age)
}

#' Calculates debut related stats and return a dataframe with them
#' @param df with Debut column
#' @param debutCol (optional) debut column name. Defualt: 'Debut'
#' @param birthdateCol (optional) birthdate column name. Defualt: 'Birthdate'
#' @param format (optional) date format
#' @return dataframe with debut stats
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
getDebutStats <- function(df, debutCol = "Debut", format = "%d-%m-%Y"){
    toReturn <- NULL
    if(debutCol %in% colnames(df)){
        debut <- as.Date(df[,debutCol], format = format)
        illness_days <- as.numeric(difftime(Sys.Date(),debut, units="days"))
        illness_years <- floor(illness_days/365)
        illness_months <- illness_years * 12
        toReturn <- data.frame(IllnessYears = illness_years,
                               IllnessMonths = illness_months,
                               DebutMonth = format(debut, "%m"),
                               DebutYear = format(debut, "%Y"))
        if("Age" %in% colnames(df)){
            debut_age <- df$Age - illness_years
            toReturn <- cbind(toReturn, data.frame(DebutAge = debut_age)) 
        }
    }
    return(toReturn)
}


