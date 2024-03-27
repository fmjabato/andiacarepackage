#' Return list of allowed columns for reports
#' @return vector of allowed columnames
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
allowedAndiacareReportCols <- function(){
    allowedNames <- c("TIR", "TAR", "TBR", "TARS", "TBRS", "Birthdate",
                    "Source", "GMI", "MeanReads", "LastUpdate", "CV", "SD",
                    "GlucoseMean", "Debut")
    algorithms <- allowedDiabetesClassifiers()

    invisible(lapply(seq_along(algorithms),function(i){
        allowedNames <<- c(allowedNames, names(algorithms)[i])
        if(!is.null(algorithms[[i]]$Extra)){
            allowedNames <<- c(allowedNames,
                                colnames(algorithms[[i]]$Extra(data.frame())))
        }
    }))
    return(unique(allowedNames))
}


#' Renders an andiacare cohort report
#' @param cohort to be plotted
#' @param output output file
#' @param template_folder (Optional) Rmd templates path
#' @param verbose (Optional) boolean that activates verbose mode
#' @param pediatric (Optional) boolean that activates pediatric mode
#' @return void
#' @export
#' @importFrom rmarkdown render
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
renderAndiacareReportDM1 <- function(cohort, output,
    template_folder = file.path(find.package("andiacare"), "templates"),
    verbose = FALSE, pediatric = FALSE, dateFormat = "%d-%m-%Y") {

    allowedCols <- allowedAndiacareReportCols()
    cohort <- cohort[, allowedCols[allowedCols %in% colnames(cohort)]]

    # Special columns
    anyAdult <<- FALSE
    ageChecks <- rep(TRUE, nrow(cohort))
    if("Birthdate" %in% colnames(cohort)){
        agefun <- function(x){getAge(x, format = dateFormat)}
        cohort$Age <- unlist(lapply(cohort$Birthdate, agefun))
        cohort$AgeRange <- ageRangeSWEET(cohort$Age, pediatric)
        ageChecks <- !grepl("Adult", cohort$AgeRange)
        anyAdult <<- !all(ageChecks)
    }

    # Add targets if possible
    cohort <- cbind(cohort, andiacare::andiacareATTDTargets(cohort))

    # Add Sources sizes
    if("Source" %in% colnames(cohort)){
        cohort$SWEET <- andiacare::sizeRangeSWEET(cohort$Source)
    }

    if("Debut" %in% colnames(cohort)){
        cohort <- cbind(cohort, andiacare:::getDebutStats(cohort))
    }

    # Check QA
    algorithms <- names(andiacare::allowedDiabetesClassifiers())
    algorithms <- algorithms[algorithms %in% colnames(cohort)]
    # Avoid conflicts because of TARS and TBRS
    if(all(c("Andiacare","GRI") %in% algorithms)) algorithms <- c("Andiacare")
    qaChecks <- checkEntryQA(cohort, algorithms, remove.ends = TRUE)
    removableEntries <- data.frame()
    if(any(!qaChecks | !anyAdult)){
        removableEntries <- cohort[!qaChecks | !ageChecks,]
        cohort <- cohort[qaChecks & ageChecks,]
    }


    # Render
    suppressWarnings(rmarkdown::render(
        input = file.path(template_folder, "cohortDM1_report.Rmd"),
        output_file = output,
        quiet = !verbose))
}