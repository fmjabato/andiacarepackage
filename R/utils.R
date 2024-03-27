#' Gives TODAY date as string
#' @return today date as string
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
todayString <- function() {
    today <- as.character(Sys.Date())
    return(today)
}

#' Generates a table from factor/character vecto
#' @param values vector of values/classes/labels
#' @return table with amounts and percentages
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
tableFromCharVector <- function(values){
    auxTable <- table(values, useNA = "ifany")
    auxDF <- data.frame(Value = as.vector(auxTable))
    rownames(auxDF) <- replace(names(auxTable), is.na(names(auxTable)), "NA")
    auxDF$Percentage <- round(auxDF$Value / sum(auxDF$Value) * 100, 2)
    if(is.factor(values)){
        lvls <- levels(values)
        newOrder <- match(lvls, rownames(auxDF))
        newOrder <- as.vector(na.omit(newOrder))
        auxDF[newOrder,]
    }
    return(auxDF)
} 


#' Generates a table from factor/character combinations of columns
#' @param df dataframe to be used
#' @param columns vector of indexes o columnames to be used
#' @param roundPercentage number of decimals to round percentage
#' @param removeZeros if TRUE, combinations with zero instances are removed
#' @return table with amounts and percentages
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
tableFrom2Columns <- function(df, columns, roundPercentage = 2,
    removeZeros = TRUE){
    valuesA <- unique(df[,columns[1]])
    valuesB <- unique(df[,columns[2]])
    finaldf <- expand.grid(valuesA,valuesB)
    colnames(finaldf) <- colnames(df[1,columns[1:2]])
    indexesA <- lapply(valuesA,function(vA){which(df[,columns[1]] == vA)})
    indexesB <- lapply(valuesB,function(vB){which(df[,columns[2]] == vB)})
    names(indexesA) <- valuesA
    names(indexesB) <- valuesB
    finaldf$Value <- unlist(lapply(seq(nrow(finaldf)),function(i){
        return(length(intersect(indexesA[[finaldf[i,1]]],
                                indexesB[[finaldf[i,2]]])))
    }))
    finaldf$Percentage <- round(finaldf$Value / nrow(df) * 100, roundPercentage)
    if(removeZeros){
        finaldf <- finaldf[finaldf$Value > 0,]
    }
    return(finaldf)
}

#' Creates a stats table for a cohort dataset and expected numeric tables
#' @param dataset to be checked
#' @param decimals to round
#' @param targets to be included into table
#' @param boolTargets (optional) extra boolean to be included into table
#' @param toPlot activates to plot process and exports a tablegrob instead a
#' data frame.
#' @param title activates toPlot and includes given title
#' @return stats dataframe (list if bool are included) or tableGrob object if
#' to plot is activated
#' @export
#' @importFrom gridExtra tableGrob
#' @importFrom grid textGrob unit gpar grobHeight
#' @importFrom gtable gtable_add_rows gtable_add_grob
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
statsTable <- function(dataset, decimals = 3,
    targets = c("TARS", "TAR", "TIR", "TBR", "TBRS", "MeanReads", "Age",
                "SourceSize","DebutAge", "IllnessYears"),
    boolTargets = c(), toPlot = FALSE, title = ""){
    
    # Setup
    if(!toPlot) toPlot <- nchar(title) > 0
    stats <- data.frame()
    targets <- targets[targets %in% colnames(dataset)]
    boolTargets <- boolTargets[boolTargets %in% colnames(dataset)]

    # Take stats
    invisible(lapply(targets, function(col){
        if(col %in% colnames(dataset)){
            values <- dataset[,col]
            NAs <- which(is.na(values))
            if(length(NAs) > 0) values <- values[-NAs]
            qs <- quantile(values)
            toBind <- data.frame(
                min = qs[[1]],
                q1 = qs[[2]],
                median = qs[[3]],
                q3 = qs[[4]],
                max = qs[[5]],
                mean = round(mean(values), decimals),
                sd = round(sd(values), decimals),
                count = length(values),
                missing = length(NAs))

            # Store
            auxNames <- rownames(stats)
            stats <<- rbind(stats, toBind)
            rownames(stats) <<- c(auxNames, col)
        }
    }))
    finalTable <- stats

    # Extra targets
    if(length(boolTargets) > 0){
        statsBool <- data.frame()
        invisible(lapply(boolTargets, function(col){
            if(col %in% colnames(dataset)){
                values <- dataset[,col]
                NAs <- which(is.na(values))
                if(length(NAs) > 0) values <- values[-NAs]
                trues <- sum(values)
                falses <- sum(!values)
                toBind <- data.frame(
                    True = trues,
                    False = falses,
                    TruePerc = round(trues / length(values) * 100, 2),
                    FalsePerc = round(falses / length(values) * 100, 2),
                    count = length(values),
                    missing = length(NAs))

                # Store
                auxNames <- rownames(statsBool)
                statsBool <<- rbind(statsBool, toBind)
                rownames(statsBool) <<- c(auxNames, col)
            }
        }))
        finalTable <- list(Numeric = stats, Boolean = statsBool)
    }



    # Prepare to plot
    if(toPlot){
        finalTable <- gridExtra::tableGrob(stats)
        if(nchar(title) > 0){
            titleGrob <- grid::textGrob(title,
                    y = grid::unit(0.5, "npc"),
                    vjust = 0,
                    gp = grid::gpar(fontsize = 20))
            finalTable <- gtable::gtable_add_rows(finalTable,
                    heights = grid::grobHeight(titleGrob) +
                                grid::unit(1, "line"),
                    pos = 0)
            finalTable <- gtable::gtable_add_grob(finalTable, titleGrob,
                    t = 1, l = 1, r = ncol(finalTable))
        }

        if(length(boolTargets) > 0){
            boolTable <- gridExtra::tableGrob(statsBool)
            boolTable <- gtable::gtable_add_rows(boolTable,
                    heights = grid::grobHeight(finalTable) +
                                grid::grobHeight(boolTable) * 1.5,
                                # grid::unit(nrow(finalTable), "line"),
                    pos = 0)
            finalTable <- gtable::gtable_add_grob(boolTable, finalTable,
                t = 1, l = 1, r = ncol(boolTable))
        }
    }

    return(finalTable)
}


#' Calculates correlation between columns of a cohort dataset
#' @param df with cohort data
#' @param targets columns that, if are available, will be studied
#' @param histogram plots histograms in main diagonal
#' @param method correlation method to be applied
#' @return PerformanceAnalytics object with correlations
#' @export
#' @importFrom PerformanceAnalytics chart.Correlation
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
corrChart <- function(df,
    targets = c("TIR", "TBR", "TBRS", "TAR", "TARS", "GMI", "MeanReads",
                "SourceSize", "SWEET", "CV", "Age", "AgeRange",
                "IllnessYears", "DebutAge"),
    method = "pearson",
    histogram = TRUE){
    # Check target columns
    targets <- targets[targets %in% colnames(df)]
    corrChart <- NULL
    if(length(targets) > 0){
        auxDf <- df[, targets]
        invisible(lapply(targets,function(tg){
            auxDf[,tg] <<- as.numeric(auxDf[,tg])
        }))
        corrChart <- PerformanceAnalytics::chart.Correlation(
            auxDf,
            method = method,
            histogram = histogram,
            pch = 19
        )
    }
    return(corrChart)
}



#' Anonimates a dataframe given. Creates a new name for each patient (row) and
#' substitute it into dataframe using the names column and the surname column.
#' If surname column is not given (NULL), the software concatenate the full name
#' using the format <Surname; Name> and stores into names column.
#' If any error occurs, NULL will be returned
#' @param df with cohort data
#' @param nameCol names column identifier or index
#' @param surnameCol (optional) surnames column identifier or index
#' @param onlyDF if TRUE, only the anonimated dataframe will be returned
#' @return list with original, anonimated dictionary and anonimated df or
#'    anonimated dataframe if onlyDF is setted to TRUE
#' @export
#' @importFrom randomNames randomNames
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
anonimate <- function(df, nameCol, surnameCol = NULL, onlyDF = FALSE){
    # setup
    toReturn <- NULL
    randomizable <- FALSE
    concatName <- TRUE

    concatNameFunc <- function(df){
        return(paste(df$last_name, df$first_name, sep = "; "))
    }

    # Check
    if(is.numeric(nameCol)){
        if(nameCol <= ncol(df)) randomizable <- TRUE
    }else if(nameCol %in% colnames(df)){
        randomizable <- TRUE
    }

    if(!is.null(surnameCol)){
        if(is.numeric(surnameCol)){
           if(surnameCol <= ncol(df)) concatName <- FALSE
        }else if(surnameCol %in% colnames(df)){
            concatName <- FALSE
        }
    }


    # Anonimate
    if(randomizable){
        newDF <- df
        randomDict <- randomNames::randomNames(n = nrow(df), 
                                                gender = c("female","male"),
                                                return.complete.data = TRUE)
        # Prepare
        indexes <- 1:nrow(df)
        randomDict$SourceIndex <- sample(indexes)
        randomDict$Source <- rep("", nrow(randomDict))
        randomDict$Unified <- concatNameFunc(randomDict)

        # Check
        auxRdmNames <- table(randomDict$Unified)
        if(any(auxRdmNames != 1)){
            repeatedNames <- which(auxRdmNames > 1)
            affectedIndx <- which(randomDict$Unified %in%
                                    names(auxRdmNames)[repeatedNames])
            invisible(lapply(affectedIndx, function(k){
                candidate <- randomDict$Unified[k]
                while(candidate %in% names(auxRdmNames)){
                    auxCandidate <- randomNames::randomNames(
                                    gender = c("female","male"),
                                    return.complete.data = TRUE)[1,]
                    candidate <- concatNameFunc(auxCandidate)
                }
                randomDict$Unified[k] <<- candidate
                randomDict[k, 1:ncol(auxCandidate)] <- auxCandidate
                auxRdmNames <<- table(randomDict$Unified)
            }))
        }

        # Infer
        invisible(lapply(indexes, function(i){
            j <- randomDict$SourceIndex[i]
            if(concatName){
                randomDict$Source[i] <<- newDF[j, nameCol]
                newDF[j, nameCol] <<- randomDict$Unified[i]
            }else{
                randomDict$Source[i] <<- paste(df[j, surnameCol], 
                                                df[j, nameCol],
                                                sep = "; ")
                newDF[j, nameCol] <<- randomDict$first_name[i]
                newDF[j, surnameCol] <<- randomDict$last_name[i]
            }
        }))

        # Prepare return
        if(onlyDF){
            toReturn <- newDF
        }else{
            toReturn <- list(df = newDF,
                             dictionary = randomDict)
        }
    }

    return(toReturn)
}

