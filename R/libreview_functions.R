#' Gives libreview (Abbot) cnames set 
#' @param lang requested language
#' @return libreview cnames requested or NULL if it's not supported
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
libreviewCNAMES <- function(lang="ES"){
    libreview <- list(ES = list(
        GMI = "Indicador de gestión de glucosa (GMI) %",
        MeanReads = "Promedio de Lecturas/Vistas por día",
        Birthdate = "Fecha de nacimiento",
        LastUpdate = "Últimos datos disponibles",
        CV = "Coeficiente de variación",
        SD = "Desviación estándar",
        GlucoseMean = "Glucosa promedio",
        TIR = "% en el objetivo",
        TBR = "% por debajo del objetivo",
        TAR = "% por encima del objetivo",
        TBRS = "% por debajo del umbral de eventos de hipoglucemia",
        TARS = "% por encima del umbral de eventos de hiperglucemia"))

    # Check and return
    cnames <- NULL
    if(lang %in% names(libreview)){
       cnames <- libreview[[lang]]
    }else{
       warning("Requested language is not supported for current libreview sets")
    }
    return(cnames)
}


#' Internal function used to read a Libreview CSV line and clean known issues
#' @param connin input connection
#' @param N lines to be read
#' @param csep separator char
#' @param NA.strings strings to be read as NA
#' @return line splitted
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
rl <- function(connin, N = 1, csep = ",", NA.strings = c("NA","*","-",""," ")){
    info <- readLines(connin, n = N)

    if(length(info)<=0) return(character(0))
    if(grepl('""',info)){ # Special case 
        info <- gsub('""','"', gsub('^"(.*)"$',"\\1",info))
    }
    # Clean numeric comma problem
    auxInfo <- unlist(strsplit(info,'"'))
    auxNumeric <- sapply(auxInfo,function(entry){
        entrydot <- gsub(',', '.',entry)
        return(suppressWarnings(as.numeric(entrydot)))
    })
    invisible(sapply(seq_along(auxNumeric),function(i){
        if(!is.na(auxNumeric[i])){
        info <<- gsub(paste0('"',auxInfo[i],'"'),auxNumeric[i],info) 
        }
    }))

    # Fix empty last entry
     if(grepl(",$",info)) info <- paste0(info," ")

    finalEntry <- unlist(strsplit(info,csep))
    # Mark NAs
    invisible(lapply(seq_along(finalEntry),function(i){
        if(finalEntry[i] %in% NA.strings) finalEntry[i] <<- NA
    }))

    return(finalEntry)
}


#' Loads a Libreview exported CSV file
#' @param file to be loaded
#' @param lang if not null, will be used to obtain libreview cnames
#' @return a list with dataframe and metadata loaded
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
loadLibreviewCSV <- function(file, lang=NULL){
    # Open connections
    cin <- file(file,"r")
    # Read metadata and header
    metadata <- rl(cin)
    header <- rl(cin)
    # Prepare final DF
    lvcohort <- data.frame(matrix("", ncol = length(header),nrow = 0))
    # Read info
    linfo <- rl(cin) # Read first line
    while(length(linfo) > 0){
        lvcohort <- rbind(lvcohort, linfo)
        linfo <- rl(cin)
    }

    # Handle numeric cols
    invisible(lapply(seq(ncol(lvcohort)),function(j){
        preNAs <- sum(is.na(lvcohort[,j]))
        auxNum <- suppressWarnings(as.numeric(lvcohort[,j]))
        postNAs <- sum(is.na(auxNum))
        if(preNAs == postNAs){
            lvcohort[,j] <<- auxNum
        }
    }))

    # Handle CNAMES
    colnames(lvcohort) <- header
    if(!is.null(lang)){
        cnames <- libreviewCNAMES(lang)
        if(!is.null(cnames)){
            auxNames <- colnames(lvcohort)
            invisible(lapply(seq_along(cnames),function(i){
                if(cnames[[i]] %in% auxNames){
                    indx <- match(cnames[[i]], auxNames)
                    auxNames[indx] <<- names(cnames)[i]
                }
            }))
            colnames(lvcohort) <- auxNames
        }
    }

    # Take metadata date
    auxDates <- grepl("[0-9]{2}-[0-9]{2}-[0-9]{4}",metadata)
    cohortDate <- NULL
    if(any(auxDates)){
        cohortDate <- gsub(".*([0-9]{2})-([0-9]{2})-([0-9]{4}).*",
                            "\\1-\\2-\\3",
                            metadata[which(auxDates)])
    }

    # Close and return
    close(cin)
    return(list(Metadata = metadata,
                Header = header,
                Date = cohortDate,
                Data = lvcohort))
}



#' Writes data as Libreview exported CSV file
#' @param df cohort dataset
#' @param outfile destiny
#' @param metadata (optional) pre-data header of file. If NULL, an empty entry
#'        will be injected 
#' @param header (optional) header colnames to be written
#' @param originalLVFormat if TRUE, original LibreView format will be exported.
#'        If FALSE, corrected format will be used
#' @return void
#' @export
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>
writeLibreviewCSV <- function(df, outfile, metadata = NULL, header = NULL,
    originalLVFormat = FALSE){
    # Checks
    if(originalLVFormat){
        warning(paste0("Original Libreview format is erroneous because use ",
            "comma as numerical digits separator and (also) as fields ",
            "separator. Avoid this erroneous format. Corrected format will be",
            " used instead."))
    }
    if(is.null(metadata)){ 
        metadata <- "# "
    }else if(length(metadata) > 1){
        metadata <- paste(metadata, collapse = ",")
    }
    if(!is.null(header)) colnames(df) <- header

    # Export
    write(metadata, file = outfile)
    suppressWarnings(write.table(df, 
        file = outfile, sep = ",", quote = FALSE, col.names = TRUE,
        row.names = FALSE, append = TRUE))
}