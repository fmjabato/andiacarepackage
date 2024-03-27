#! /usr/bin/env Rscript

#' Script to handle libreview datasets
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>

#############################################
### CONFIGURE 
#############################################
options(warn=1)

option_list <- list(
  optparse::make_option(c("-i", "--input"), type="character",
    help="File or path with CSV files in Libreview format."),
  optparse::make_option(c("-l", "--lang"), type="character", default = "ES",
    help="Libreview language. Default: %default"),
  optparse::make_option(c("-d", "--dateInit"), type="character",
    default=as.character(Sys.Date()),
    help="Initial date range to be used from path. Used only if input is a directory. Default: today"),
  optparse::make_option(c("-D", "--dateEnd"), type="character",
    default=as.character(Sys.Date()),
    help="End date range to be used from path. Used only if input is a directory. Default: today"),
  optparse::make_option(c("-f", "--dateformat"), type="character", default = "%d-%m-%Y",
    help="Birthdate date format. Default: %default"),
  optparse::make_option(c("-F", "--regularFormat"), type="logical",
    action="store_true", default=FALSE, 
    help="Activates file in regular <Date>_<Hospital>.csv format."),
  optparse::make_option(c("-P", "--pediatric"), type="logical",
    action="store_true", default=FALSE, 
    help="Activates pediatric mode."),
  optparse::make_option(c("-r", "--report"), type="logical",
    action="store_true", default=FALSE, 
    help="Generates all reports. Including aggregated report if possible."),
  optparse::make_option(c("-R", "--aggreport"), type="logical",
    action="store_true", default=FALSE, 
    help="Generates aggregated report if possible."),
  optparse::make_option(c("-s", "--skip"), type="logical",
    action="store_true", default=FALSE, 
    help="Skip classification process."),
  optparse::make_option(c("-u", "--updateThr"), type="numeric",
    default=30,
    help="Days threshold to mark as obsolete patient data. Default: %default"),
  optparse::make_option(c("-A", "--andiacare"), type="logical",
    action="store_true", default=FALSE, 
    help="Uses Andiacare algorithm."),
  optparse::make_option(c("-G", "--gri"), type="logical",
    action="store_true", default=FALSE, 
    help="Uses GRI algorithm."),
  optparse::make_option(c("-v", "--verbose"), type="logical",
    action="store_true", default=FALSE, 
    help="Activates verbose mode."),
  optparse::make_option(c("-o", "--outpath"), type="character", default = NULL,
    help="Output path. Default: input path")
)

opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))

#############################################
### MAIN VARIABLES
#############################################

# Check mandatory info
if(opt$report | opt$aggreport){
    template_folder <- paste(find.package("andiacare"), "templates", 
                            sep = .Platform$file.sep)
}

# Check I/O
directoryFlow <- file.info(opt$input)$isdir
if(directoryFlow){
    inpath <- opt$input
    hospitals_dfs <- list()
}else{
    inpath <- dirname(opt$input)
}

if(is.null(opt$outpath)) opt$outpath <- dirname(opt$input)
outpath <- ifelse(dir.exists(opt$outpath),
                  opt$outpath,
                  dirname(opt$outpath))


# Check algorithms
algorithms <- c()
if(opt$andiacare) algorithms <- c(algorithms,"Andiacare")
if(opt$gri) algorithms <- c(algorithms,"GRI")
if(length(algorithms)<=0){
  warning("No algorithms has been specified. Andiacare will be executed by default.")
  algorithms <- c("Andiacare")
}

# Date variables
targetDateRange <- c(as.Date(opt$dateInit), as.Date(opt$dateEnd))


# I/O actions
if(directoryFlow){
    infiles <- list.files(path=opt$input, 
                          pattern = ifelse(opt$regularFormat,
                                            "^[0-9]{8}_.*\\.csv$",
                                            "\\.csv$"),
                          full.names = TRUE,
                          include.dirs = FALSE)
    auxfilenames <- basename(infiles)
    infilesdf <- data.frame(File = auxfilenames,
                            FullFile = infiles)

    if(opt$regularFormat){
        infilesdf <- cbind(data.frame(
                        Date = gsub("^([0-9]{4})([0-9]{2})([0-9]{2}).*",
                                    "\\1-\\2-\\3",auxfilenames),
                        Hospital = gsub("_"," ", gsub("^[0-9]{8}_(.*)\\.csv",
                                                      "\\1",auxfilenames))),
                        infilesdf)
            auxdates <- as.Date(infilesdf$Date) 
            infilesdf <- infilesdf[targetDateRange[1] <= auxdates & 
                          auxdates <= targetDateRange[2],]    

    }
}else{
    infilesdf <- data.frame(File = basename(opt$input),
                            FullFile = opt$input)
    if(opt$regularFormat){
        infilesdf <- cbind(data.frame(
                        Date = gsub("^([0-9]{4})([0-9]{2})([0-9]{2}).*",
                                    "\\1-\\2-\\3",opt$input),
                        Hospital = gsub("_"," ", gsub("^[0-9]{8}_(.*)\\.csv",
                                                      "\\1",opt$input))),
                        infilesdf)
    }
}


#############################################
### ANALYSIS
#############################################

if(opt$verbose & directoryFlow){
    message(paste0("Detected (",nrow(infilesdf),") files to be classified"))
}

invisible(lapply(seq(nrow(infilesdf)),function(i){
    if(opt$verbose) message(paste0("> Analyzing file: ",infilesdf$File[i]))
    cohort <- andiacare::loadLibreviewCSV(file = infilesdf$FullFile[i],
                                            lang = opt$lang)
    cohort$Data <- andiacare::inferTXR(cohort$Data)

    if(opt$verbose){
        message(paste0("\t> Loaded (", nrow(cohort$Data), ") patients"))
    }

    allowed_algorithms <- andiacare::allowedDiabetesClassifiers()
    classification <- as.data.frame(do.call(cbind,lapply(algorithms, function(alg){
      # Useful functions
      classifier <- allowed_algorithms[[alg]]$Classifier
      # Classify
      classes <- data.frame(Class = classifier(cohort$Data))
      colnames(classes) <- c(alg)
      if(andiacare::hasExtra(alg)){
        extraf <- allowed_algorithms[[alg]]$Extra
        extra <- extraf(cohort$Data)
        classes <- cbind(classes, extra)
      }
      return(classes)
    })))
    cohort$Data <- cbind(cohort$Data, classification)

    # Export
    if(opt$verbose) message("\t- Saving CSV file")
    cohortBasename <- gsub("\\.[a-zA-z]*$", "", infilesdf$File[i])
    currName <- paste0(cohortBasename, "_classified.csv")
    outf <- paste(outpath, currName, sep = .Platform$file.sep)
    suppressWarnings(andiacare::writeClassificationTable(
            cohort$Data,
            outfile = outf,
            sourceDate = cohort$Date,
            analysisDate = andiacare::todayString()))


    # Render report
    if(opt$report){
        currName <- paste0(cohortBasename, "_report.html")
        outf <- paste(outpath, currName, sep = .Platform$file.sep)
        if(opt$verbose) message(paste0("\t- Generating report at: ", currName))

        andiacare::renderAndiacareReportDM1(cohort$Data, outf,
            pediatric = opt$pediatric, verbose = !opt$verbose,
            dateFormat = opt$dateformat)

    }

    # Store for group report
    if(directoryFlow){
      entryName <- ifelse(opt$regularFormat, 
                        infilesdf$Hospital[i],
                        infilesdf$File[i])
      hospitals_dfs[[entryName]] <<- cohort
    }
}))


#############################################
### GROUP ANALYSIS
#############################################



# Final report
if(directoryFlow){
    # Useful variables
    reportCols <- andiacare::allowedAndiacareReportCols()
    cohortConsensus <- unique(unlist(lapply(seq_along(hospitals_dfs),function(i){
        colnames(hospitals_dfs[[i]]$Data)
    })))

    # Unify cohorts
    cohortNames <- cohortConsensus[cohortConsensus %in% reportCols]
    cohort <- as.data.frame(do.call(rbind,lapply(seq_along(hospitals_dfs),function(i){
        auxNames <- cohortNames %in% colnames(hospitals_dfs[[i]]$Data)
        auxCohort <- hospitals_dfs[[i]]$Data[,cohortNames[auxNames]]
        invisible(lapply(cohortNames[which(!auxNames)],function(newCol){
            auxCol <- data.frame(A = rep(NA, nrow(auxCohort)))
            colnames(auxCol) <- newCol
            auxCohort <<- cbind(auxCohort,auxCol)
        }))

        return(cbind(auxCohort[,cohortNames],
                    data.frame(Source = rep(names(hospitals_dfs)[i],
                                            nrow(hospitals_dfs[[i]]$Data)))))
    })))
    sourcesSizes <- sort(table(cohort$Source))
    cohort$Source <- factor(cohort$Source, levels = names(sourcesSizes))

    if(opt$verbose){
        message(paste0("Exporting grouped cohort for (", length(hospitals_dfs),
        ") facilites and (", nrow(cohort), ") patients"))
    }

    outf_base <- outf <- paste(outpath,
                    paste0(
                        paste("GeneralReport",
                            gsub("-","",as.character(Sys.Date())),
                            paste0("Init",
                                    gsub("-",
                                            "",
                                            as.character(targetDateRange[1]))),
                            paste0("End",
                                    gsub("-",
                                            "",
                                            as.character(targetDateRange[2]))),
                            sep = "_")), sep = .Platform$file.sep)
    suppressWarnings(andiacare::writeClassificationTable(
        cohort,
        outfile = paste0(outf_base,".csv"),
        analysisDate = andiacare::todayString()
    ))

    if(opt$report | opt$aggreport){
        if(opt$verbose) message(paste0("Generating GENERAL report for (",
                                        length(hospitals_dfs), ") facilities"))
        # Render report
        outf <- paste0(outf_base,".html")
        andiacare::renderAndiacareReportDM1(cohort, outf,
                    pediatric = opt$pediatric,
                    dateFormat = opt$dateformat,
                    verbose = !opt$verbose)

    }
}

if(opt$verbose) message("Classification pipeline finished!")
