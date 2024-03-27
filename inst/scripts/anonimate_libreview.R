#! /usr/bin/env Rscript

#' Script to anonimate libreview datasets
#' @author Fernando Moreno Jabato <fmjabato(at)gmail(dot)com>

#############################################
### CONFIGURE 
#############################################
options(warn=1)

option_list <- list(
  optparse::make_option(c("-i", "--input"), type="character",
    help="File with CSV file in Libreview format."),
  optparse::make_option(c("-l", "--lang"), type="character", default = "ES",
    help="Libreview language. Default: %default"),
  optparse::make_option(c("-n", "--name"), type="character", default = "Nombre",
    help="Libreview names colname. Default: %default"),
  optparse::make_option(c("-s", "--surname"), type="character",
    default = "Apellidos",
    help="Libreview surnames colname. Default: %default"),
  optparse::make_option(c("-o", "--outbasename"), type="character", 
    default = NULL,
    help="Output file basename. Default: <inputbasename>_anonimated")
)

opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))

#############################################
### PIPELINE
#############################################

# Load
cohort <- andiacare::loadLibreviewCSV(file = opt$input,
                                      lang = opt$lang)

# Anonimate
cohort_anonimated <- andiacare::anonimate(df = cohort$Data,
                                          nameCol = opt$name,
                                          surnameCol = opt$surname)

# Output basename
if(is.null(opt$outbasename)){
  opt$outbasename <- sub(pattern = "(.*)\\..*$",
                          replacement = "\\1", 
                          opt$input)
}


# Export
andiacare::writeLibreviewCSV(df = cohort_anonimated$df,
                            outfile = paste0(opt$outbasename, 
                                            "_anonimated.csv"),
                            metadata = cohort$Metadata, header = cohort$Header)
write.table(x = cohort_anonimated$dictionary, 
            file = paste0(opt$outbasename, "_anonimated.dict"),
            sep = ",", quote = FALSE, col.names = TRUE, row.names = FALSE)
