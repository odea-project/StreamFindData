
#' msFilePaths
#'
#' @description Gets the full paths of ".mzML" and ".mzXML" files in the
#' \pkg{streamFindData} package.
#'
#' @return A character vector with the full paths of MS files.
#'
#' @export
#'
msFilePaths <- function() {
  r_path <- system.file(package = "streamFindData", dir = "extdata")
  files <- list.files(r_path, pattern = ".mzML|.mzXML", full.names = TRUE)
  return(files)
}


#' msFilesDescription
#'
#' @description A table with a description of the ".mzML" and ".mzXML" files
#' in the \pkg{streamFindData} package.
#'
#' @return A \linkS4class{data.table} with details for each ".mzML" and
#' ".mzXML" file.
#'
#' @export
#'
msFilesDescription <- function() {

  files <- msFilePaths()

  df_files_desc <- data.frame(
    N. = seq_len(length(files)),
    file_type = rep("MS", 27),
    file_name = basename(files),
    device = c(rep("Agilent Q-TOF", 27)),
    data_type = c(rep("centroid", 6), rep("profile", 3), rep("centroid", 18)),
    ac_mode = c(rep("MS/MS", 27)),
    polarity = c(rep("positive", 9),
                 rep("negative", 3), rep("positive",3),
                 rep("negative", 3), rep("positive",3),
                 rep("negative", 3), rep("positive",3)),
    description = c(
      rep("Basic centroided MS data as mzML spiked with chemical
        and internal standards (CS and IS, respectively).", 3),
      rep("Basic centroided MS data as mzXML spiked with chemical
        and internal standards (CS and IS, respectively).", 3),
      rep("Basic profile MS data as mzML spiked with CS and IS.", 3),
      rep("Blank MS data as mzML spiked with IS.", 6),
      rep("Wastewater secondary effluent MS data as mzML spiked with IS.", 6),
      rep("Wastewater secondary effluent treated with ozonated
        strong water MS data as mzML spiked with IS.", 6)
    )
  )

  return(df_files_desc)
}


#' msSpikedChemicals
#'
#' @description List of chemicals spiked to ".mzML" and ".mzXML" files.
#'
#' @return A \linkS4class{data.table} with the list of chemicals spiked in
#' samples corresponding to the ".mzML" and ".mzXML" files.
#'
#' @importFrom data.table data.table fread setnames
#'
#' @export
#'
msSpikedChemicals <- function() {
  r_path <- system.file(package = "streamFindData", dir = "extdata")
  db <- paste0(r_path, "/spiked_chemicals_hrms.csv")
  db <- fread(db)
  db$ionization <- "positive"
  db[, `:=`("mz_pos" = neutralMass + 1.0073, "mz_neg" = neutralMass - 1.0073 )]
  db[grepl("neg", db$comment), ionization := "both"]
  setnames(db, "mixIUTA", "tag")
  db[tag %in% 1, tag := "CS"]
  db[, in_file := "1-6"]
  db[tag %in% "IS", in_file := "1-24"]

  return(db)
}
