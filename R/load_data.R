
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
    file_type = rep("MS", 30),
    file_name = basename(files),
    device = c(rep("Agilent Q-TOF", 27), rep("Schimadzu Q-Trap", 1),
               rep("Sciex Q-Trap", 2)),
    data_type = c(rep("centroid", 6), rep("profile", 3), rep("centroid", 18), rep("profile", 3)),
    ac_mode = c(rep("MS/MS", 27), rep("MRM", 3)),
    polarity = c(rep("positive", 9),
                 rep("negative", 3), rep("positive",3),
                 rep("negative", 3), rep("positive",3),
                 rep("negative", 3), rep("positive",3),
                 rep("negative", 1), rep("positive", 2)),
    description = c(
      rep("Basic centroided MS data as mzML spiked with chemical
        and internal standards (CS and IS, respectively).", 3),
      rep("Basic centroided MS data as mzXML spiked with chemical
        and internal standards (CS and IS, respectively).", 3),
      rep("Basic profile MS data as mzML spiked with CS and IS.", 3),
      rep("Blank MS data as mzML spiked with IS.", 6),
      rep("Wastewater secondary effluent MS data as mzML spiked with IS.", 6),
      rep("Wastewater secondary effluent treated with ozonated
        strong water MS data as mzML spiked with IS.", 6),
      rep("MRM acquisition of estrogenic compounds.", 1),
      rep("MRM acquisition of a nitrosamine mixture.", 1),
      rep("MRM acquisition of chemical and internal standards, as in the
          hrms files", 1)
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
#' @importFrom data.table data.table fread setnames copy
#'
#' @export
#'
msSpikedChemicals <- function() {
  r_path <- system.file(package = "streamFindData", dir = "extdata")
  db <- paste0(r_path, "/spiked_chemicals_hrms.csv")
  db <- fread(db)
  db$ionization <- "positive"
  db[, `:=`("mz_pos" = mass + 1.0073, "mz_neg" = mass - 1.0073 )]
  db[grepl("neg", db$comment), ionization := "both"]
  db[tag %in% "MIX1", tag := "S"]
  db[, in_file := "1-6"]
  db[tag %in% "IS", in_file := "1-27"][]
  return(db)
}



#' msSpikedEstrogens
#'
#' @description List of estrogens spiked to ".mzML" files in MRM mode.
#'
#' @return A \linkS4class{data.table} with the list of estrogens spiked in
#' sample corresponding to the ".mzML" file in MRM mode with negative polarity.
#'
#' @importFrom data.table data.table fread setnames
#'
#' @export
#'
msSpikedEstrogens <- function() {
  r_path <- system.file(package = "streamFindData", dir = "extdata")
  db <- paste0(r_path, "/spiked_estrogens.csv")
  db <- fread(db)
  db$ionization <- "negative"
  db$in_file <- "28"

  return(db)
}
