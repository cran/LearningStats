#' Data Input
#'
#' \code{read.data} allows to read a file and create a data frame from it. Wrapper for
#' different data input functions available, namely \code{data.table::fread}, \code{readxl::read_excel},
#' \code{haven::read_sas}, \code{haven::read_sav}, \code{haven::read_dta} and \code{readODS::read_ods}. The file
#' extensions supported are: csv, dat, data, dta, ods, RDa, RData, sas7bdat, sav, txt, xls and xlsx.
#'
#' @param name a character string with the name of the file including the file extension from which the data are to be read from.
#' @param dec a character string indicating the decimal separator for txt, csv, dat and data
#' files. If not "." (default) then usually ",".
#' @param sheet the sheet to read for xls, xlsx and ods files. Either a string (the name of a sheet)
#' or an integer (the position of the sheet). If not specified, defaults to the first sheet.
#' @param header a character string indicating if the first data line contains column names, as
#' in \code{data.table::fread}. Defaults according to whether every non-empty field on the first
#' data line is type character; if so, or TRUE is supplied, then the first row is considered as the variables names and any empty column names are given a default name.
#' @param ... Further arguments to be passed to \code{data.table::fread}, \code{readxl::read_excel},
#' \code{haven::read_sas}, \code{haven::read_sav}, \code{haven::read_dta} or \code{readODS::read_ods}.
#'
#' @return A \code{data.frame} containing the data in the specified file or, if Rdata or Rda,
#' an object of class \code{"ls_str"}.
#'
#' @examples
#' data <- data.frame(V1 = 1:5*0.1, V2 = 6:10)
#' tf <- tempfile("tp", fileext = c(".csv", ".txt", ".RData"))
#'
#' write.csv(data, tf[1], row.names = FALSE)
#' read.data(tf[1])                             # csv
#' write.csv2(data, tf[1], row.names = FALSE)
#' read.data(tf[1], dec = ",")                  # csv2
#' write.table(data, tf[2], row.names = FALSE)
#' read.data(tf[2])                             # txt
#' save(data, file = tf[3])
#' read.data(tf[3])                             # RData
#' @export
read.data <- function(name, dec = ".", header = "auto", sheet = 1, ...) {

  ext <- tools::file_ext(name)
  list_ext <- c("txt", "csv", "dat", "data")

  if (ext %in% list_ext) {
    data_tmp <- data.table::fread(name, dec = dec, header = header, ...)
  } else if (toupper(ext)  %in% toupper(c("Rdata", "RDa"))) {
    load(name, envir = globalenv())
    env <- new.env()
    load(name, envir = env)
  } else if (ext == "xls" | ext == "xlsx") {
    data_tmp <- readxl::read_excel(name, sheet = sheet, ...)
  } else if (ext == "sas7bdat") {
    data_tmp <- haven::read_sas(name, ...)
  } else if (ext == "sav") {
    data_tmp <- haven::read_sav(name, ...)
  } else if (ext == "dta") {
    data_tmp <- haven::read_dta(name, ...)
  } else if (ext == "ods") {
    data_tmp <- readODS::read_ods(name, sheet = sheet, ...)
  } else {
    stop("Data extension is not supported")
  }

  # Return
  if (toupper(ext) %in% toupper(c("Rdata", "RDa"))) {
    # return(invisible(NULL))
    return(ls.str(env))
  } else {
    data_df <- as.data.frame(data_tmp)
    return(data_df)
  }
}

