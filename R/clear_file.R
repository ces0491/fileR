#' clear all files in a directory
#'
#' @param clear_dir string indicating the directory of files to clear
#' @param file_ext character vector of file extensions of files to remove
#'
#' @export
#'
clear_all <- function(clear_dir, file_ext = NULL) {

  if(is.null(file_ext)) {
    #remove all files in clear dir
    do.call(file.remove, list(list.files(clear_dir, full.names = TRUE)))
  } else {
    #remove only specified file types from clear dir
    files <- stringr::str_subset(list.files(clear_dir, full.names = TRUE), file_ext)
    do.call(file.remove, list(files))
  }

}

#' clear files
#'
#' @param clear_dir string indicating the directory of files to clear
#' @param file_name character vector of file names to remove
#'
#' @export
#'
clear_files <- function(clear_dir, file_name) {

  for(f in file_name) {

    f_path <- file.path(clear_dir, f)

    if(file.exists(f_path)) {
      file.remove(f_path)
    } else {
      message(paste("file ", f, " not found in directory ", clear_dir))
    }

  }

}
