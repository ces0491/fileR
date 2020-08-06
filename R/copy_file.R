#' copy a file
#'
#' @param source_file string with file path  of the object to copy
#' @param dest_dir string indicating the directory to paste copied file to
#' @param overwrite logical indicating whether existing files in destination directory should be overwritten
#' @param timestamp logical indicating whether you'd like to timestamp the copied file
#'
#' @return NULL
#'
copy_file <- function(source_file, dest_dir, overwrite, timestamp) {

  assertR::assert_true(length(source_file) == 1, "logic error - expecting single file")

  reqd_file_exists <- file.exists(source_file)
  assertR::assert_true(reqd_file_exists, "source file doesn't exist")

  file_name <- basename(source_file)

  dest_dir_exists <- dir.exists(dest_dir)
  if(!dest_dir_exists) {
    dir.create(dest_dir, recursive = TRUE)
  }

  if(timestamp) {
    copied_file <- glue::glue("{dest_dir}/{format(Sys.time(), '%H%M%S')}_{file_name}") # want the copied file to have the same name as original but with date stamp
  } else {
    copied_file <- glue::glue("{dest_dir}/{file_name}")
  }

  file.copy(from = source_file, to = copied_file, overwrite = overwrite, copy.date = TRUE)

}

#' copy multiple files from a windows directory
#'
#' @param source_files \code{data.frame} with file_path and file_name of the object to copy
#' @param dest_dir string indicating the directory to paste copied file to
#' @param overwrite logical indicating whether existing files in destination directory should be overwritten - default FALSE results in no copy
#' @param timestamp logical indicating whether you'd like to timestamp the copied file
#'
#' @return NULL
#' @export
#'
copy_files <- function(source_files, dest_dir, overwrite = FALSE, timestamp = FALSE) {

  assertR::assert_present(names(source_files), c("file_path", "file_name"), "logic error")

  for(n in 1:nrow(source_files)) {

    s_file <- source_files[n, ]

    filepath <- s_file$file_path
    filename <- s_file$file_name

    S <- which(source_files$file_path == filepath)
    progress <- round(S/length(source_files$file_path), 2) * 100
    print(glue::glue("{filename} copied from source - {progress}% complete"))

    copy_file(filepath, dest_dir, overwrite, timestamp)
  }

}
