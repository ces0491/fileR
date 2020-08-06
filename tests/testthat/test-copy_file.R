test_that("files copy correctly", {

  # pkg_desc <- desc::desc()
  # pkg_name = pkg_desc$get_field("Package")

  test_file <- "copy_test_file.csv"
  src_dir <- system.file("extdata", ".", package = "fileR")
  dest_dir <- system.file("testdata", ".", package = "fileR")

  src <- paste(src_dir, test_file, sep = "/")

  copy_file(src, dest_dir, overwrite = TRUE, timestamp = FALSE)

  test_file <- read.csv(paste(dest_dir, test_file, sep = "/"), stringsAsFactors = FALSE)

  expected_file <- data.frame(col_a = c(1, 2, 3),
                              col_b = log(c(1, 2, 3)),
                              col_c = c("x", "y", "z"))

  testthat::expect_equal(test_file, expected_file)

})
