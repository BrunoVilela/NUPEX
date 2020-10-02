test_that("get_lattes_folder works", {
  path_lattes <- system.file("lattes", package = "NUPEX")
  lattes_data <- get_lattes_folder(path_lattes)
  expect_equal(class(lattes_data), "list")
  expect_equal(length(lattes_data), 27)
  classes1 <- sapply(lattes_data, tibble::is_tibble)
  classes2 <- sapply(lattes_data, is.null)
  expect_true(all(classes1 | classes2))
})
