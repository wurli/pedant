test_that("add_double_colons works", {
  expect_equal(
    add_double_colons("mtcars %>% filter(mpg > 21)", "dplyr"),
    "mtcars %>% dplyr::filter(mpg > 21)"
  )

  expect_equal(
    add_double_colons("`%>%`(1:3, sum)", "dplyr"),
    "dplyr::`%>%`(1:3, sum)"
  )

  expect_equal(
    add_double_colons("`intersect`()", "dplyr"),
    "dplyr::`intersect`()"
  )

  expect_equal(
    add_double_colons("tibble(x = 1); anotherPkg::some_fun()", "dplyr"),
    "dplyr::tibble(x = 1); anotherPkg::some_fun()"
  )

  expect_warning(
    add_double_colons("tibble(x = 1); some_fun()", "dplyr"),
    "Couldn't find packages exporting 1 function\\(s\\): `some_fun\\(\\)`"
  )
})

test_that("Error_002 is written when file does not exist", {
  # Define a non-existent file path
  non_existent_file <- tempfile()

  # Run the function with a non-existent file
  ic(non_existent_file)

  # Check if the file was created and contains 'Error_002'
  expect_true(file.exists(non_existent_file))
  expect_equal(readLines(non_existent_file), "Error_002")

  # Clean up
  file.remove(non_existent_file)
})
