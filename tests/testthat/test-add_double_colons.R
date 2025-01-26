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

  expect_equal(
    add_double_colons("utils::as.roman(12)", "utils"),
    "utils::as.roman(12)"
  )

  expect_warning(
    add_double_colons("tibble(x = 1); some_fun()", "dplyr"),
    "Couldn't find packages exporting 1 function\\(s\\): `some_fun\\(\\)`"
  )
  
})
