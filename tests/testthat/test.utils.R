

test_that("default options load", {
  library(snomedizer)
  expect_equal(snomedizer_options_get()$limit, 50)
  expect_equal(snomedizer_options_get()$branch, "MAIN")
  expect_equal(snomedizer_options_get("branch"), "MAIN")
  onLoadendpoint <- snomedizer_options_get("endpoint")
  expect_true(onLoadendpoint == "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct" |
                onLoadendpoint == "https://browser.ihtsdotools.org/snowstorm/snomed-ct")
})

test_that("snomedizer_default_options", {
  expect_error(snomedizer_options_set())
  expect_error(snomedizer_options_set(endpoint = "https://URLWHICHDOESNOTEXIST"))
  expect_invisible(snomedizer_options_set(endpoint = snomed_public_endpoint_suggest()))
  expect_warning(
    expect_error(snomedizer_options_set(endpoint = "http://detectportal.firefox.com/success.txt"))
  )
})

test_that("result_flatten", {
  pneumo <- result_flatten(
    api_find_concepts(term = "pneumonia",
                      activeFilter = TRUE,
                      limit = 2))
  expect_true(is.data.frame(pneumo))
  expect_equal(dim(pneumo)[1], 2)
})

test_that("result_completeness", {
  pneumo <- api_find_concepts(term = "pneumonia",
                              activeFilter = TRUE,
                              limit = 10)
  expect_false(result_completeness(pneumo, silent = TRUE))
})
#
# test_that("snowstorm_branch_info", {
#   expect_named(
#     snowstorm_branch_info(),
#     c(
#       "path", "state", "containsContent", "locked", "creation",
#       "base", "head", "creationTimestamp", "baseTimestamp",
#       "headTimestamp", "metadata", "versionsReplacedCounts"
#     )
#   )
# })

