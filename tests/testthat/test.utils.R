

# default options load ----------------------------------------------------

test_that("default options load", {
  library(snomedizer)
  expect_equal(snomedizer_options_get()$limit, 50)
  expect_equal(snomedizer_options_get()$branch, "MAIN")
  expect_equal(snomedizer_options_get("branch"), "MAIN")
  onLoadendpoint <- snomedizer_options_get("endpoint")
  expect_true(onLoadendpoint == "https://snowstorm.ihtsdotools.org/snowstorm/snomed-ct" |
                onLoadendpoint == "https://browser.ihtsdotools.org/snowstorm/snomed-ct")
  expect_error(snomedizer_options_get("Kaiserschmarren"))
})

# snomedizer_default_options ----------------------------------------------

test_that("snomedizer_default_options", {
  expect_error(snomedizer_options_set())
  expect_error(snomedizer_options_set(endpoint = "https://URLWHICHDOESNOTEXIST"))
  expect_invisible(snomedizer_options_set(endpoint = snomed_public_endpoint_suggest()))
  expect_error(
    expect_warning(
      snomedizer_options_set(
        endpoint = "http://detectportal.firefox.com/success.txt")))
  expect_invisible(snomedizer_options_set(branch = "MAIN"))
  expect_invisible(snomedizer_options_set(limit = 50))
})


# release_version ---------------------------------------------------------

test_that("release_version", {
  ct_version <- release_version(branch = "MAIN/2021-07-31")
  expect_equal(ct_version$rf2_date, "20210731")
  expect_equal(ct_version$rf2_month_year, "July 2021")
})


# result_flatten ----------------------------------------------------------

test_that("result_flatten", {
  pneumo <- result_flatten(
    api_concepts(term = "pneumonia",
                 activeFilter = TRUE,
                 limit = 2))
  expect_true(is.data.frame(pneumo))
  expect_equal(dim(pneumo)[1], 2)

  expect_equal(result_flatten(api_concepts(conceptIds = "thingummy"))$items, NA)
})

# result_completeness -----------------------------------------------------

test_that("result_completeness", {
  pneumo <- api_concepts(term = "pneumonia",
                              activeFilter = TRUE,
                              limit = 10)
  expect_false(result_completeness(pneumo, silent = TRUE))
})

# .check_rest_query_length1 -----------------------------------------------

test_that(".check_rest_query_length1", {
  expect_silent(.check_rest_query_length1(list()))
  expect_silent(.check_rest_query_length1(list(query = list(a = 1, b = 2))))
  expect_error(.check_rest_query_length1(list(query = list(a = 1:2))))
})


# .concatenate_array_parameter --------------------------------------------

test_that(".concatenate_array_parameter", {
  conceptIds = c("233604007", "68566005")
  expect_equal(
    .concatenate_array_parameter(conceptIds),
    I("233604007&conceptIds=68566005")
  )

  thingummy = c("233604007", "68566005")
  expect_equal(
    .concatenate_array_parameter(thingummy),
    I("233604007&thingummy=68566005")
  )
})


# .validate_limit ---------------------------------------------------------

test_that(".validate_limit", {
  expect_equal(.validate_limit(1e+04), 10000L)
  expect_error(.validate_limit("100"))
  expect_error(.validate_limit( "blurgh"))
  expect_error(.validate_limit(-100L))
  expect_warning(.validate_limit(100000))
  expect_error(.validate_limit(NULL))
  expect_error(.validate_limit(NA_real_))
  expect_error(.validate_limit(c(1, 2)))
})


# .validate_branch --------------------------------------------------------

test_that(".validate_branch", {
  expect_equal(.validate_branch("MAIN"), "MAIN")
  expect_equal(.validate_branch("MAIN/SNOMEDCT-GB"), "MAIN%2FSNOMEDCT-GB")
  expect_error(.validate_branch(""))
})

# snomedizer_version_compatibility ----------------------------------------

test_that("snomedizer_version_compatibility", {
  expect_true(snomedizer_version_compatibility())
})


# .validate_branch --------------------------------------------------------

test_that(".validate_branch", {
  expect_equal(.validate_branch("MAIN"), "MAIN")
  expect_equal(.validate_branch("MAIN/SNOMEDCT-GB"), "MAIN%2FSNOMEDCT-GB")
  expect_error(.validate_branch(""))
})

# snomedizer_version_compatibility ----------------------------------------

test_that("snomedizer_version_compatibility", {
  expect_true(snomedizer_version_compatibility(), "logical")
})



# .snomed_identifiers_deduplicate ----------------------------------------

test_that(".snomed_identifiers_deduplicate", {
  expect_equal(
    .snomed_identifiers_deduplicate(c("001", " 001", NA, "")),
    "001"
  )
  expect_equal(
    .snomed_identifiers_deduplicate(1:3),
    c("1", "2", "3")
  )
})

# .split_into_chunks ------------------------------------------------------

test_that(".split_into_chunks", {
  expect_equal(
    .split_into_chunks(x = c(1, 1, 2, 2, 3), max_length = 2),
    list(
      "0" = c(1,1),
      "1" = c(2,2),
      "2" = c(3)
    )
  )
})
