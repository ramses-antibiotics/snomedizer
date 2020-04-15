

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
  expect_error(snomedizer_options_set(endpoint = "http://detectportal.firefox.com/success.txt"))


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
#
#
# test_that("snowstorm_search_term", {
#   expect_warning(asthma_concepts <- snowstorm_search_term("asthma"))
#   expect_true("195967001" %in% purrr::map_chr(asthma_concepts$items, "conceptId"))
# })
#
#
# test_that("snowstorm_fetch_concepts", {
#   asthma_concepts <- snowstorm_fetch_concepts("195967001")
#   expect_equal(asthma_concepts$items[[1]]$fsn$term, "Asthma (disorder)")
# })
#
#
# test_that("snowstorm_fetch_children", {
#   expect_warning(asthma_concepts <- snowstorm_fetch_children(c("195967001")))
#   expect_true("304527002" %in% asthma_concepts$conceptId)
# })

