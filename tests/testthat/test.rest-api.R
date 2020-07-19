


test_that("findConcept", {
  expect_equal(httr::content(api_find_concept(conceptId = "233604007"))$fsn$term, "Pneumonia (disorder)")
  expect_equal(
    expect_warning(httr::content(api_find_concept(conceptId = "biduletruccestleurtruc")))$error,
    "NOT_FOUND")
  expect_warning(
    api_find_concept(conceptId = c("233604007", "68566005"))
  )
})


test_that("findConcepts", {
  # by term
  pneumo_term <- api_find_concepts(term = "pneumonia")
  pneumo_term_codes <- sapply(httr::content(pneumo_term)[["items"]], function(X) X$conceptId)
  expect_true("233604007" %in% pneumo_term_codes)

  # one concept code
  infection_id <- api_find_concepts(conceptIds = "233604007")
  infection_id_code <- sapply(httr::content(infection_id)[["items"]], function(X) X$conceptId)
  expect_equal(infection_id_code, "233604007")

  # several concept codes
  infections_id <- api_find_concepts(conceptIds = c("233604007", "68566005"))
  infections_id_codes <- sapply(httr::content(infections_id)[["items"]], function(X) X$conceptId)
  expect_setequal(infections_id_codes, c("233604007", "68566005"))

  # additional arguments from `...`
  # `Pneumonia` is a descendant of `Clinical finding`
  infection_id <- api_find_concepts(conceptIds = "233604007", ecl = "<404684003|Clinical finding|")
  infection_id_code <- sapply(httr::content(infection_id)[["items"]], function(X) X$conceptId)
  expect_equal(infection_id_code, "233604007")
  # `Pneumonia` is not an ancestor of `Clinical finding`
  infection_id <- api_find_concepts(conceptIds = "233604007",  ecl = ">404684003|Clinical finding|")
  infection_id_code <- sapply(httr::content(infection_id)[["items"]], function(X) X$conceptId)
  expect_equal(infection_id_code, list())
})



test_that("api_all_branches", {
  expect_equal(httr::content(api_all_branches())[[1]][["path"]],
               "MAIN")
})

test_that("api_branch", {
  expect_equal(httr::content(api_branch())[["path"]],
               "MAIN")
})

test_that("api_branch_descendants", {
  expect_true(exists("path", httr::content(api_branch_descendants())[[1]]))
})


test_that("api_descriptions", {
  expect_equal(
    httr::content(api_descriptions(concept = "233604007"))[["items"]][[1]][["conceptId"]],
    "233604007"
  )
})

test_that("api_version", {
  expect_true(exists("version", httr::content(api_version())))
})

test_that("api_descriptions", {
  pneumo_no_desc <- api_browser_concepts(conceptId = "233604007",
                                         descendantCountForm = NULL) %>%
    httr::content()
  expect_false(exists("descendantCount", pneumo_no_desc))
  expect_equal(pneumo_no_desc$conceptId, "233604007")


  pneumo_stated_desc <- api_browser_concepts(conceptId = "233604007",
                                         descendantCountForm = "stated") %>%
    httr::content()
  expect_true(exists("descendantCount", pneumo_stated_desc))
  expect_equal(pneumo_stated_desc$conceptId, "233604007")

  expect_error(api_browser_concepts())
  expect_error(api_browser_concepts(conceptId = "233604007",
                                    descendantCountForm = "biduletruccestleurtruc"))
})


test_that("api_browser_concept_ancestors", {

  pneumo <- httr::content(api_browser_concept_ancestors(conceptId = "233604007")) %>%
    purrr::map(~dplyr::as_tibble(.x))%>% dplyr::bind_rows()
  expect_true("609623002" %in% pneumo$conceptId)

  expect_equal(
    httr::content(api_browser_concept_ancestors(conceptId = "233604007",
                                                form = NULL) ),
    httr::content(api_browser_concept_ancestors(conceptId = "233604007"))
  )
  expect_error(api_browser_concept_ancestors(conceptId = "233604007",
                                             form = "biduletruccestleurtruc"))
})

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
