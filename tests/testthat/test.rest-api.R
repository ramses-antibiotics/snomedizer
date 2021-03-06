
# api_concept --------------------------------------------------------

test_that("api_concept", {
  expect_equal(httr::content(api_concept(conceptId = "233604007"))$fsn$term, "Pneumonia (disorder)")
  expect_equal(
    expect_warning(httr::content(api_concept(conceptId = "biduletruccestleurtruc")))$error,
    "NOT_FOUND")
  expect_error(
    api_concept(conceptId = c("233604007", "68566005"))
  )
})

# api_concepts -------------------------------------------------------

test_that("api_concepts", {
  # by term
  pneumo_term <- api_concepts(term = "pneumonia")
  pneumo_term_codes <- sapply(httr::content(pneumo_term)[["items"]], function(X) X$conceptId)
  expect_true("233604007" %in% pneumo_term_codes)

  # one concept code
  infection_id <- api_concepts(conceptIds = "233604007")
  infection_id_code <- sapply(httr::content(infection_id)[["items"]], function(X) X$conceptId)
  expect_equal(infection_id_code, "233604007")

  # several concept codes
  infections_id <- api_concepts(conceptIds = c("233604007", "68566005"))
  infections_id_codes <- sapply(httr::content(infections_id)[["items"]], function(X) X$conceptId)
  expect_setequal(infections_id_codes, c("233604007", "68566005"))

  # additional arguments from `...`
  # `Pneumonia` is a descendant of `Clinical finding`
  infection_id <- api_concepts(conceptIds = "233604007", ecl = "<404684003|Clinical finding|")
  infection_id_code <- sapply(httr::content(infection_id)[["items"]], function(X) X$conceptId)
  expect_equal(infection_id_code, "233604007")
  # `Pneumonia` is not an ancestor of `Clinical finding`
  infection_id <- api_concepts(conceptIds = "233604007",  ecl = ">404684003|Clinical finding|")
  infection_id_code <- sapply(httr::content(infection_id)[["items"]], function(X) X$conceptId)
  expect_equal(infection_id_code, list())

  uti_children <- sapply(httr::content(api_concepts(ecl = "<!68566005"))[["items"]],
                         function(X) X$conceptId)
  expect_true("422747000" %in% uti_children)


  # invalid arguments
  expect_error(api_concepts(conceptIds = "422747000", limit = "blurgh"))
  expect_error(api_concepts(conceptIds = "422747000", limit = "400000"))
  expect_warning(api_concepts(ecl = "blur&*("))
  expect_warning(api_concepts(conceptId = "233604007", limit = 100000))
})


# api_concept_descendants -------------------------------------------------

test_that("api_concept_descendants", {
  pneumo_desc <- httr::content(api_concept_descendants("205237003"))
  pneumo_desc_stated <- httr::content(api_concept_descendants("205237003",
                                                              stated = T))
  expect_true(exists("conceptId", pneumo_desc$items[[1]]))
  expect_true(pneumo_desc_stated$total < pneumo_desc$total)
  expect_error(api_concept_descendants(conceptId = c("a", "b")))
})


# api_concept_descriptions ------------------------------------------------

test_that("api_concept_descriptions", {
  pneumo_descr <- httr::content(api_concept_descriptions("233604007"))
  expect_equal(
    pneumo_descr$conceptDescriptions[[1]]$descriptionId,
    "350049016"
  )
})
# api_all_branches --------------------------------------------------------

test_that("api_all_branches", {
  expect_equal(httr::content(api_all_branches())[[1]][["path"]],
               "MAIN")
})

# api_branch --------------------------------------------------------------

test_that("api_branch", {
  expect_equal(httr::content(api_branch())[["path"]],
               "MAIN")
})

# api_branch_descendants --------------------------------------------------

test_that("api_branch_descendants", {
  expect_true(exists("path", httr::content(api_branch_descendants())[[1]]))
})

# api_descriptions --------------------------------------------------------

test_that("api_descriptions", {
  expect_setequal(
    snomedizer::result_flatten(api_descriptions(conceptIds = c("233604007")))$descriptionId,
    c("350049016", "621810017")
  )
  expect_setequal(
    snomedizer::result_flatten(api_descriptions(conceptIds = c("233604007", "205237003")))$descriptionId,
    c("314740018", "350049016", "621810017", "590574014")
  )
})

# api_version -------------------------------------------------------------

test_that("api_version", {
  expect_true(exists("version", httr::content(api_version())))
})

# api_browser_concepts ----------------------------------------------------

test_that("api_browser_concepts", {
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

  expect_error(api_browser_concepts(conceptId = "233604007",
                                    descendantCountForm = "biduletruccestleurtruc"))
})

# api_browser_concept_ancestors -------------------------------------------

test_that("api_browser_concept_ancestors", {

  pneumo <- httr::content(api_browser_concept_ancestors(conceptId = "233604007")) %>%
    purrr::map(~dplyr::as_tibble(.x))%>% dplyr::bind_rows()
  expect_true("609623002" %in% pneumo$conceptId)
  expect_error(api_browser_concept_ancestors(conceptId = "233604007",
                                             form = NULL))
  expect_equal(
    httr::content(api_browser_concept_ancestors(conceptId = "233604007",
                                                form = "inferred") ),
    httr::content(api_browser_concept_ancestors(conceptId = "233604007"))
  )
  expect_error(api_browser_concept_ancestors(conceptId = "233604007",
                                             form = "biduletruccestleurtruc"))
})

# api_browser_concept_children --------------------------------------------

test_that("api_browser_concept_children", {

  pneumo_children <- httr::content(api_browser_concept_children(conceptId = "233604007"))
  expect_false(exists("descendantCount", pneumo_children[[1]]))
  expect_false(exists("isLeafStated", pneumo_children[[1]]))
  expect_true(exists("isLeafInferred", pneumo_children[[1]]))
  pneumo_children <- httr::content(api_browser_concept_children(conceptId = "233604007",
                                                                form = "stated",
                                                                includeDescendantCount = TRUE))
  expect_true(exists("descendantCount", pneumo_children[[1]]))
  expect_true(exists("isLeafStated", pneumo_children[[1]]) &
                pneumo_children[[1]]$isLeafStated)
  expect_false(exists("isLeafInferred", pneumo_children[[1]]))


  expect_error(api_browser_concept_children(conceptId = "233604007",
                                            includeDescendantCount = NA))
  expect_error(api_browser_concept_children(conceptId = "233604007",
                                            includeDescendantCount = "blah"))
  expect_error(api_browser_concept_children(conceptId = "233604007",
                                            includeDescendantCount = as.logical(NA)))
  expect_error(api_browser_concept_children(conceptId = "233604007",
                                            includeDescendantCount = NULL))

})


# api_browser_concept_parents ---------------------------------------------

test_that("api_browser_concept_parents", {

  pneumo_parents <- httr::content(api_browser_concept_parents(conceptId = "233604007",
                                                              form = "inferred"))
  # descendantCount = TRUE by default unlike snowstorm default.
  expect_true(exists("descendantCount", pneumo_parents[[1]]))
  expect_false(exists("isLeafStated", pneumo_parents[[1]]))
  expect_true(exists("isLeafInferred", pneumo_parents[[1]]))
  expect_equal(pneumo_parents[[1]]$conceptId, "205237003")
  pneumo_parents <- httr::content(api_browser_concept_parents(conceptId = "233604007",
                                                              form = "stated"))
  # descendantCount = TRUE by default unlike snowstorm default.
  expect_true(exists("descendantCount", pneumo_parents[[1]]))
  expect_true(exists("isLeafStated", pneumo_parents[[1]]))
  expect_false(exists("isLeafInferred", pneumo_parents[[1]]))
  expect_equal(pneumo_parents[[1]]$conceptId, "64572001")

  expect_error(api_browser_concept_parents(conceptId = "233604007",
                                            includeDescendantCount = NA))
  expect_error(api_browser_concept_parents(conceptId = "233604007",
                                            includeDescendantCount = "blah"))
  expect_error(api_browser_concept_parents(conceptId = "233604007",
                                            includeDescendantCount = as.logical(NA)))
  expect_error(api_browser_concept_parents(conceptId = "233604007",
                                            includeDescendantCount = NULL))

})


# api_browser_concept_descriptions ----------------------------------------

test_that("api_browser_concept_descriptions", {
  expect_error(api_browser_concept_descriptions(term = c("a", "b")))
  expect_error(api_browser_concept_descriptions(term = "a", active = "blah"))
  pneumo <- httr::content(api_browser_concept_descriptions("pneumonia"))
  expect_equal(pneumo$items[[1]]$concept$conceptId, "60363000")
  expect_equal(
    pneumo$totalElements,
    httr::content(
      api_browser_concept_descriptions("pneumonia",
                                       active = TRUE))$totalElements +
      httr::content(
        api_browser_concept_descriptions("pneumonia",
                                         active = FALSE))$totalElements
  )
})



# api_descriptions_semantic_tags ------------------------------------------

test_that("api_descriptions_semantic_tags", {
  tags <- httr::content(api_descriptions_semantic_tags())
  expect_true("core metadata concept" %in% names(tags))
})
