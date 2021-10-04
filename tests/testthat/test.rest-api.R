
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
  uti_children <- httr::content(api_browser_concept_children(conceptId = "233604007"))
  # descendantCount = TRUE by default unlike snowstorm default.
  expect_true(exists("descendantCount", uti_children[[1]]))
  expect_false(exists("isLeafStated", uti_children[[1]]))
  expect_true(exists("isLeafInferred", uti_children[[1]]))
  uti_children <- httr::content(api_browser_concept_children(conceptId = "68566005",
                                                                form = "stated",
                                                                includeDescendantCount = FALSE))
  # descendantCount = TRUE by default unlike snowstorm default.
  expect_false(exists("descendantCount", uti_children[[1]]))
  expect_true(exists("isLeafStated", uti_children[[1]]) &
                uti_children[[1]]$isLeafStated)
  expect_false(exists("isLeafInferred", uti_children[[1]]))


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


# api_relationships -------------------------------------------------------

test_that("api_relationships", {
  #test that NULL is equivalent to c("NULL", "NULL")
  bacter_pneumo_relationships <- result_flatten(api_relationships(source = "312119006"))
  expect_true(all(c("116680003", "246075003", "370135005", "363698007") %in%
                    bacter_pneumo_relationships$type.conceptId))

  caused_by_ecoli <- result_flatten(api_relationships(type = "246075003", destination = "112283007"))
  expect_true(all(c("9323009", "10625111000119106") %in% caused_by_ecoli$source.conceptId))

  caused_by_ecoli <- result_flatten(api_relationships(type = "246075003", destination = "112283007",
                                                      characteristicType = "STATED_RELATIONSHIP"))
  expect_false("INFERRED_RELATIONSHIP" %in% caused_by_ecoli$characteristicType)

  expect_equal(api_relationships()$status_code, 200)
})


# api_relationship --------------------------------------------------------

test_that("api_relationship", {
  expect_error(api_relationship())
  is_a <- result_flatten(api_relationship(relationshipId = "1698297027"))
  expect_equal(is_a$source.conceptId, "312119006")
  expect_equal(is_a$type.conceptId, "116680003")
  expect_equal(is_a$target.conceptId, "50417007")
})


# api_all_code_systems ----------------------------------------------------

test_that("api_all_code_systems", {
  expect_error(api_all_code_systems(forBranch = c(1, 2)))
  expect_true("SNOMEDCT" %in% result_flatten(api_all_code_systems())$shortName)
  expect_equal(
    result_flatten(api_all_code_systems(forBranch = "MAIN"))$shortName,
    "SNOMEDCT"
  )
})


# api_code_system ---------------------------------------------------------

test_that("api_code_system", {
  expect_error(api_code_system(shortName = NULL))
  expect_error(api_code_system(shortName = c("a", "b")))
  expect_true(
    "SNOMEDCT" %in% result_flatten(api_code_system(shortName = "SNOMEDCT"))$shortName
  )
})


# api_code_system_all_versions --------------------------------------------

test_that("api_code_system_all_versions", {
  expect_error(api_code_system_all_versions(shortName = NULL))
  expect_error(api_code_system_all_versions(shortName = c("a", "b")))
  expect_true(
    "SNOMEDCT" %in% result_flatten(api_code_system_all_versions(shortName = "SNOMEDCT"))$shortName
  )
})


# api_browser_refset_members ---------------------------------------------

test_that("api_browser_refset_members includes ICD10 map", {
  refsets <- httr::content(api_browser_refset_members())
  refsets <- dplyr::bind_rows(lapply(refsets$referenceSets, as.data.frame))
  expect_true("447562003" %in% refsets$id)
})

test_that("api_browser_refset_members filtering by RefSet member", {
  refsets <- httr::content(api_browser_refset_members(referencedComponentId = "49436004"))
  refsets <- dplyr::bind_rows(lapply(refsets$referenceSets, as.data.frame))
  expect_true("447562003" %in% refsets$id)
})

test_that("api_browser_refset_members filtering by RefSet member and module", {
  refsets <- httr::content(api_browser_refset_members(
    referencedComponentId = "49436004",
    referenceSetModule = "<900000000000445007"
    ))
  refsets <- dplyr::bind_rows(lapply(refsets$referenceSets, as.data.frame))
  expect_true("447562003" %in% refsets$id)

  refsets <- httr::content(api_browser_refset_members(
    referencedComponentId = "49436004",
    referenceSetModule = "900000000000445007"
  ))
  expect_length(refsets$referenceSets, 0)
})

test_that("api_browser_refset_members filtering by RefSet member and RefSet", {
  refsets <- httr::content(api_browser_refset_members(
    referencedComponentId = "49436004",
    referenceSet = "447562003"
  ))
  refsets <- dplyr::bind_rows(lapply(refsets$referenceSets, as.data.frame))
  expect_equal(refsets$id, "447562003")
})

# api_refset_members -----------------------------------------------------

test_that("api_refset_members find all concepts within ICD N39.0 urinary tract inf", {
  uti_concepts <- httr::content(api_refset_members(
    mapTarget = "N39.0",
    referenceSet = "447562003"
  ))
  uti_concepts <- dplyr::bind_rows(lapply(uti_concepts$items, as.data.frame))
  expect_true(all(
    c("61373006", "4800001") %in% uti_concepts$referencedComponent.conceptId
  ))
})

test_that("api_members find ICD code(s) corresponding to bacteriuria", {
  bacteriuria <- httr::content(api_refset_members(
    referenceSet = "447562003",
    referencedComponentId = "61373006"
  ))
  bacteriuria <- dplyr::bind_rows(lapply(bacteriuria$items, as.data.frame))
  expect_equal(
    bacteriuria$additionalFields.mapTarget,
    "N39.0"
  )
})

