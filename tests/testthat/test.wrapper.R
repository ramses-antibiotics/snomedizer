

# concepts_find -----------------------------------------------------------

test_that("concepts_find", {

  expect_warning(asthma_concepts <- concepts_find(term = "asthma"))
  expect_true("195967001" %in% asthma_concepts$conceptId)

  asthma_concepts <- concepts_find(conceptIds = "195967001")
  expect_true(dim(asthma_concepts)[1] == 1)
  expect_equal(asthma_concepts$fsn.term, "Asthma (disorder)")

  infection_concepts <- concepts_find(conceptIds =  c("233604007", "68566005"))
  expect_setequal(infection_concepts$conceptId, c("233604007", "68566005"))

  ecl_query_concept <- concepts_find(
    ecl = paste(
      "<! 68566005 | Urinary tract infectious disease (disorder) |",
      "AND",
      "< 87628006 | Bacterial infectious disease (disorder) |"
    )
  )
  expect_equal(ecl_query_concept$conceptId, "312124009")

  # test when both ecl and conceptIds are provided
  disjoint <- concepts_find(
    ecl = "<<40733004", #Infectious disease
    conceptIds = c("233604007", # Pneumonia
                   "40733004")
  )
  expect_equal(disjoint$conceptId, "40733004")

  included <- concepts_find(
    ecl = "<<40733004", #Infectious disease
    conceptIds = c("312342009", # INFECTIVE pneumonia
                   "40733004")
  )
  expect_equal(sort(included$conceptId),
               c("312342009", "40733004"))
})

test_that("concepts_find (batch)", {
  concepts <- concepts_find(ecl = "<233604007", limit = 300)
  concepts_batch <- concepts_find(
    conceptIds = concepts$conceptId,
    limit = 300,
    silent = TRUE
  )
  concepts_batch <- concepts_find(
    conceptIds = concepts$conceptId,
    limit = 300,
    silent = FALSE
  )
  expect_equal(sort(concepts$conceptId),
               sort(concepts_batch$conceptId))
})

# concepts_ancestors -----------------------------------------------------

test_that("concepts_ancestors (order preserving)", {
  infections <- concepts_ancestors(conceptIds = c("68566005", "233604007"),
                                    silent = TRUE)
  expect_equal(names(infections),
               c("68566005", "233604007"))
  infections <- concepts_ancestors(conceptIds = c("233604007", "68566005"),
                                    silent = TRUE)
  expect_equal(names(infections),
               c("233604007", "68566005"))
})

test_that("concepts_ancestors ", {
  infections <- concepts_ancestors(conceptIds = c("233604007", "68566005"),
                                    silent = FALSE)
  expect_true(
    "205237003" %in% #pneumonitis
      infections[[1]]$conceptId
  )
  expect_false(
    "53084003" %in% #bacterial pneumonia
      infections[[1]]$conceptId
  )
  expect_false(
    "40733004" %in% #Infectious disease
      infections[[1]]$conceptId
  )
  expect_true(
    "40733004" %in% #Infectious disease
      infections[[2]]$conceptId
  )
  expect_warning(concepts_ancestors(conceptIds = c("233604007", "68566005"), limit = 2))
  expect_is(
    infections <- concepts_ancestors(conceptIds = c("233604007", "68566005"), limit = 300),
    "list"
  )

  expect_false("68566005" %in% infections$`68566005`$conceptId)
  expect_false("233604007" %in% infections$`233604007`$conceptId)
})


test_that("concepts_ancestors (include_self)", {
  infections <- concepts_ancestors(conceptIds = c("68566005", "233604007"),
                                    include_self = TRUE,
                                    silent = TRUE)
  expect_true("68566005" %in% infections$`68566005`$conceptId)
  expect_true("233604007" %in% infections$`233604007`$conceptId)
})


# concepts_descendants ----------------------------------------------------

test_that("concepts_descendants", {

  infections <- concepts_descendants(conceptIds = c("68566005", "233604007"),
                                     silent = TRUE,
                                     limit = 250)
  expect_true("882784691000119100" %in% infections$`233604007`$conceptId)
  expect_true("1469007" %in% infections$`68566005`$conceptId)
  expect_true("422747000" %in% infections$`68566005`$conceptId)
  expect_warning(concepts_descendants(conceptIds = c("233604007", "68566005"), limit = 2))
  infections <- concepts_descendants(conceptIds = c("233604007", "68566005"), limit = 300)
  expect_true("882784691000119100" %in% infections$`233604007`$conceptId)

  expect_warning(concepts_descendants(conceptIds = "blurgh"))

  expect_false("68566005" %in% infections$`68566005`$conceptId)
  expect_false("233604007" %in% infections$`233604007`$conceptId)
})

test_that("concepts_descendants (include_self)", {

  infections <- concepts_descendants(conceptIds = c("68566005", "233604007"),
                                     include_self = TRUE,
                                     silent = TRUE,
                                     limit = 250)
  expect_true("68566005" %in% infections$`68566005`$conceptId)
  expect_true("233604007" %in% infections$`233604007`$conceptId)
})

# concepts_descriptions ---------------------------------------------------

test_that("concepts_descriptions", {
  infection_descriptions <- concepts_descriptions(
    conceptIds =  c("68566005", "233604007")
  )
  expect_equal(
    names(infection_descriptions),
    c("233604007", "68566005")
  )
  expect_true("Pneumonia" %in% infection_descriptions[["233604007"]]$term)
  expect_true("Urinary tract infectious disease" %in% infection_descriptions[["68566005"]]$term)
  expect_error(concepts_descriptions(""))
})

test_that("concepts_descriptions (batch)", {

  concepts <- concepts_find(ecl = "<233604007", limit = 300)

  infection_descriptions <- concepts_descriptions(
    conceptIds = concepts$conceptId,
    limit = 500,
    silent = TRUE
  )
  infection_descriptions <- concepts_descriptions(
    conceptIds = concepts$conceptId,
    limit = 500,
    silent = FALSE
  )
  expect_equal(
    sort(names(infection_descriptions)),
    sort(concepts$conceptId)
  )

})


# concepts_map ------------------------------------------------------------

test_that("concepts_map single concept", {
  map_icd10 <- concepts_map(concept_ids = "431308006", map_refset_id = NULL)
  expect_true(all(
    map_icd10$referencedComponent.conceptId == "431308006"
  ))
  expect_equivalent(
    sort(map_icd10$refsetId),
    sort(c("733073007", "447562003", "900000000000497000"))
  )
})

test_that("concepts_map many concepts", {
  infection_codes <- suppressWarnings(concepts_find(ecl = "<40733004", limit = 150))
  map_icd10 <- concepts_map(concept_ids = infection_codes$conceptId, limit = 500)
  expect_true(all(
    map_icd10$refsetId == "447562003"
  ))
})

test_that("concepts_map no equivalent", {
  map_amoxicillin <- concepts_map(concept_ids = "372687004")
  expect_null(map_amoxicillin)
})


# release_version ---------------------------------------------------------

test_that("release_version", {
  ct_version <- release_version(branch = "MAIN/2021-07-31")
  expect_equal(ct_version$rf2_date, "20210731")
  expect_equal(ct_version$rf2_month_year, "July 2021")
})


