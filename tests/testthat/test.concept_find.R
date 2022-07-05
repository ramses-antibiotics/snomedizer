test_that("concept_find", {

  expect_warning(asthma_concepts <- concept_find(term = "asthma"))
  expect_true("195967001" %in% asthma_concepts$conceptId)

  asthma_concepts <- concept_find(conceptIds = "195967001")
  expect_true(dim(asthma_concepts)[1] == 1)
  expect_equal(asthma_concepts$fsn.term, "Asthma (disorder)")

  infection_concepts <- concept_find(conceptIds =  c("233604007", "68566005"))
  expect_setequal(infection_concepts$conceptId, c("233604007", "68566005"))

  ecl_query_concept <- concept_find(
    ecl = paste(
      "<! 68566005 | Urinary tract infectious disease (disorder) |",
      "AND",
      "< 87628006 | Bacterial infectious disease (disorder) |"
    )
  )
  expect_equal(ecl_query_concept$conceptId, "312124009")

  # test when both ecl and conceptIds are provided
  disjoint <- concept_find(
    ecl = "<<40733004", #Infectious disease
    conceptIds = c("233604007", # Pneumonia
                   "40733004")
  )
  expect_equal(disjoint$conceptId, "40733004")

  included <- concept_find(
    ecl = "<<40733004", #Infectious disease
    conceptIds = c("312342009", # INFECTIVE pneumonia
                   "40733004")
  )
  expect_equal(sort(included$conceptId),
               c("312342009", "40733004"))
})



test_that("concept_find (batch)", {
  concepts <- concept_find(ecl = "<233604007", limit = 300)
  concepts_batch <- concept_find(
    conceptIds = concepts$conceptId,
    limit = 300,
    silent = TRUE
  )
  concepts_batch <- concept_find(
    conceptIds = concepts$conceptId,
    limit = 300,
    silent = FALSE
  )
  expect_equal(sort(concepts$conceptId),
               sort(concepts_batch$conceptId))

  # Test when limit is overridden by long conceptId input
  concepts_batch_limit_50 <- concept_find(
    conceptIds = concepts$conceptId,
    limit = 50,
    silent = FALSE
  )
  expect_equal(sort(concepts$conceptId),
               sort(concepts_batch_limit_50$conceptId))
})
