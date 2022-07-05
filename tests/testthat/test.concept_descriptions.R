test_that("concept_descriptions", {
  infection_descriptions <- concept_descriptions(
    conceptIds =  c("68566005", "233604007")
  )
  expect_equal(
    names(infection_descriptions),
    c("233604007", "68566005")
  )
  expect_true("Pneumonia" %in% infection_descriptions[["233604007"]]$term)
  expect_true("Urinary tract infectious disease" %in% infection_descriptions[["68566005"]]$term)
  expect_error(concept_descriptions(""))
})

test_that("concept_descriptions (batch)", {

  concepts <- concept_find(ecl = "<233604007", limit = 300)

  infection_descriptions <- concept_descriptions(
    conceptIds = concepts$conceptId,
    limit = 500,
    silent = TRUE
  )
  infection_descriptions <- concept_descriptions(
    conceptIds = concepts$conceptId,
    limit = 500,
    silent = FALSE
  )
  expect_equal(
    sort(names(infection_descriptions)),
    sort(concepts$conceptId)
  )

  # Test when limit is overridden by long conceptId input
  infection_descriptions_batch_limit_50 <- concept_descriptions(
    conceptIds = concepts$conceptId,
    limit = 50,
    silent = FALSE
  )
  expect_equal(sort(names(infection_descriptions_batch_limit_50)),
               sort(concepts$conceptId))
})
