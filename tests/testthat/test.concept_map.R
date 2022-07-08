test_that("concept_map", {
  map_icd10 <- concept_map(concept_ids = "431308006", map_refset_id = NULL)
  expect_true(all(
    map_icd10$referencedComponent.conceptId == "431308006"
  ))
  expect_equivalent(
    sort(map_icd10$refsetId),
    sort(c("733073007", "447562003", "900000000000497000"))
  )
})

test_that("concept_map (batch)", {
  infection_codes <- suppressWarnings(concept_find(ecl = "<40733004", limit = 150))
  map_icd10 <- concept_map(concept_ids = infection_codes$conceptId, limit = 500)
  expect_true(all(
    map_icd10$refsetId == "447562003"
  ))

  # Test when limit is overridden by long conceptId input
  map_icd10_limit_50 <- concept_map(concept_ids = infection_codes$conceptId, limit = 50)
  expect_equal(
    map_icd10$refsetId,
    map_icd10_limit_50$refsetId
  )

})

test_that("concept_map no equivalent", {
  map_amoxicillin <- concept_map(concept_ids = "372687004")
  expect_null(map_amoxicillin)
})
