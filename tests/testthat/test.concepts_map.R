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
