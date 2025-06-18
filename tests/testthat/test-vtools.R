test_that("vparse handles different input formats correctly", {
  expect_equal(vparse(Treatment, Type), c("Treatment", "Type"))
  expect_equal(vparse(c("Treatment", "Type")), c("Treatment", "Type"))
  expect_equal(vparse(~Treatment + Type), c("Treatment", "Type"))
})

test_that("vselect selects correct columns from CO2", {
  data(CO2)
  selected <- vselect(CO2, Treatment, Type)
  expect_true(all(c("Treatment", "Type") %in% names(selected)))
  expect_equal(ncol(selected), 2)
})

test_that("vcheck confirms variables exist", {
  data(CO2)
  expect_true(vcheck(CO2, Treatment, Type))
  expect_true(vcheck(CO2, ~Treatment + Type))
})

test_that("vcheck throws error for missing variables", {
  data(CO2)
  expect_error(vcheck(CO2, Nonexistent), "Missing variables")
})

test_that("vmap applies function to variable names", {
  result <- vmap(~Treatment + Type, toupper)
  expect_equal(result, list("TREATMENT", "TYPE"))
})

test_that("vrename renames columns correctly", {
  data(CO2)
  renamed <- vrename(CO2, c(Treatment = "Trt", Type = "Group"))
  expect_true(all(c("Trt", "Group") %in% names(renamed)))
  expect_false("Treatment" %in% names(renamed))
})
