test_that("split_by works with different input types", {
  data(CO2)

  # unquoted
  out1 <- split_by(CO2, Treatment)
  expect_s3_class(out1, "splitByData")
  expect_true(all(sapply(out1, function(df) !("Treatment" %in% names(df)))))

  # formula
  out2 <- split_by(CO2, ~Treatment + Type)
  expect_equal(length(out2), length(unique(paste(CO2$Treatment, CO2$Type))))

  # character vector
  out3 <- split_by(CO2, c("Treatment", "Type"))
  expect_equal(length(out3), length(out2))

  # do not omit group vars
  out4 <- split_by(CO2, Treatment, omit = FALSE)
  expect_true("Treatment" %in% names(out4[[1]]))
})
