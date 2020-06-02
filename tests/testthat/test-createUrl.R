context("createUrl")

if (FALSE) {
  devtools::test(filter = "createUrl")
}

test_that("createUrl should create url without params", {
  expect_equal(createUrl("/endpoint"), "http://www.registeruz.sk/cruz-public/api/endpoint")
})

test_that("createUrl should create url with params", {
  expect_equal(createUrl("/endpoint", a = 5, b = 10), "http://www.registeruz.sk/cruz-public/api/endpoint?a=5&b=10")
})
