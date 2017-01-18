# Unit tests for utility functions
# 
# Author: Renaud Gaujoux
###############################################################################

context('Utility functions')

test_that("package_remote_type correctly parse package specifications", {
  
  # default without slash is CRAN
  expect_identical(package_remote_type('a'), 'cran::a')
  # default with slash is github
  expect_identical(package_remote_type('a/b'), "github::a/b")
  # remote type is correctly extracted
  expect_identical(package_remote_type('other::a/b'), "other::a/b")
  
})