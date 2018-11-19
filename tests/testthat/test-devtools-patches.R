# Unit tests for utility functions
#
# Author: Renaud Gaujoux
###############################################################################

context('devtools patches')

test_that("patches return correct values on public repos", {

  rem <- remotes:::github_remote('renozao/repotools')
  # public SHA is not NA
  expect_true( !is.na(remotes:::remote_sha(rem)), 'SHA from repotools is not NA')
  # public package name is correct
  expect_identical(remotes:::remote_package_name(rem), 'repotools', 'Public package name is correctly fetched')

})
