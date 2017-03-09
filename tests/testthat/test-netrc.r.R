# Unit tests for netrc files
# 
# Author: Renaud Gaujoux
###############################################################################

context("netrc")

test_that("netrcfile are correctly parsed", {

#  expect_equal('Not yet implemented', '')
      
})

test_that("machine patterns are correctly matched", { 
  
  expect_equal(match_url("abc.com", 'aaa.com'), setNames(NA_integer_, "abc.com"))
  expect_equal(match_url("abc.com", 'abc.com'), setNames(1L, "abc.com"))
  expect_equal(match_url("abc.com/ab", 'abc.com'), setNames(1L, "abc.com/ab"))
  expect_equal(match_url("abc.com/ab/", 'abc.com'), setNames(1L, "abc.com/ab/"))
  expect_equal(match_url("abc.com/ab/", 'abc.com/'), setNames(1L, "abc.com/ab/"))
  # fixed names does not match longer final element
  expect_equal(match_url("abc.com/ab", 'abc.com/a'), setNames(NA_integer_, "abc.com/ab"))
  # regexp pattern is honoured
  expect_equal(match_url("abc.com/ab", 'abc.com/a.*'), setNames(1L, "abc.com/ab"))
  expect_equal(match_url("abc.com/ab/a", 'abc.com/a.*'), setNames(1L, "abc.com/ab/a"))
  expect_equal(match_url("abc.com/a", 'abc.com/a.*$'), setNames(1L, "abc.com/a"))
  expect_equal(match_url("abc.com", 'abc.com/a*$'), setNames(1L, "abc.com"))
  expect_equal(match_url("abc.com/ac", 'abc.com/a*$'), setNames(NA_integer_, "abc.com/ac"))
  expect_equal(match_url("abc.com/ac", 'abc.com/ac$'), setNames(1L, "abc.com/ac"))
  expect_equal(match_url("abc.com/acd", 'abc.com/ac$'), setNames(NA_integer_, "abc.com/acd"))
  # case insensitive
  expect_equal(match_url("ABC.com", 'abc.com'), setNames(1L, "ABC.com"))
  expect_equal(match_url("abc.com", 'ABC.com'), setNames(1L, "abc.com"))
  # vectorized
  expect_equal(match_url("abc.com", c('aaa', 'abc.com')), setNames(2L, "abc.com"))
  expect_equal(match_url("abc.com", c('aaa', 'ab.*\\.com')), setNames(2L, "abc.com"))
  # regular expression
  expect_equal(match_url(c("abc.com", 'def.org'), c('aaa', 'ab.*\\.com')), setNames(c(2L, NA), c("abc.com", 'def.org')))
  expect_equal(match_url(c("abc.com", 'abc.org'), c('abc.((com)|(org))', 'aaa')), setNames(c(1L, 1L), c("abc.com", 'abc.org')))
  # sub-domain are matched unless explicitely disabled by regular expression
  expect_equal(match_url("d.abc.com", 'abc.com'), setNames(1L, "d.abc.com"))
  expect_equal(match_url("d.abc.com", '//abc.com'), setNames(NA_integer_, "d.abc.com"))
  expect_equal(match_url("b/abc.com", 'abc.com'), setNames(NA_integer_, "b/abc.com"))
  
})
