#
# This test file has been generated by kwb.test::create_test_files()
# launched by user mrustl on 2021-02-26 14:17:36.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("read_weintek_batch() works", {
  f <- kwb.pilot:::read_weintek_batch

  expect_error(
    kwb.pilot:::read_weintek_batch()
    # Argument "files" fehlt (ohne Standardwert)
  )
})
