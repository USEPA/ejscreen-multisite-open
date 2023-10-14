test_that("getblocks_summarize_blocks_per_site does not crash", {
  expect_no_error(
    getblocks_summarize_blocks_per_site(testoutput_getblocksnearby_1000pts_1miles)
  )
})

test_that("getblocks_summarize_sites_per_block does not crash", {
  expect_no_error({
    # if (exists("getblocks_summarize_sites_per_block")) { # had not been exported in a previous version of code
    getblocks_summarize_sites_per_block(testoutput_getblocksnearby_1000pts_1miles)
    # }
  })
})

test_that("getblocks_diagnostics does not crash", {
  expect_no_error(
    getblocks_diagnostics(testoutput_getblocksnearby_1000pts_1miles)
  )
})

