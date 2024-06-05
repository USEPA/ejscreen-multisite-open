test_that("ejamit_compare_types_of_places works", {
  expect_no_error({
    
    # slow
    suppressWarnings({
      suppressMessages({
        cat("running ejamit_compare_types_of_places() to test it...\n")
        # junk = tempfile()
        # sink(file = junk)
        junk <- capture_output({
          # on.exit(sink(NULL))
          out <- ejamit_compare_types_of_places(
            testpoints_10[1:3, ], radius = 1,
            typeofsite = c("A", "B", "B")
          )
        })
        # sink(NULL)
      })
    })
    
  })
})


