
cat("NEED TESTS WRITTEN FOR fixcolnames()\n\n")

testthat::test_that("fixcolnames() works at all", {
  
  testthat::expect_no_error({
    
    rtest <- c("pm", "no2", "pctlowinc", "NOTVALIDRNAME")
    oldtest <-  c("RAW_D_INCOME", "ACSTOTPOP", "NOTVALIDRNAME")
    
    fixcolnames(oldtest) # assumes it was in original format as API outputs
    fixcolnames(oldtest, "csv", newtype = "shortlabel")
    fixcolnames(oldtest, "api", newtype = "short")
    
    fixcolnames(oldtest, "original", "r")
    
    fixcolnames(rtest, 'r', 'short')
    fixcolnames(rtest, 'r', newtype = "long")
    fixcolnames(rtest, 'r', newtype = "api")
    fixcolnames(rtest, 'r', newtype = "varlist")
   
  })

})

