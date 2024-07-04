
 # report all dependencies and downstream ones etc.
# 
# x <- dependencies_of_ejam()
# y <- sort(packrat:::recursivePackageDependencies('EJAM', lib.loc = .libPaths(), ignores = NULL))
# setdiff(y, x)
# ## [1] "snow"
# 
# only_due_to_EJAMejscreenapi      = setdiff(
#   dependencies_of_ejam("EJAMejscreenapi"), 
#   dependencies_of_ejam("EJAM", ignores_grep = "EJAMejscreenapi"))
# only_due_to_EJAMbatch.summarizer = setdiff(
#   dependencies_of_ejam("EJAMbatch.summarizer"), 
#   dependencies_of_ejam("EJAM", ignores_grep = "EJAMbatch.summarizer"))
# length(only_due_to_EJAMejscreenapi)
# length(only_due_to_EJAMbatch.summarizer)


dependencies_of_ejam <- function(localpkg = "EJAM", depth = 6, ignores_grep = "0912873410239478") {
  
  cat(" 
      sort(packrat:::recursivePackageDependencies('EJAM', lib.loc = .libPaths(), ignores = NULL))
      would do something similar.
      \n")  
  
  cat("
      dependencies_of_ejam(ignores_grep = 'EJAM' )
      would ignore other EJAM-prefixed package names.
      \n")
  
  sort( 
    unique( 
      grep(
        ignores_grep, 
        deepdep::deepdep(
          localpkg, 
          local = TRUE, 
          downloads = FALSE, 
          depth = depth
        )$name, 
        value = TRUE, 
        invert = TRUE)
    )
  )
}
