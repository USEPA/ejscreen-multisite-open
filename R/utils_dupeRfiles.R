dupeRfiles <- function(folder1 = './R', folder2 = '../EJAM/R') {
  cat("Comparing .R files in ", folder1, ", to the files in ", folder2, "\n\n")
  docs1 <- list.files(folder1)
  docs2 <- list.files(folder2)
  # print("Only in folder1: ")
  # cat('\n\n')
  # print(setdiff(docs1, docs2))
  # cat('\n\n')
  # print("Only in folder2: ")
  # cat('\n\n')
  # print(setdiff(docs2, docs1))
  
  both <- intersect(docs1, docs2)
  # cat('\n\n')
  # print("Shared files: ")
  # cat('\n\n')
  # print( both); cat('\n\n')
  
  x <- list()
  for (fname in both) {
    x[[fname]] <-  ifelse(identical(
      readLines(file.path(folder1, fname)), 
      readLines(file.path(folder2, fname))
    ) , "identical", "differ") 
  }
  cat('\n\n')
  out <- data.frame(filename = both, identical = unlist(as.vector(x)))
  out <- out[order(out$identical), ]
  rownames(out) <- NULL
  return(out)
}
