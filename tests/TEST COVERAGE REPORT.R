# !diagnostics off
##   disables diagnostics within this document

if (1 == 0) { # so it does not do anything if source() is done by testing 
  
# # Needed only for this script:
  # 
  library(fs)
  library(tidyverse)

tdat = bind_rows(
  tibble(
    type = "R",
    path = dir_ls("R/", regexp = "\\.[Rr]$"),
    name = as.character(path_ext_remove(path_file(path))),
  ),
  tibble(
    type = "test",
    path = dir_ls("tests/testthat/", regexp = "/test[^/]+\\.[Rr]$"),
    name = as.character(path_ext_remove(str_remove(path_file(path), "^test[-_]"))),
  )
) %>%
  pivot_wider(names_from = type, values_from = path) 

tdat %>%   print(n = Inf)

################################ # 

# see all TEST FILES that were not named based on a .R file, 
#   making it hard to know which .R files really lack tests

tdat[!is.na(tdat$test) & is.na(tdat$R),  ] |> print(n = 500)
 

################################ # 

# see all .R files that lack a test file with exactly matching name

tdat[is.na(tdat$test) & !is.na(tdat$R) & "data_" != substr(tdat$name, 1,5), ] |> print(n = 500)
 

################################ # 


# MATCHED EXACTLY -- see all test files that exactly match name of a .R file


tdat[!is.na(tdat$test) & !is.na(tdat$R), c("R", "test")] |> print(n = 500)



}
