## unit tests for EJAM::naics_categories
## Author: Sara Sokolinski

# function is in the script frs_from_xyz.R
# my frequency table has more 2 digit naics (24) than the script documents (17)
# higher digit naics all have the same counts
table(nchar(NAICS))

# should digits = 1 give a warning? since it's empty
test_that('the function works for all digits',{
  expect_no_warning(val <- naics_categories(digits = 1))
  expect_no_warning(val <- naics_categories(digits = 2))
  expect_no_warning(val <- naics_categories(digits = 3))
  expect_no_warning(val <- naics_categories(digits = 4))
  expect_no_warning(val <- naics_categories(digits = 5))
  expect_no_warning(val <- naics_categories(digits = 6))
})

# how do we limit the response?
# filter EJAM data going in

test_that('entering filtered dataframe works', {
  df <- EJAM::NAICS %>% as.data.frame 
  df <- df %>% filter(startsWith(as.character(df$.), "11"))
  df <- df$.
  expect_no_warning(val <- naics_categories(digits = 2, dataset = df))
  expect_equal(length(val), 1)
  
}
)
