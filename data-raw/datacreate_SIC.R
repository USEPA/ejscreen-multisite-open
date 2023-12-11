###################################################################################
# create SIC ####
###################################################################################

# list of SIC codes
# from https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/sic-code-descriptions/sic88_97.txt

library(magrittr)

download.file(
  "https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/sic-code-descriptions/sic88_97.txt", 
  destfile = "sic88_97.txt"
)
sic_txt <- readr::read_delim(
  'sic88_97.txt', 
  skip = 1, delim = '  ', col_names = c('code','desc')
)
sic_txt$desc <- gsub(
  "Dry, condensed, evaporated  products",  # fix this row that had double space error
  "Dry, condensed, evaporated products", 
  sic_txt$desc
)
file.remove("sic88_97.txt")
sic_txt <- sic_txt %>% dplyr::mutate(code = gsub("[^0-9]+", "", code))

## separate 2- and 3-digit codes
sic_cats2 <- sic_txt %>% dplyr::filter(nchar(code) == 2) #filter(str_detect(code,'--'))
sic_cats3 <- sic_txt %>% dplyr::filter(nchar(code) == 3)#filter(str_detect(code,'\\'))

## save remaining codes as vector for list used in app
sic_cats4 <- sic_txt %>% dplyr::filter(nchar(code) == 4) 

sictable <- sic_cats4
#saveRDS(sic_cats4, file = 'SIC_vec.rds')
#usethis::use_data(sictable)

## create named list for use in dropdown menu
SIC <- sic_cats4 %>% 
  dplyr::mutate(code_and_desc = paste(code, desc, sep = ' - ')) %>% 
  dplyr::select(code_and_desc, code) %>% 
  ## turn 2-column df into named list (col1 = names, col2 = values)
  tibble::deframe() 
#saveRDS(SIC, file='SIC.rds')

attr(SIC, "date_downloaded") <- Sys.Date()

usethis::use_data(SIC)

