
#' Script to download NAICS file with code and name of sector
#'
#' See source code. Mostly just a short script to get the 2017 or 2022 codes and names.
#' See <'https://www.census.gov/naics/?48967'>
#' @param year which vintage of NAICS codes to use, 2012, 2017, or 2022
#' @param urlpattern full url of xlsx file to use, but with YYYY instead of year
#' @param destfile full path and name of file to save as locally
#'
#' @return names list with year as an attribute
#' 
#' @keywords internal
#'
naics_download <- function(year=2017, urlpattern='https://www.census.gov/naics/YYYYNAICS/2-6%20digit_YYYY_Codes.xlsx', destfile= paste0('~/Downloads/', year, 'NAICS.xlsx')) {
  # this can be used to create the NAICS dataset as for this package
  # See \url{https://www.census.gov/naics/}
  if (!(year %in% c(2012, 2017, 2020))) {warning('only works for 2012, 2017, 2020')
    return(NULL)  
  }
  url <- gsub('YYYY',year, urlpattern)
  if (year == 2012) {url <- gsub('6%20', '', url)} 
  # 'https://www.census.gov/naics/2022NAICS/2-6%20digit_2022_Codes.xlsx'
  # 'https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx'
  # 'https://www.census.gov/naics/2012NAICS/2-digit_2012_Codes.xls'
  download.file(
    url = url,
    destfile = destfile
  )
  x <- readxl::read_xlsx(path = destfile, skip = 2, col_names = c('n','code','title','b','c','d'))
  mynames <- paste(x$code, ' - ', x$title, sep = '')
  mycodes <- as.numeric(as.list(x$code))
  # mynames[(is.na(mycodes))]  # remove the ones that are ranges instead of being a 2-digit or longer code
  # ###  "31-33 - Manufacturing"  "44-45 - Retail Trade"   "48-49 - Transportation and Warehousing"
  NAICS        <- as.list(mycodes[!is.na(mycodes)])
  names(NAICS) <- mynames[!is.na(mycodes)]
  # table(as.numeric(sapply((NAICS), FUN=nchar)))
  # head(cbind(NAICS[substr(NAICS,1,2)=='31']))
  
  # save as NAICS dataset for package, but with year attribute indicating vintage:
  NAICS <- structure(NAICS, year = year)
  # attr(NAICS, 'year')
  # [1] 2017 # for example
  
  ################# #  ################# #  ################# #  ################# #
  ################# #  ################# #  ################# #  ################# #
  # ADDED IN 2023:
  # CREATES PLACEHOLDERS AT 2-DIGIT LEVEL SO THEY SHOW UP IN LISTING
  # SINCE THESE WERE NOT LISTED AT 2 DIGIT LEVEL IN ORIGINAL NAICS LIST USED HERE
  
  extrarows <- c(
    31,32,33, 
    44,45,  
    48,49
  )
  names(extrarows) <- c(
    "31 - Manufacturing",
    "32 - Manufacturing",
    "33 - Manufacturing",
    
    "44 - Retail Trade",
    "45 - Retail Trade",
    
    "48 - Transportation and Warehousing",
    "49 - Transportation and Warehousing"
  )
  
  NAICS <- c(NAICS, extrarows)
  # usethis::use_data(NAICS, overwrite = TRUE)
  
  # code	industry_title
  # 11	Agriculture, Forestry, Fishing and Hunting
  # 21	Mining
  # 22	Utilities
  # 23	Construction
  # 31-33	Manufacturing
  # 42	Wholesale Trade
  # 44-45	Retail Trade
  # 48-49	Transportation and Warehousing
  # 51	Information
  # 52	Finance and Insurance
  # 53	Real Estate Rental and Leasing
  # 54	Professional, Scientific, and Technical Services
  # 55	Management of Companies and Enterprises
  # 56	Administrative and Support and Waste Management and Remediation Services
  # 61	Educational Services
  # 62	Health Care and Social Assistance
  # 71	Arts, Entertainment, and Recreation
  # 72	Accommodation and Food Services
  # 81	Other Services (except Public Administration)
  # 92	Public Administration
  
  
  
  
  ################# #
  
  cat('To update source code R package, try usethis::use_data(NAICS.rdata) or just save(NAICS, file = \'yourpath/EJAM/data/NAICS.rdata\') \n')
  # usethis::use_data(NAICS.rdata, overwrite = TRUE)
  # ### not ### save(NAICS, file = './data/NAICS.rdata')
  
  return(NAICS)
}
