# datacreate_censusplaces
 

# library(data.table)  

myurl <- "https://www2.census.gov/geo/docs/reference/codes/PLACElist.txt"
censusplaces <- data.table::fread(myurl, sep = "|", header = TRUE, colClasses = list(character = 1:7))
censusplaces
# STATE|STATEFP|PLACEFP|PLACENAME|TYPE|FUNCSTAT|COUNTY
# AL|01|00100|Abanda CDP|Census Designated Place|S|Chambers County
censusplaces[ , FUNCSTAT := NULL]

censusplaces[ , fips := paste0(STATEFP, PLACEFP)] # adds 2 MB
censusplaces[ , PLACEFP := NULL]  # saves 2MB
setnames(censusplaces, old = "STATEFP", new = "ST_FIPS")
censusplaces[ , CO_FIPS := as.integer(substr(fips, 1, 5))]
censusplaces[ , PL_FIPS := as.integer(fips)]   # saves 3 MB 
censusplaces[ , fips := NULL]
setnames(censusplaces, old = "PLACENAME", new = "PLACE")

fips_st2eparegion <- function(stfips) {
  EJAM::stateinfo$REGION[match(stfips, EJAM::stateinfo$FIPS.ST)]
}

censusplaces[ , EPA_REGION := fips_st2eparegion(ST_FIPS)]

# censusplaces[ , .N, by = "TYPE"]
#                       TYPE      N
# 1: Census Designated Place  9,974
# 2:      Incorporated Place 19,540
# 3:      County Subdivision 11,900
# censusplaces[ , TYPE := as.factor(TYPE)]
censusplaces[ , TYPE := NULL]
gc()
# censusplaces

setcolorder(censusplaces, neworder = c("EPA_REGION", "STATE","ST_FIPS", "COUNTY", "CO_FIPS", "PLACE", "PL_FIPS"))

setDF(censusplaces)


censusplaces



attr(censusplaces, "date_created") <- Sys.Date()
usethis::use_data(censusplaces, overwrite = TRUE)

