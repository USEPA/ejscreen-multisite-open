# datacreate_censusplaces
 
# stop("see source/notes")
# 
# warning("SOMETHING IS WRONG WITH THE COUNTY AND CO_FIPS COLS OF THE censusplaces dataset? ")

# SOME CITIES/PLACES OVERLAP WITH 2+ COUNTIES
# and also the FIPS here is NOT a way to create the countyfips !!

# censusplaces[12,]
# STATE STATEFP PLACEFP    PLACENAME               TYPE FUNCSTAT                       COUNTY
# 1:    AL      01   01660 Altoona town Incorporated Place        A Blount County, Etowah County
# > 
  # 
  # cbind(censusplaces$COUNTY[1:100], tolower(paste(censusplaces$COUNTY[1:100], censusplaces$STATE[1:100], sep = ",")) %in% tolower(paste(blockgroupstats$countyname, blockgroupstats$ST, sep = ",")))
  
  
  # unique(censusplaces[censusplaces$STATE == "DE", 1:5])
  # EPA_REGION STATE ST_FIPS                         COUNTY CO_FIPS
  # 4222          3    DE      10              New Castle County   10014
  # 4223          3    DE      10              New Castle County   10015
  # 4224          3    DE      10              New Castle County   10016
  # 4225          3    DE      10              New Castle County   10041
  # 4226          3    DE      10              New Castle County   10046
# 4227          3    DE      10                  Sussex County   10056
# 4228          3    DE      10                  Sussex County   10058
# 4229          3    DE      10                  Sussex County   10067




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


# to be able to filter all places/cities to view and select just those within at least partially within 
#  a select list of counties, you would need to make longer the censusplaces table to have separate rows for all county-place pairs !
# seems like more work than it is worth - You can just filter on states and select all relevant counties
# and maybe we just show each place as being within the first of the listed counties to simplify it??
# need to clean COUNTY column of censusplaces table to have just one entry,
# then use 
# tolower(paste(censusplaces$COUNTY[1:100], censusplaces$STATE[1:100], sep = ","))
# to match on state and name of  
# tolower(paste(blockgroupstats$countyname, blockgroupstats$ST, sep = ","))
#  to get county fips for each  substr(blockgroupstats$bgfips,1,5))] 

fips_counties_from_countyname <- function(countyname, ST) {
  co_st <- tolower(paste(countyname, ST, sep = ","))
  bgs_co_st <- unique(data.frame(countyfips = substr(blockgroupstats$bgfips,1,5), fullname = tolower(paste(blockgroupstats$countyname, blockgroupstats$ST, sep = ","))) )
  countyfips  <- bgs_co_st$countyfips[match(co_st, bgs_co_st$fullname)]
}
#  *** SAVE ONLY THE FIRST COUNTY LISTED WHEN THERE ARE MULTIPLE ONES THAT CONTAIN A GIVEN CENSUS PLACE, TO SIMPLIFY THIS
censusplaces$COUNTY <- gsub(",.*", "", censusplaces$COUNTY)
  
# fix issue of encoding characters  "Do\xf1a Ana County" "NM"  ***   ISO-8859-1  ??  stringi::stri_enc_detect(censusplaces$COUNTY)
stop('fix encoding first')
countyfips  <- fips_counties_from_countyname(censusplaces$COUNTY, censusplaces$STATE)

censusplaces[ , CO_FIPS := as.integer(..countyfips)]
censusplaces[ , countyfips := NULL]

censusplaces[ , PL_FIPS := as.integer(fips)]   # saves 3 MB 
censusplaces[ , fips := NULL]
setnames(censusplaces, old = "PLACENAME", new = "PLACE")


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

