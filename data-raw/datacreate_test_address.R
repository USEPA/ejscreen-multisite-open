
#          datacreate_test_address.R

##################### #

# test data:


# test_address_parts1
# test_addresses2
# test_addresses_9
# test_address_table_9
# test_address_table
# test_address_table_withfull
# test_address_table_goodnames
##################### #


# test data defined:


test_address_table_9 <- structure(list(
  FacilityID = c("17119110017423171", "18089110000397794", "18089110000397829", "18089110000398374", "18127110000607558",
                 "26163110027375668", "39017110000392557", "39035110011945681", "42003110001116934"),
  FacLong = c(-90.136, -87.45, -87.433, -87.328, -87.138, -83.157, -84.383, -81.673, -79.859),
  FacLat = c(38.694, 41.667, 41.671, 41.615, 41.637, 42.302, 39.483, 41.47, 40.395),
  FacilityName = c("US Steel-Granite City Works", "ArcelorMittal Indiana Harbor - West",
                   "ArcelorMittal Indiana Harbor - East", "US Steel-Gary Works",
                   "ArcelorMittal Burns Harbor LLC", "Dearborn-Works", "AK Steel Corporation – Middletown Works",
                   "ArcelorMittal Cleveland", "US Steel-Edgar Thompson"),
  Address = c("1951 State Street", "3001 Dickey Road", "3210 E Watling Street", "One North Broadway",
              "250 W. US Highway 12", "4001 Miller Road", "1801 Crawford Street",
              "3060 Eggers Avenue", "13th Street and Braddock Avenue"),
  City = c("Granite City", "East Chicago", "East Chicago", "Gary", "Burns Harbor", "Dearborn",
           "Middletown", "Cleveland", "Braddock"),
  State = c("IL", "IN", "IN", "IN", "IN", "MI", "OH", "OH", "PA"),
  ZipCode = c("62040", "46312", "46312", "46403", "46304-9745", "48120", "45043", "44105", "15104"),
  County = c("Madison County", "Lake County", "Lake County", "Lake County", "Porter County",
             "Wayne County", "Butler County", "Cuyahoga County", "Allegheny County")
), row.names = c(NA, -9L), class = "data.frame")
##################### #

test_addresses_9 <- address_from_table(test_address_table_9)

test_address_parts1 <- c("latitude", "LONGITUDE",
                         "address", "STREET", "City", "Statename", "zipcode")

test_addresses2 <- c("1200 Pennsylvania Ave, NW Washington DC",
                     "Research Triangle Park")

test_address_table <- data.frame(
  Acol = 1:2,
  STREET =  c("1200 Pennsylvania Ave", "5 pARK AVE"),
  City = c("Washington", "NY"),
  statename = c("DC", "NY"),
  zipcode = c("", ""),
  other_column = 9:10
)

test_address_table_withfull <- data.frame(Address = test_addresses2, test_address_table)

test_address_table_goodnames <- structure(list(
  Acol = 1:2,
  street = c("1200 Pennsylvania Ave", "5 pARK AVE"),
  city = c("Washington", "NY"), state = c("DC", "NY"),
  zip = c("", ""),
  other_column = 9:10
), class = "data.frame", row.names = c(NA, -2L))

##################### #


usethis::use_data(test_address_table_9,
                  test_address_parts1,
                  test_addresses_9,
                  test_addresses2,
                  test_address_table,
                  test_address_table_withfull,
                  test_address_table_goodnames,
                  overwrite = TRUE)

##################### #


writexl::write_xlsx(test_address_table_9, "./inst/testdata/address/test_address_table_9.xlsx")

writexl::write_xlsx(test_address_table, "./inst/testdata/address/test_address_table.xlsx")

writexl::write_xlsx(test_address_table_withfull, "./inst/testdata/address/test_address_table_withfull.xlsx")

writexl::write_xlsx(test_address_table_goodnames, "./inst/testdata/address/test_address_table_goodnames.xlsx")

##################### #
