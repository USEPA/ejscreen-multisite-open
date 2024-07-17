# datacreate_stateinfo.R makes stateinfo with a few columns 
# ?stateinfo
# 
# datacreate_stateinfo2.R makes stateinfo2 with more columns
# ?stateinfo2 

##################################################################################### # 

stateinfo <- structure(list(
  
  ST = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT",
         "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY",
         "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV",
         "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
         "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
         # "AS", "GU", "MP","VI"
         # "UM", #### U.S. Minor Outlying Islands
         # "US",
         "PR"),
  
  statename = c("Alabama", "Alaska", "Arizona", "Arkansas",
                "California", "Colorado", "Connecticut", "Delaware", "District of Columbia",
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                "Washington", "West Virginia", "Wisconsin", "Wyoming",
                # "American Samoa", "Guam", "Northern Mariana Islands", "U.S. Virgin Islands",
                # "U.S. Minor Outlying Islands",
                # "United States",
                "Puerto Rico"),
  
  ftpname = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
              "Colorado", "Connecticut", "Delaware", "DistrictOfColumbia",
              "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
              "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
              "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
              "Montana", "Nebraska", "Nevada", "NewHampshire", "NewJersey",
              "NewMexico", "NewYork", "NorthCarolina", "NorthDakota", "Ohio",
              "Oklahoma", "Oregon", "Pennsylvania", "RhodeIsland", "SouthCarolina",
              "SouthDakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
              "Washington", "WestVirginia", "Wisconsin", "Wyoming",
              # NA, NA, NA, NA,
              # NA,                 ####  U.S. Minor Outlying Islands
              # "UnitedStates",
              "PuertoRico"),
  
  FIPS.ST = c("01", "02", "04", "05", "06", "08", "09", "10",
              "11", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22",
              "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
              "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
              "46", "47", "48", "49", "50", "51", "53", "54", "55", "56",
              # "60", "66", "69", "78",
              # "74",       ####  U.S. Minor Outlying Islands
              # NA,         #### US
              "72"),
  REGION = c(4, 10, 9, 6, 9, 8, 1, 3, 3, 4, 4, 9, 10, 5, 5,
             7, 7, 4, 6, 1, 3, 1, 5, 5, 4, 7, 8, 7, 9, 1, 2, 6, 2, 4, 8, 5,
             6, 10, 3, 1, 4, 8, 4, 6, 8, 1, 3, 10, 3, 5, 8,
             # NA, NA, NA, NA,
             # NA,                ##### U.S. Minor Outlying Islands
             # NA,                # US
             2)
), row.names = c(NA, -52L), class = "data.frame")

###################################################################################### # 

stateinfo <-  metadata_add(stateinfo)
usethis::use_data(stateinfo, overwrite = TRUE)

###################################################################################### # 
