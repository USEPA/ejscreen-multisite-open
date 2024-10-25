#################################################################### #
# LIST OF FUNCTIONS TO TEST ####
#################################################################### #

################################## #     misc fips-related functions 

# fips_valid()
# fipstype()
# fips_lead_zero()  - tests are at the end of this file
# counties_as_sites() # creates table like getblocksnearby() does. and could use in mod_fips_picker-DRAFT.R
#   states_as_sites()   
# is.island()

################################## #    fips_ from_ 

###  if func is the OUTPUT, names are like   fips <- fips_from_x(x)

#          fips_from_table()
#     name2fips() and names2fips()  # inconsistent naming, but useful aliases
#          fips_from_name()  # same as name2fips()
#    fips_state_from_state_abbrev()
#    fips_state_from_statename()     # should it be statename or state_name
# fips_counties_from_statefips(   )  # should it be statefips or state_fips
# fips_counties_from_state_abbrev()
# fips_counties_from_statename(   )  # should it be statename or state_name
# fips_counties_from_countyname() 
# fips_counties_from_countynamefull()  internal helper
#       fips_bg_from_anyfips()
### and
###   see   getblocksnearby_from_fips() which uses  fips_bg_from_anyfips()

##NOT  cities_as_sites()  would be a name that makes sense but not used.
##NOT regions_as_sites()  would be a name that makes sense but not used.
##NOT   tracts_as_ and blockgroups_as_  maybe useful?

################################## #    fips2...

####  if fips is the INPUT, names are like   x <- fips2x(fips) 
#
# fips_st2eparegion()  # but not eparegion2statefips() ?? or
#    fips2state_fips(    )     #  fips2statefips would be a more consistent name?
#    fips2state_abbrev(  )
#    fips2statename(     ) # should it be statename or state_name
#    fips2countyname()
#    fips2name()    # inverse of name2fips()
############################################################################# #

#################################################################### #
## misc functions ####
#################################################################### #

# fips_valid()

test_that("fips_valid() works but is slow", {
  
  expect_no_error({
    testfipsx <- c(
      "3651000", # cdp
      "10", "10001", "02170000300", 
      "021700003003", "721537506011", 
      "010010201001000")
    x <- fips_valid(testfipsx)
  })
  expect_true(all(x))
  expect_identical(
    fipstype(testfipsx),
    c('city',
      "state", "county", "tract",
      "blockgroup", "blockgroup", 
      "block"
    )
  )
  
  expect_no_error({
    fips_valid(c('01',NA))
    fips_valid(NA)
    fips_valid("text")
  })
  expect_no_warning({
    fips_valid(c('01',NA))
    fips_valid(NA)
    fips_valid("text")
  })
  expect_false(fips_valid(NA))
  expect_identical(
    fips_valid(c(NA, "01")),
    c(FALSE, TRUE)
  )
  
  expect_identical(
    fips_valid(c(
      1, "1", "01",
      "00", 
      "10001", 
      "01001", 1001
    )),
    c(FALSE, FALSE, TRUE, 
      FALSE,
      TRUE,
      TRUE, FALSE
    )
  )
  
  expect_identical(
    fips_valid(
      fips_lead_zero(
        c(
          1, "1", "01", # lead zero fixed 1 and "1"
          "00", 
          "10001", 
          "01001", 1001 # lead zero fixed 1001
        ))),
    c(TRUE, TRUE, TRUE, 
      FALSE,
      TRUE,
      TRUE, TRUE
    )
  )
}
)
#################################################################### #

# fipstype()

testfips16 = c("1", "12", "123", "1234", "12345", "123456", "1234567", "12345678", "123456789",
               "1234567890", "12345678901", "123456789012", "1234567890123", "12345678901234",
               "123456789012345", "1234567890123456")
cbind(fipstype(testfips16), fips_lead_zero(testfips16), testfips16)
testfips16
#
# [1,] "state"      "01"              "1"
# [2,] "state"      "12"              "12"
# [3,] NA           NA                "123"
# [4,] "county"     "01234"           "1234"
# [5,] "county"     "12345"           "12345"
# [6,] "city"       "0123456"         "123456"          
# [7,] "city"       "1234567"         "1234567"     
# [8,] NA           NA                "12345678"
# [9,] NA           NA                "123456789"
# [10,] "tract"      "01234567890"     "1234567890"
# [11,] "tract"      "12345678901"     "12345678901"  # AMBIGUOUS CASE - MIGHT BE BLOCK GROUP MISSING THE LEADING 0 ***
# [12,] "blockgroup" "123456789012"    "123456789012"
# [13,] NA           NA                "1234567890123"
# [14,] "block"      "012345678901234" "12345678901234"
# [15,] "block"      "123456789012345" "123456789012345"
# [16,] NA           NA                "1234567890123456"

test_that("fipstype() works", {
  
  testfips16 = c("1", "12", "123", "1234", "12345", 
                 "123456", "1234567",  # CDP/CITY
                 "12345678", "123456789",
                 "1234567890", "12345678901", "123456789012", "1234567890123", "12345678901234",
                 "123456789012345", "1234567890123456")
  
  suppressWarnings({
    
    expect_no_error({
      testfips <- c("1", "12", "123", "1234", "12345", 
                    "123456",  "1234567", # CDP/CITY
                    "", NA, "words")
      fipstype(testfips)
    })
    expect_warning({
      fipstype(NA)
    })
    expect_warning({
      fipstype("WORD")
    })
    expect_identical(
      c("state","state",NA,"county","county",
        'city','city',
        NA,NA,NA),
      fipstype(testfips)
    )
    test_types = fips_counties_from_statename(c("Connecticut", "Delaware") )
    expect_true({
      all(
        fipstype(test_types) == "county"
      )
    })
    expect_identical(
      fipstype(testfips16),
      c("state", "state", NA, 
        "county", "county", 
        "city", "city", 
        NA, NA, 
        "tract", "tract", 
        "blockgroup", NA, 
        "block", "block", NA)
    )
    expect_true(!identical(fipstype(c(NA,NA)), c(NA,NA))) # c(NA,NA) is logical class
    expect_identical(fipstype( c(NA_character_,NA_character_)), c(NA_character_,NA_character_))
    expect_identical(fipstype("words"), NA_character_)
    expect_identical(fipstype(""), NA_character_)
    expect_identical(fipstype(99), "state")
    expect_no_warning(fipstype(99))
  })
})
#################################################################### #

# fips_lead_zero() tests are at the end of this file 

#################################################################### #
#    counties_as_sites()          # creates table like getblocksnearby()   and could get used by EJAM/R/mod_fips_picker-DRAFT.R

test_that("counties_as_sites works", {
  suppressWarnings({
    expect_no_error({
      counties_as_sites(
        fips_counties_from_state_abbrev("DE")
      )
    })
    
    # counties_as_sites(fips_counties_from_state_abbrev("DE"))
    x = counties_as_sites(
      fips_counties_from_state_abbrev("DE")
    )
    expect_true(is.data.table(x))
    expect_identical(
      names(x), c("ejam_uniq_id", "countyfips", "bgid", "blockid", "blockwt", 
                  "distance", "distance_unadjusted")
    )
    expect_true(
      all(x$countyfips %in% fips_counties_from_state_abbrev("DE"))
    )
    expect_true(
      length(unique(x$ejam_uniq_id)) == length(fips_counties_from_state_abbrev("DE"))
    )
    
    expect_warning(
      counties_as_sites(c(10001, NA))  # it does warn in this case
    )
    expect_warning(
      counties_as_sites(NA)  # it probably should warn in this case and does
    )
    # note it ignores duplicates:   counties_as_sites(c(10005, 10005))
  })
})
#################################################################### #
#      states_as_sites()   

test_that("states_as_sites works", {
  suppressWarnings({
    suppressMessages({
      
      expect_no_error({
        states_as_sites(
          fips_state_from_state_abbrev("DE")
        )
      })
      x = states_as_sites(
        fips_state_from_state_abbrev(c("RI", "DE"))
      )
      expect_true(is.data.table(x))
      expect_identical(
        names(x), c("ejam_uniq_id", "statefips", "bgid")
      )
      expect_true(
        all(x$statefips %in% fips_state_from_state_abbrev(c("RI", "DE")))
      )
      expect_true(
        length(unique(x$ejam_uniq_id)) == length(fips_state_from_state_abbrev(c("RI", "DE")))
      )
      
      expect_no_error(states_as_sites(c(10, NA)) )  # should not crash
      expect_warning(
        states_as_sites(c(10, NA))  # it does warn in this case
      )
      expect_warning(
        states_as_sites(NA)  # it probably should warn in this case !
      )
      # note it ignores duplicates:   states_as_sites(c(10, 10))
    })
  })
})

#################################################################### #

test_that("is.island() works", {
  suppressWarnings({
    suppressMessages({
      
      expect_true({
        all(is.island(statename = tolower(unique(stateinfo2$statename[stateinfo2$is.island.areas]))))
      })
      expect_true({
        all(is.island(ST = tolower(unique(stateinfo2$ST[stateinfo2$is.island.areas]))))
      })
      expect_true({
        all(is.island(fips = unique(stateinfo2$FIPS.ST[stateinfo2$is.island.areas])))
      })
      expect_true({
        !(is.island(NA))
      })
      expect_error({
        is.island(NULL)
      })
      expect_error({
        is.island(ST = "NY", statename = "New York")
      })
      # is.island(c("PR", "DE", "AS", NA))
      # is.island(statename = c("Guam", "New York", "american samoa", NA))
      # is.island(fips = c(21001, 60, "60", "600010000000"))
      # tail(cbind(stateinfo2[ , c("statename", "is.island.areas")], is.island(stateinfo2$ST)),10)
    })
    
  })
})
#################################################################### #
#################################################################### #
## fips_ from_ ####
#################################################################### #
#################################################################### #
# fips_from_table()

test_that("fips_from_table() works", {
  # fips_alias <- c('FIPS','fips','fips_code','fipscode','Fips','statefips','countyfips',
  #                 'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST', 'FIPS.COUNTY', 'FIPS.TRACT')
  suppressWarnings({
    mydat <- data.frame(
      FIPS = c("10001", 1, 10, NA, "text"),
      other = c("ok", "ok", "ok", "not ok, na", "not fips, text")
    )
    expect_no_error({
      x = fips_from_table(fips_table = mydat)
    })
    expect_true(
      all(c("10001", "01", "10", NA, NA) == x, na.rm = T)
    )
    
    mydat_y <- data.frame(
      countyfips = c("10001", 1, 10, NA, "text"),
      other = c("ok", "ok", "ok", "not ok, na", "not fips, text")
    )
    expect_warning({
      y = fips_from_table(mydat_y)
    })
    expect_identical(x, y)
    
    # uses acceptable names in order or preference picking best one
    # uses statefips as first choice here and ignores countyfips
    mydat_z <- data.frame(
      countyfips = "01001",
      statefips = "10"
    )
    expect_true(
      fips_from_table(mydat_z) == "10"
    )
    expect_warning(
      # no suitable colname found
      fips_from_table(data.frame(x = 1:3, y = 1:3))
    )
    
  })
})
#################################################################### #
#     name2fips()  # inconsistent name but useful . inverse of  fips2name()  
#          fips_from_name()  # same as name2fips()

test_that("name2fips() works", {
  
  # THESE SEEM SLOW... ***
  
  suppressWarnings({
    expect_no_error({
      x = name2fips(c("delaware", "NY"))
    })
    expect_true({
      all.equal(x, c("10", "36"))
    })
    expect_identical(
      name2fips("rhode island"), "44"
    )
    expect_true(
      "48201" == name2fips("Harris County, TX")
    )
    expect_true(
      "48201" == name2fips("harris county, tx")
    )
    expect_true(
      all(c("48201", "36") == name2fips(c("harris county, tx", "NY")))
    )
    expect_true(
      13 == name2fips("georgia")
    )
    expect_true(
      is.na(name2fips(NA))
    )
    expect_true(
      identical(name2fips(c("georgia", "Harris County, TX", NA)), c("13", "48201", NA))
    )
  })
})
#################################################################### #
#    fips_state_from_state_abbrev()

test_that("fips_state_from_state_abbrev() works", {
  suppressWarnings({
    suppressMessages({
      expect_true({
        all(fips2state_abbrev(
          fips_state_from_state_abbrev(c("DE", "RI")) 
        ) %in% c("DE", "RI"))
      })
      expect_true(
        unique(fips2state_abbrev( fips_state_from_state_abbrev("pr") )) == "PR"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_state_abbrev("vi") )) == "VI"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_state_abbrev("GU") )) == "GU"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_state_abbrev("MP") )) == "MP"
      )
      expect_true(
        fips_state_from_state_abbrev("UM") == 74 #    U.S. Minor Outlying Islands 
      )
      
      expect_warning(
        fips_state_from_state_abbrev(c("text", "other", "DE"))
      )  # probably should warn
      expect_true(
        is.na(fips_state_from_state_abbrev("text"))  # DOES return NA
      )
      expect_warning(fips_state_from_state_abbrev(13))  # probably should warn there is no such fips
      expect_warning(fips_state_from_state_abbrev(c(NA, "RI")))  # maybe should warn
    })
  })
})
#################################################################### #
#    fips_state_from_statename()    

test_that("fips_state_from_statename() works", {
  suppressWarnings({
    suppressMessages({
      expect_true({
        all(fips2state_abbrev(
          fips_state_from_statename(c("delaware", "Montana")) 
        ) %in% c("DE", "MT"))
      })
      testnames = stateinfo2$statename
      testnames = testnames[!(testnames %in% c("United States", "U.S. Minor Outlying Islands"))]
      testfips <- fips_state_from_statename(testnames)
      expect_true(
        all(fips_valid(testfips))
      )
      
      expect_true(
        unique(fips2state_abbrev( fips_state_from_statename("PUERto rico") )) == "PR"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_statename("U.S. Virgin Islands") )) == "VI"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_statename("GUAM") )) == "GU"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_statename("Northern Mariana Islands") )) == "MP"
      )
      expect_true(
        fips_state_from_statename("U.S. Minor Outlying Islands") == 74 #    U.S. Minor Outlying Islands 
      )
      
      expect_warning(
        fips_state_from_statename("text")
      )  # probably should warn NO SUCH STATE
      expect_true(
        is.na(fips_state_from_statename("text"))  # DOES return NA
      )
      expect_warning(fips_state_from_statename(3))  # probably should warn IT IS NOT TEXT
      expect_warning(fips_state_from_statename(c(NA, "Montana")))  # maybe should warn some are NA
    })
  })
})
#################################################################### #
# fips_counties_from_statefips()  # should it be statefips or state_fips

test_that("fips_counties_from_statefips() works", {
  suppressWarnings({
    myst = stateinfo2$FIPS.ST[!is.na(stateinfo2$FIPS.ST)]
    myst = myst[!(myst %in% c(60,66,69,74,78))] # remove US territories since don't have counties
    expect_no_error({
      myst2 = fips_counties_from_statefips(myst)
    })
    expect_identical(
      sort(unique(substr(myst2, 1, 2))),
      myst
    )
    expect_true(5 == unique(nchar(myst2)))
    expect_true(all(fips_valid(myst2)))
    expect_true(length(myst2) > 3200)
    
  })
})
#################################################################### #
# fips_counties_from_state_abbrev()

test_that("fips_counties_from_state_abbrev() works", {
  suppressWarnings({
    suppressMessages({
      expect_true({
        all(fips2state_abbrev(
          fips_counties_from_state_abbrev(c("DE", "RI")) 
        ) %in% c("DE", "RI"))
      })
      expect_true(
        unique(fips2state_abbrev( fips_counties_from_state_abbrev("pr") )) == "PR"
      )
      ## Commenting out since island areas are dropped from EJAM in v2.32
      # expect_true(
      #   unique(fips2state_abbrev( fips_counties_from_state_abbrev("vi") )) == "VI"
      # )
      # expect_true(
      #   unique(fips2state_abbrev( fips_counties_from_state_abbrev("GU") )) == "GU"
      # )
      # expect_true(
      #   unique(fips2state_abbrev( fips_counties_from_state_abbrev("MP") )) == "MP"
      # )    
      
      expect_true(
        is.na(fips_counties_from_state_abbrev("UM"))#  NO data for U.S. Minor Outlying Islands 
      )
      
      expect_warning(expect_warning(
        expect_warning(
        fips_counties_from_state_abbrev("text")
      )))
      expect_warning(expect_warning(
        expect_warning(fips_counties_from_state_abbrev(13))
      ))
      expect_warning(expect_warning(
        expect_warning(fips_counties_from_state_abbrev(c(NA, "RI")))
      ))
      
      expect_true(
        is.na(
          suppressWarnings(
          fips_counties_from_state_abbrev("text")
          )
        ) # PROBABLY SHOULD BUT DOES NOT RETURN NA, just empty
      )
    })
  })
})
#################################################################### #
# fips_counties_from_statename(   )  # should it be statename or state_name

test_that("fips_counties_from_statename() works", {
  suppressWarnings({
    suppressWarnings({
      suppressMessages({
        expect_true({
          all(fips2state_abbrev(
            fips_counties_from_statename(c("Montana", "District of Columbia")) 
          ) %in% c("MT", "DC"))
        })
        expect_true(
          unique(fips2state_abbrev( fips_counties_from_statename("PUERto rico") )) == "PR"
        )
        ## Commenting out since island areas are currently dropped from EJAM in v2.32
        # expect_true(
        #   unique(fips2state_abbrev( fips_counties_from_statename("U.S. Virgin Islands") )) == "VI"
        # )
        # expect_true(
        #   unique(fips2state_abbrev( fips_counties_from_statename("GUAM") )) == "GU"
        # )
        # expect_true(
        #   unique(fips2state_abbrev( fips_counties_from_statename("Northern Mariana Islands") )) == "MP"
        # )    
        
        expect_true(
          is.na(fips_counties_from_statename("U.S. Minor Outlying Islands"))  #  NO data for U.S. Minor Outlying Islands 
        )
        
        expect_warning(expect_warning(expect_warning(fips_counties_from_statename("text"))))
        expect_true(
          suppressWarnings(
           is.na(fips_counties_from_statename("text"))   # DOES NOT RETURN NA, just empty
          )
        )
        expect_warning(expect_warning(expect_warning(fips_counties_from_statename(13))))
        expect_warning(expect_warning(expect_warning(fips_counties_from_statename(c(NA, "Montana")))))
      })
    })
  })
})
#################################################################### #
# fips_counties_from_countyname()

test_that("fips_counties_from_countyname() works", {
  suppressWarnings({
    suppressMessages({
      expect_identical(
        fips_counties_from_countyname("Harris County", "TX"),
        "48201"
      )
      expect_identical(
        "Baltimore County, MD",
        fips2countyname(fips_counties_from_countyname("Baltimore County, MD"))
      )
      expect_true(
        is.na((fips_counties_from_countyname("Har", "TX", exact = T)))
      )
      expect_true(
        all('county' == fipstype(fips_counties_from_countyname("Har", "TX", exact = FALSE)))
      )
      expect_true({
        length(
          fips2countyname(fips_counties_from_countyname("Har", "TX", exact = FALSE))
        ) == 5 
        # fips_counties_from_countyname("Har",               "TX")    # finds 5 matches
        # fips_counties_from_countyname("Harris",            "TX")    # finds 2 matches
        # fips_counties_from_countyname("Harris ",           "TX")    # finds 1 match
        # fips_counties_from_countyname("Harris County, Texas", "TX") # finds 0 if state spelled out
      })
    })
  })
})

############################################################################# #
### fips_bg_from_anyfips() ####

############################################################################# #
# convert any FIPS codes to the FIPS of all the blockgroups that are
#   among or within or containing those FIPS
# @   details  This is a way to get a list of blockgroups, specified by state/county/tract or even block.
#
# Takes a vector of one or more FIPS that could be State (2-digit), County (5-digit),
#   Tract (11-digit), or blockgroup (12 digit), or even block (15-digit fips).
#
#   Returns unique vector of FIPS of all US blockgroups (including DC and Puerto Rico)
#   that contain any specified blocks, are equal to any specified blockgroup fips,
#   or are contained within any provided tract/county/state FIPS.
#
# @   param fips vector of US FIPS codes, as character or numeric,
#   with or without their leading zeroes, each with as many characters
# @   seealso [fips_lead_zero()]
# @   return vector of blockgroup FIPS (or NA values) that may be much longer than the
#   vector of fips passed to this function.
# @   examples
#   # all blockgroups in one state
#   blockgroupstats[,.N,by=substr(bgfips,1,2)]
#   length(fips_bg_from_anyfips("72"))
#   # all blockgroups in this one county
#   fips_bg_from_anyfips(30001)
#   # all blockgroups that contain any of these 6 blocks (just one bg)
#   fips_bg_from_anyfips( blockid2fips$blockfips[1:6])
#   # 2 counties
#   fips_bg_from_anyfips(c(36009,36011))
############################################################################# #
test_that('by state', {
  expect_no_warning({val <- fips_bg_from_anyfips(36)})
  expect_no_warning({val <- fips_bg_from_anyfips("36")})
  expect_equal(length(val), 16070)
})
################## #
test_that('by county', {
  expect_no_warning({val <- fips_bg_from_anyfips(36071)})
  expect_no_warning({val <- fips_bg_from_anyfips("36071")})
  expect_equal(length(val), 292)
  # check it's the same as the subset of state codes
  x <- fips_bg_from_anyfips("36")
  y <- x[which(startsWith(x, "36071"))]
  expect_equal(y, val)
})
################## #
test_that('by  tract', {
  expect_no_warning({val <- fips_bg_from_anyfips(36071000100)})
  expect_no_warning({val <- fips_bg_from_anyfips("36071000100")})
  expect_equal(length(val), 4)
  # check it's the same as the subset of state codes
  x <- fips_bg_from_anyfips("36")
  y <- x[which(startsWith(x, "36071000100"))]
  expect_equal(y, val)
})
################## #
test_that('by  block group', {
  expect_no_warning({val <- fips_bg_from_anyfips(360710001001)})
  expect_no_warning({val <- fips_bg_from_anyfips("360710001001")})
  expect_equal(length(val), 1)
  # check it's the same as the subset of state codes
  x <- fips_bg_from_anyfips("36")
  y <- x[which(startsWith(x, "360710001001"))]
  expect_equal(y, val)
})
################## #

# ACTUALLY     CANNOT use fips_bg_from_anyfips() with city fips SINCE CDP IS NOT BROKEN INTO BGS EXACTLY 

test_that('by CITY', {
  expect_warning({val <- fips_bg_from_anyfips(3651000)})
  expect_warning({val <- fips_bg_from_anyfips("3651000")})
  #expect_equal(length(val), 1)
  # check it's the same as the subset of state codes
  # x <- fips_bg_from_anyfips("36")
  # y <- x[which(startsWith(x, "3651000"))]
  # expect_equal(y, val)
})

################## #
# returns only UNIQUE bg fips once each, 
# even if 2 inputs contain or are inside same bg (turn into same bgid)
#  - do we want that to be the behavior? ***

test_that("returns only UNIQUE bgs in and/or containing the(se) fips", {
  expect_true({
    length(fips_bg_from_anyfips(c("36071010801"))) == 3 # contains 3 unique blockgroups
  })
  expect_true({
    length(fips_bg_from_anyfips(rep("36071010801", 5))) == 3 # will not return more matches than just unique
  })
  expect_true({
    length(fips_bg_from_anyfips(rep("360710108011", 5))) == 1 # will not return more matches than just unique
  })
  expect_true({
    length(fips_bg_from_anyfips(c(360710108011012, 360710108011006, 360710108011023))) == 1 # one unique bg returned even if it contains multiple blocks provided as query terms
  })
})
test_that('by  block - uniques only - is that behavior we want?', {
  expect_true( {
    length(fips_bg_from_anyfips(rep("360710108011", 5))) == 1
  })
  expect_no_warning({val <- fips_bg_from_anyfips(c(360710108011012, 360710108011006, 360710108011023))})
  expect_no_warning({val <- fips_bg_from_anyfips(c("360710108011012", "360710108011006", "360710108011023"))})
  expect_equal(length(val), 1)
})
################## #
test_that('test leading zero addition', {
  
  expect_no_warning({val <- fips_bg_from_anyfips("1055")}) # county
  expect_no_warning({val <- fips_bg_from_anyfips(1055)})
  expect_equal(length(val), 90)
  expect_equal(substr(val[1], 1,2) , "01")
  
  expect_no_warning({val <- fips_bg_from_anyfips("1")}) # state
  expect_no_warning({val <- fips_bg_from_anyfips(1)})
  expect_equal(length(val), 3925)
  expect_equal(substr(val[1], 1,2) , "01")
  
  expect_no_warning({val <- fips_bg_from_anyfips("1055011002")}) # tract
  expect_no_warning({val <- fips_bg_from_anyfips(1055011002)})
  expect_equal(length(val), 3)
  expect_equal(substr(val[1], 1,2) , "01")
  
  expect_no_warning({val <- fips_bg_from_anyfips(10690401001010)}) # not a bg
  expect_no_warning({val <- fips_bg_from_anyfips("10690401001010")})
  expect_equal(length(val), 1) #
  expect_equal(substr(val[1], 1,2) , "01")
})
################## #
test_that('returns bgs inside given tract(s)', {
  tractfips1 <- "10005051900"
  expect_true(fipstype(tractfips1) == "tract") # tract as input,
  expect_true(all(fips_bg_from_anyfips(tractfips1) %in% blockgroupstats$bgfips)) # returns actual bg fips
  expect_no_condition({val <- fips_bg_from_anyfips(tractfips1)}) # tract that contains 3 bgs
  expect_equal(length(val), 3)
  expect_true(all(substr(val, 1, 11) == tractfips1))
  rm(tractfips1)
})
################## #
# > fipstype("blue")
# [1] "county"
# > fipstype("sdfsdfsdfasdf0")
# [1] "block"
### THESE RETURNED NULL, not NA:
# fips_bg_from_anyfips("blue")
# fips_bg_from_anyfips("36-071")
# fips_bg_from_anyfips("36-07")
# fips_bg_from_anyfips("$1001")

#  NO ERROR for invalid strings, no string cleaning (dashes/dots not removed)
test_that('NO ERROR if inputs are invalid text strings', {
  suppressWarnings({
    expect_no_error({val <- fips_bg_from_anyfips("blue")})
    expect_no_error({val <- fips_bg_from_anyfips("36-071")})
    expect_no_error({val <- fips_bg_from_anyfips("36-07")})
    expect_no_error({val <- fips_bg_from_anyfips("$1001")})
  })
})

#  warnings for invalid strings, no string cleaning (dashes/dots not removed)
test_that('WARN if inputs are invalid text strings', {
  suppressWarnings({
    expect_warning({val <- fips_bg_from_anyfips("blue")})
    expect_warning({val <- fips_bg_from_anyfips("36-071")})
    expect_warning({val <- fips_bg_from_anyfips("36-07")})
    expect_warning({val <- fips_bg_from_anyfips("$1001")})
  })
})
################## #


#################################################################### #
## fips2...  ####
#################################################################### #

# fips_st2eparegion()  # fips_st2eparegion(fips_state_from_state_abbrev(stateinfo$ST))

test_that("fips_st2eparegion() works", {
  suppressWarnings({
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("ME")) == 1
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("NY")) == 2
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("VA")) == 3
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("GA")) == 4
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("IL")) == 5
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("LA")) == 6
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("KS")) == 7
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("MT")) == 8
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("ca")) == 9
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("wa")) == 10
    )
    expect_true({
      all(fips_st2eparegion(stateinfo$FIPS.ST) %in% 1:10)
    })
  })
})
#################################################################### #
#    fips2state_abbrev(  )

test_that("fips2state_abbrev() works", {
  suppressWarnings({
    expect_identical(
      fips2state_abbrev(stateinfo$FIPS.ST), 
      stateinfo$ST
    )
  })
})
#################################################################### #
#    fips2state_fips(    )  

test_that("fips2state_fips() works", {
  suppressWarnings({
    TESTFIPS = c(2, "02170", 10001, "01003011103", "010030111031", "010030111031054")
    expect_identical(
      fips2state_fips(TESTFIPS), 
      c("02", "02", "10","01" ,"01", "01")
    )
  })
})
#################################################################### #
#    fips2statename(     ) 

test_that("fips2statename() works", {
  suppressWarnings({
    TESTFIPS = c(2, "02170", 10001, "01003011103", "010030111031", "010030111031054")
    expect_identical(
      fips2statename(TESTFIPS), 
      fips2statename( c("02", "02", "10","01" ,"01", "01") )
    )
    expect_warning(fips2statename(NA))
    expect_true(
      is.na(fips2statename(NA))  # returns United States instead of NA due to stateinfo2 table
    )
  })
})
#################################################################### #
#    fips2countyname()

test_that("fips2countyname() works", {
  suppressWarnings({
    expect_true({
      
      # REMOVE ISLAND AREAS SINCE THEIR FIPS ARE NOT QUITE LIKE COUNTY FIPS - FIRST 5 LETTERS ARE NOT UNIQUE "COUNTY"
      
      bg = blockgroupstats[!is.island(blockgroupstats$ST), c("countyname","bgfips", "ST")]
      first_bg_per_county = match(unique(bg$countyname), bg$countyname)
      
      bgfips_examples = bg$bgfips[first_bg_per_county]
      countyfips_eg = substr(bgfips_examples,1,5)
      their_countyname = paste0(
        bg$countyname[first_bg_per_county], 
        ", ", 
        bg$ST[first_bg_per_county])
      all.equal(
        fips2countyname(fips = countyfips_eg), 
        their_countyname
      )
      
    })
    expect_warning(
      fips2countyname(NA)
    )
    expect_no_error(
      fips2countyname(NA)
    ) 
    expect_true(
      is.na(fips2countyname(10))
    )
    
  })
})
#################################################################### #
#    fips2name()    # inverse of name2fips()

test_that("fips2name() works", {
  suppressWarnings({
    
    # for states
    expect_true(
      all(fips2name(fips = fips_state_from_state_abbrev(stateinfo$ST)) == stateinfo$statename)
    )
    
    # for counties
    expect_true(
      identical(
        fips2name("10001"),
        "Kent County, DE"
      )
    )
    allcountyfips = fips_counties_from_statefips(stateinfo$FIPS.ST)
    expect_true(
      identical(
        fips2name(allcountyfips),
        fips2countyname(allcountyfips)
      )
    )
    expect_true({
      identical(
        fips2name(fips = fips_counties_from_statename("Delaware")),
        c("Kent County, DE", "New Castle County, DE", "Sussex County, DE")
      )
    })
    
    # 
  })
})
#################################################################### #


################################################################### # 
## fips_lead_zero() ####
################################################################### # 
# Adds a leading zero to fips code if leading zero seems to be missing, 
# and replaces with NA if length not plausible. 
# @  param fips vector of numeric or character US FIPS codes
# @  return vector of same length

# Note it does NOT VALIDATE FIPS beyond 
# checking length seems OK as-is or with an added leading 0.
#  ***  Should this function warn/ return NA if fips is invalid? i.e., 
#  if state code becomes 00 or state code is not in list of valid ones, or
#  next 3 digits are 000 or 1st 5 digits as adjusted are not in list of valid county fips, or
#  resulting longer fips is not in lists of valid tracts, blockgroups, blocks?
#  Only checks if it is a length that 
#  might be a state, county, tract, blockgroup, or block FIPS code
#  already, or might be with one added leading zero.

# testfips16 = c("1", "12", "123", "1234", "12345", "123456", "1234567", "12345678", "123456789",
#                "1234567890", "12345678901", "123456789012", "1234567890123", "12345678901234",
#                "123456789012345", "1234567890123456")
# cbind(fipstype(testfips16), fips_lead_zero(testfips16), testfips16)
# testfips16        
#
# [1,] "state"      "01"              "1"
# [2,] "state"      "12"              "12"
# [3,] NA           NA                "123"
# [4,] "county"     "01234"           "1234"
# [5,] "county"     "12345"           "12345"
# [6,] NA           NA                "123456"
# [7,] NA           NA                "1234567"
# [8,] NA           NA                "12345678"
# [9,] NA           NA                "123456789"
# [10,] "tract"      "01234567890"     "1234567890"
# [11,] "tract"      "12345678901"     "12345678901"  # AMBIGUOUS CASE - MIGHT BE BLOCK GROUP MISSING THE LEADING 0 ***
# [12,] "blockgroup" "123456789012"    "123456789012"
# [13,] NA           NA                "1234567890123"
# [14,] "block"      "012345678901234" "12345678901234"
# [15,] "block"      "123456789012345" "123456789012345"
# [16,] NA           NA                "1234567890123456"


test_that("fips_lead_zero correct for 1 through 16 digits long", {
  testfips16 = c(
    "1", "12",                      # 1 or 2 digits becomes 2 digit state fips
    "123",  
    "1234", "12345",               # 4 or 5 digits becomes 5 digit county fips
    "123456", "1234567", "12345678", "123456789",
    "1234567890", "12345678901", # 10 or 11 digits becomes 11 digit tract fips
    "123456789012",     # 12 digits is already like a 12 digit blockgroup fips
    "1234567890123",
    "12345678901234","123456789012345", # 14 or 15 becomes 15 digit block fips
    "1234567890123456")
  suppressWarnings({
    expect_identical(
      fips_lead_zero(testfips16),
      c("01", "12",                       # state fips 2 digits
        NA, 
        "01234", "12345",                # county fips 5 digits
        "0123456"  ,       "1234567",    # city/cdp fips  7 digits
        NA, NA, 
        "01234567890", "12345678901",    # tract fips 11 digits
        "123456789012",             # blockgroup fips 12 digits
        NA, 
        "012345678901234", "123456789012345", # block 15 digits
        NA)
    )  
  })
})

#################### # #################### #
test_that("negative, decimal, space, any non-digit means NA is returned", {
  junk = c(1, -1, 1.1, "-1", "1.1", "  1", "text", NA)
  suppressWarnings({
    expect_identical(
      fips_lead_zero(junk),
      c("01", NA,NA,NA,NA,NA,NA,NA)
    )
  })
})
#################### # #################### #
# test with 1 digit
#  meant to only add one zero at most, to create state-codes
# should it warn if the fips <1 or >78 (largest possible 2digit number code) ?

test_that('1 digit', {
  expect_no_warning({val <- fips_lead_zero("1")})
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero(1)})
  expect_equal(val, "01")
  
  expect_no_warning({val <- fips_lead_zero("0")})
  expect_equal(val, "00")
  expect_no_warning({val <- fips_lead_zero(0)})
  expect_equal(val, "00")
})
#################### # #################### #
# test with 2 digits
# it doesn't add any zeros since it infers this to be a state-code
test_that('2 digit', {
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("01")}) # leading zero
  })
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero(10)}) # numeric
  expect_equal(val, "10")
  expect_no_warning({val <- fips_lead_zero(01)}) #leading zero in numeric
  expect_equal(val, "01")
  suppressWarnings({
    expect_no_warning({fips_lead_zero("00")})  # DOES NOT FULLY VALIDATE SO DOES NOT KNOW 00 IS NOT ANY STATE'S FIPS
    expect_no_error({val <- fips_lead_zero("00")}) # zero string
  })
  expect_equal(val, "00")
})
#################### # #################### #
# test with 3 digits
#  A 3 digit fips is invalid since 1st two are state fips, which cannot be 00 - should it warn/ give NA? ***
test_that('3 digit', {
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("001")}) # leading zero
    expect_warning({val <- fips_lead_zero("001")}) # leading zero
  })
  #expect_equal(val, "011")
  expect_true(is.na(val))
  suppressWarnings({
    expect_warning({val <- fips_lead_zero(100)}) # numeric
    expect_no_error({val <- fips_lead_zero(100)}) # numeric
  })
  #expect_equal(val, "100")
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(001)}) #leading zero in numeric
  expect_equal(val, "01")
  suppressWarnings({
    expect_warning({val <- fips_lead_zero("000")}) # zero string
    expect_no_error({val <- fips_lead_zero("000")}) # zero string
  })
  expect_true(is.na(val))
})
#################### # #################### #
# test with 4 digits
# adds one leading zero if needed to create 5-digit county-code
#  but note fips is invalid if first of 4 digits is zero (no state fips is 00) - should it warn/ give NA? ***
test_that('4 digit', {
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("0001")}) # leading zero
    expect_no_warning({val <- fips_lead_zero("0001")}) # leading zero # zero string ### LENGTH SEEMS OK AND IT DOES NOT KNOW 00001 IS NO COUNTY'S FIPS
  })
  expect_equal(val, "00001")
  #expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000)}) # numeric
  expect_equal(val, "01000")
  #expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(0001)}) #leading zero in numeric
  expect_equal(val, "01")
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("0000")}) # zero string
    expect_no_warning({val <- fips_lead_zero("0000")}) # zero string ### LENGTH SEEMS OK AND IT DOES NOT KNOW 00000 IS NO COUNTY'S FIPS
  })
  expect_equal(val, "00000")
})
#################### # #################### #
# test with 5 digits
# adds no leading zero since it is inferred to be 5 digit county-code
test_that('5 digit', {
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("00001")}) # leading zero - invalid actual code, but format is valid - does not warn since not really validating fully
    expect_no_warning({val <- fips_lead_zero("00001")}) # leading zero- same
  })
  expect_equal(val, "00001")
  #expect_true(is.na(val))
  suppressWarnings({
    expect_no_warning({val <- fips_lead_zero(10000)}) # numeric
    expect_no_error({val <- fips_lead_zero(10000)}) # numeric
  })
  expect_equal(val, "10000")
  #expect_true(is.na(val))
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero(00001)}) #leading zero in numeric - invalid actual code, but format is valid - does not warn since not really validating fully
    expect_no_warning({val <- fips_lead_zero(00001)}) #leading zero in numeric - same
  })
  expect_equal(val, "01")
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("00000")}) # zero string - invalid actual code, but format (length) is valid
    expect_no_warning({val <- fips_lead_zero("00000")}) # same
  })  
  expect_equal(val, "00000")
})
#################### # #################### #
# test with 6 digits
#    6 digits is city or other cdp - 
# 6 digit numeric has problems... unless change options scipen...
#   100,000 gets converted to 1e+05 which is 5 digits and returned
# raise threshold with options(scipen = 999) and the test passes
# should add options(scipen = 0) to return back to default
test_that('6 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  suppressWarnings({
    testthat::expect_no_warning({val <- fips_lead_zero("000001")}) #  
    expect_true(!is.na(val))
  })
  # 
  expect_no_warning({val <- fips_lead_zero(100000)}) # numeric -------------------    1e+05  unless adjust options in which case it is NA and test passes
  expect_true(!is.na(val))
  suppressWarnings({
    expect_no_warning({val <- fips_lead_zero(100001)}) # numeric  
    expect_true(!is.na(val))
  })
  suppressWarnings({
    expect_no_warning({val <- fips_lead_zero("000000")}) # zero string
    expect_true(!is.na(val))
  })
  options(scipen = 0)
})
#################### # #################### #
# test with 7 digits
# length is  city or other cdp
test_that('7 digit', {  
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  
  expect_no_warning({val <- fips_lead_zero("0000001")}) # leading zero
  expect_true(!is.na(val))
  expect_no_warning({val <- fips_lead_zero(3651000)}) # numeric
  expect_true(!is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000001)}) # numeric
  expect_true(!is.na(val))
  expect_no_warning({val <- fips_lead_zero("0000000")}) # zero string
  expect_true(!is.na(val))
  
  options(scipen = 0)
})
#################### # #################### #
# test with 8 digits
# 8 characters is ALWAYS INVALID - cannot be a county (that is 5) nor can it be a tract (that is 11 including any leading zero)
test_that('8 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  
  expect_warning({val <- fips_lead_zero("00000001")}) # leading zero
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(10000000)}) # numeric
  # expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(10000001)}) # numeric
  expect_true(is.na(val))
  
  expect_no_warning({val <- fips_lead_zero(00000001)}) #leading zero in numeric - drops the zeroes and then sees it as a 1 which is ok
  expect_equal(val, "01")
  expect_warning({val <- fips_lead_zero("00000000")}) # zero string
  expect_true(is.na(val))
  
  options(scipen = 0)
})
#################### # #################### #
# test with 9 digits
# ALWAYS INVALID -- cannot be a county (that is 5) nor can it be a tract (that is 11 including any leading zero)
test_that('9 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  
  expect_warning({val <- fips_lead_zero("000000001")}) # leading zero
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(100000000)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(100000001)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero("000000000")}) # zero string
  expect_true(is.na(val))
  
  options(scipen = 0)
})
#################### # #################### #
# test with 10 digits
# adds a leading zero if needed to create 11 digit census-tract-code
test_that('10 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  
  expect_no_warning({val <- fips_lead_zero("0000000001")}) # leading zero
  expect_equal(val, "00000000001")
  expect_no_warning({val <- fips_lead_zero(1000000000)}) # numeric
  expect_equal(val, "01000000000")
  expect_no_warning({val <- fips_lead_zero(1000000001)}) # numeric
  expect_equal(val, "01000000001")
  expect_no_warning({val <- fips_lead_zero("0000000000")}) # zero string
  expect_equal(val, "00000000000")
  
  options(scipen = 0)
})
#################### # #################### #
# test with 11 digits
# AMBIGUOUS CASE - MIGHT BE A BLOCKGROUP MISSING A LEADING ZERO, BUT FUNC ASSUMES IT IS A TRACT FIPS.
# adds no leading zero since it is inferred as a 11 digit census-tract-code
#  - WHICH MIGHT BE WRONG IF USER TRIED TO PROVIDE BG FIPS LACKING its LEADING ZERO
test_that('11 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  
  expect_no_warning({val <- fips_lead_zero("00000000001")}) # leading zero
  expect_equal(val, "00000000001")
  expect_no_warning({val <- fips_lead_zero(10000000000)}) # numeric
  expect_equal(val, "10000000000")
  expect_no_warning({val <- fips_lead_zero(10000000001)}) # numeric
  expect_equal(val, "10000000001")
  expect_no_warning({val <- fips_lead_zero("00000000000")}) # zero string
  expect_equal(val, "00000000000")
  
  options(scipen = 0)
})
#################### # #################### #
# test with 12 digits
# adds no leading zero since it is inferred as a 12 digit block-group-code
test_that('12 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  
  expect_no_warning({val <- fips_lead_zero("000000000001")}) # leading zero
  expect_equal(val, "000000000001")
  expect_no_warning({val <- fips_lead_zero(100000000000)}) # numeric
  expect_equal(val, "100000000000")
  expect_no_warning({val <- fips_lead_zero(100000000001)}) # numeric
  expect_equal(val, "100000000001")
  expect_no_warning({val <- fips_lead_zero("000000000000")}) # zero string
  expect_equal(val, "000000000000")
  
  options(scipen = 0)
})
#################### # #################### #
# test with 13 digits
# ALWAYS INVALID LENGTH, SINCE BGFIPS IS 12 AND BLOCK IS 15
test_that('13 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  
  expect_warning({val <- fips_lead_zero("0000000000001")}) # leading zero
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(1000000000000)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(1000000000001)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero("0000000000000")}) # zero string
  expect_true(is.na(val))
  
  options(scipen = 0)
})
#################### # #################### #
# test with 14 digits
# add leading zero  to create 15-digit block-code
test_that('14 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  
  expect_no_warning({val <- fips_lead_zero("00000000000001")}) # leading zero
  expect_equal(val, "000000000000001")
  expect_no_warning({val <- fips_lead_zero(10000000000000)}) # numeric
  expect_equal(val, "010000000000000")
  expect_no_warning({val <- fips_lead_zero(10000000000001)}) # numeric
  expect_equal(val, "010000000000001")
  expect_no_warning({val <- fips_lead_zero("00000000000000")}) # zero string
  expect_equal(val, "000000000000000")
  
  options(scipen = 0)
})
#################### # #################### # 
# test with 15 digits
# add no leading zero since it is inferred to be a 15-digit block-code
test_that('15 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  
  expect_no_warning({val <- fips_lead_zero("000000000000001")}) # leading zero
  expect_equal(val, "000000000000001")
  expect_no_warning({val <- fips_lead_zero(100000000000000)}) # numeric
  expect_equal(val, "100000000000000")
  expect_no_warning({val <- fips_lead_zero(100000000000001)}) # numeric
  expect_equal(val, "100000000000001")
  expect_no_warning({val <- fips_lead_zero("000000000000000")}) # zero string
  expect_equal(val, "000000000000000")
  
  options(scipen = 0)
})
#################### # #################### # 
# test with 16 digits
# ALWAYS INVALID LENGTH - 16 digits cannot be valid fips since 15 is block fips and longest we can interpret
test_that('16 digit +', {  
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  
  expect_warning({val <- fips_lead_zero("0000000000000001")}) # leading zero
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(1000000000000000)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(1000000000000001)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero("0000000000000000")}) # zero string
  expect_true(is.na(val))
  
  options(scipen = 0)
})

options(scipen = 0)
#################### # #################### # 
# test with invalid text input
test_that('warn on invalid text- cant coerce to numeric FIPS', {
  suppressWarnings({
    expect_warning({
      val <- fips_lead_zero("blue")
      ## revised that function so it now does warn in this case, but does return something like "0blue" still.
    }) 
  })
})

################################################################### # 
################################################################### # 

