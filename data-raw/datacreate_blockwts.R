#################################################################################### # 

# load census2020download pkg to get block data  ####

# The block data.table tables
#  'blockpoints', 'blockwts', 'quaddata', 'blockid2fips'
# were created from Census 2020 data
# using a non-CRAN package (in github) called census2020download
# and they only need to be updated if FIPS codes or boundaries of blockgroups or blocks change.
######################################### # 

warning("not finished updating 'datacreate_blockwts.R' script to include Island Areas GU VI MP AS")

if (!exists("askquestions")) {askquestions <- FALSE}

# do_download = TRUE

if (interactive() && askquestions) {
  do_download <- askYesNo("Download and update all block tables like blockpoints, blockwts, etc.? 
  (Must update if FIPS codes or boundaries of blockgroups or blocks changed)")
  if (is.na(do_download)) {do_download <- FALSE}
} else {
  do_download <- TRUE
}

# do_data_save = TRUE

if (interactive() && askquestions) {
  do_data_save <- askYesNo("Do metadata_add() and use_data() ?")
  if (is.na(do_data_save)) {do_data_save <- FALSE}
} else {
  do_data_save <- TRUE
}

######################################### # 

# tryinstall = FALSE

if (!do_download) {
  
  message("block data not downloaded")
} else {
  
  # check if right pkg is installed so cando this
  
  if (require(census2020download)) {
    cando <- TRUE
    cat('attached census2020download package\n')
  } else {
    ## tryinstall ####
    
    if (interactive() && askquestions) {
      tryinstall = askYesNo("Requires the census2020download package, not found installed yet. Try to install now from github?")
      if (is.na(tryinstall)) {tryinstall <- FALSE}
    } else {
      warning('Cannot update block data -- That requires the census2020download package from\n devtools::install_github("ejanalysis/census2020download") ')
      tryinstall <- FALSE
      cando <- FALSE
    }
    if (tryinstall) {
      devtools::install_github("ejanalysis/census2020download")
      if (!require(census2020download)) {
        cando <- FALSE
        warning('failed to install necessary package from  devtools::install_github("ejanalysis/census2020download")')
      } else {
        cat('installed and attached census2020download package\n')
        cando <- TRUE
      }
    } else {
      warning('Not updating block data -- that requires the census2020download package from\n devtools::install_github("ejanalysis/census2020download") ')
      cando <- FALSE
    }
  }
  ################################################ #
  
  if (cando)  {
    # census2020_get_data() ####
    
    cat('Downloading block data and preparing block tables for EJAM...')
    blocks <- census2020_get_data()
    cat("done downloading block tables:\n")
    print(cbind(names(blocks)))
    message("block data downloaded")
    got_download <- TRUE
  } else {
    message("block data not downloaded")
    got_download <- FALSE
  }
  ################################################ #
  
  if (got_download && do_data_save) {
    
    # census2020_save_datasets() ####
    
    cat('Saving block tables to EJAM package, with latest metadata...')
    mylistoftables <- census2020_save_datasets(
      blocks, 
      save_as_data_for_package = TRUE,  #  does metadata_add()  and  use_data()
      overwrite = TRUE
    )
    cat("done saving block tables as datasets in package.")
    # want to keep the objects in memory? OK - may use to run EJAM functions like recreating testoutput data
    rm(mylistoftables)
    gc()
  }
}
#################################################################################### # 












# browseURL("https://ejanalysis.github.io/census2020download/reference/blockpoints.html")
# browseURL("https://ejanalysis.github.io/census2020download/reference/blockwts.html")
# browseURL("https://ejanalysis.github.io/census2020download/reference/index.html")

# devtools::install_github("ejanalysis/census2020download")
# require # (census2020download)

# ?census2020download   # the name of the package  
# ?census2020_download  # the name of the function
# Sys.time()
# blocks <- census2020_get_data()  # takes minutes -- has to download each state, etc.
# Sys.time()
# xlist  <- census2020_save_datasets(blocks, save_as_data_for_package = FALSE)
#
# rm(blocks)
# names(xlist)

# Those block data.table objecs were then saved via pins board 

#################################################################################### # 

# The dataset used by EJAM called blockwts has a column called 
# block_radius_miles that is what the radius would be if the block were circular, 
# and it was created based on 
# 
# block_radius_miles = sqrt(area / pi) 
#
## because   area = pi * block_radius_miles^2 
#
# where area is in square miles and is the sum of land and water area from Census data.

#################################################### #

# Excerpt from 2017 tech doc on using min dist to create proximity scores
#  but the same idea applies if EJAM is
#  calculating and reporting distance from each site to avg resident in each block and then bg) ---
#  ########### #
#   Since we cannot easily find out how the
#   residents are actually distributed in those areas, we made two simplifying assumptions:
#   - residents are evenly distributed across the surface area of each block, and
#  - each block can be represented by a circle whose radius is 

#  block_radius_miles  =  sqrt(area / pi) 
#
#  We call this latter value the "Block Area Equivalent Radius."
#
#  area = pi * block_radius_miles^2 

#  Our investigations indicate that for any dij less than the Block Area Equivalent Radius, 0.9 times that
#  value is a reasonable representation of the average distance from the facility for all residents in the
#  block. We call this the dij corrected.
#  Our computational scheme determines the dij values as described above, tests for the comparison with
#  Block Area Equivalent Radius, and substitutes dij corrected values. We found that we needed to make
#  that correction for less than 1% of all facility/block combinations in an early testing dataset that used
#  2005-2009 ACS data.
#  ########### #

#    HOWEVER, a significant % of blocks have effective radius of at least 1 mile radius,
#    which is an area of at least pi*R^2 = 3.14 square miles area.

# See separate notes/ analysis in archived   dev\block counts near FRS sites
# and some of that in the 4_advanced.Rmd

#################################################################################### # 
