# Script to modify the dataset blockwts using area of block, to store the block_radius_miles
#

# 1) During EJAM creation/ updates, try to recreate the block-related datasets for EJAM that are stored in AWS
#
# blockwts was originally created using a non-CRAN package called 
# census2020download, functions were 
# listoftables = census2020_get_data()  # does download/unzip/clean
# census2020_save_datasets()

# used 
# block area and
# added precalculated block_radius_miles 
# to the blockwts data.table 
# something like this:
# to create or get created copies of these datasets:
#
# to recreate them using code from the census2020download package:
# x <- census2020download  :: census2020_get_data()
# xlist <- census2020download  ::  census2020_save_datasets(x, save_as_data_for_package = FALSE)
# rm(x)
# names(xlist)
#
# To load them since they were saved as data sets in local source package census2020download
 # ... get them into memory here via one of these 2 ways:
pkg = "census2020download"
data(bgid2fips,blockid2fips, blockpoints, blockwts, quaddata, 
     package = pkg)

### or else...

  # load(file = "~/../../R/mysource/census2020download/data/blockwts.rda")

# load(file = "~/../../R/mysource/census2020download/data/bgid2fips.rda")
# load(file = "~/../../R/mysource/census2020download/data/blockid2fips.rda")
# load(file = "~/../../R/mysource/census2020download/data/blockpoints.rda")
# load(file = "~/../../R/mysource/census2020download/data/quaddata.rda")
#################################################################################### # 

# create rounded off temporary version that only takes 5.5 MB as dataset
# to use in getblocks... until replace blockwts data.table on dmap data commons 9/2023
# and getblocks will check and use that small temporary file if necessary until finalize update.
block_radius_miles_round_temp <- round(blockwts$block_radius_miles, 2)
  usethis::use_data(block_radius_miles_round_temp)

# stopped here - need to save in dmap data commons to replace older .rda version that lacks block_radius_miles info


##############################################################
# how to write to AWS with new names and new format? need permission for each new file.
#############

# replace the tables in AWS with the updated blockwts that has area or block_radius_miles

#   varnames <- c('bgid2fips',   'blockid2fips', 'blockpoints', 'blockwts' , 'quaddata' )
#   fnames <- paste0(varnames, ".rda")
#   mybucketfolder <- "EJAM"
#   pathnames <- paste0(mybucketfolder, '/', fnames)    # "EJAM/blockwts.rda", # pathnames[i],
# # specify the AWS bucket, etc. 
# baseurl = "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/"
#  mybucket <- 'dmap-data-commons-oa' 

# i = 4
 # not sure how to use vector of names here instead of typing blockwts, etc.:

 # aws.s3::s3save(blockwts, object = "EJAM/blockwts.rda",  bucket = mybucket, opts = list(show_progress = TRUE)) # almost worked
 


### also wanted to try arrow format next: 
#
# aws.s3::s3write_using(bgid2fips, FUN = arrow::write_ipc_file, object = "EJAM/bgid2fips.arrow", bucket = mybucket )
# aws.s3::s3read_using(FUN = arrow::read_ipc_file)
# aws.s3::s3read_using(FUN = load, object = "EJAM/bgid2fips.rda", bucket = mybucket)
# 
# for (i in 1:length(varnames)) {
#   arrow::write_ipc_file(
#     x = get(varnames[i]), 
#     sink = paste0(baseurl, paste0(varnames[i], ".arrow")))
# }
##############################################################

 
# and update documentation of that blockwts data in the package help file for now
#
# and document, rebuild /reinstall the package 
# 
# You can use the EPA Open Data Metadata editor: https://edg.epa.gov/epa-open-data-metadata-editor/
#   If you need any assistance please reach out to edg@epa.gov. 


#  
# Updated code so that when getblocksnearby() code is run, 
# it does a join to get block_radius_miles value for each relevant block,
# 
# in getblocksnearby() so you can directly use those distances as adjusted already,  
# not in doaggregate() which is where you would almost always use the results of getblocksnearby()? 
#  ***  as you join blockwts to sites2blocks, grab block_radius_miles as well, not just wts -
# -- will now try to do this join in getblocksnearby() not in doaggregate() !
#
# Also updated same code so it will use that to modify the distance of site to block.
#
############ #
# Excerpt from 2017 tech doc on using min dist to create proximity scores
#  but the same idea applies if EJAM is
#  calculating and reporting distance from each site to avg resident in each block and then bg) ---
#  ########### #
#   Since we cannot easily find out how the
#   residents are actually distributed in those areas, we made two simplifying assumptions:
#   - residents are evenly distributed across the surface area of each block, and
#  - each block can be represented by a circle whose radius is [Block area / Pi]^(1/2) .
#  We call this latter value the Block Area Equivalent Radius.
#  Our investigations indicate that for any dij less than the Block Area Equivalent Radius, 0.9 times that
#  value is a reasonable representation of the average distance from the facility for all residents in the
#  block. We call this the dij corrected.
#  Our computational scheme determines the dij values as described above, tests for the comparison with
#  Block Area Equivalent Radius, and substitutes dij corrected values. We found that we needed to make
#  that correction for less than 1% of all facility/block combinations in an early testing dataset that used
#  2005-2009 ACS data.
#  ########### #

# and update also sites2blocks_example
#
# and document, rebuild /reinstall the package 
