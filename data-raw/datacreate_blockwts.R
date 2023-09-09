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

# to recreate them using code from the census2020download package:
# x <- census2020download  :: census2020_get_data()
# xlist <- census2020download  ::  census2020_save_datasets(x, save_as_data_for_package = FALSE)
# rm(x)
# names(xlist)


## To load once, after the new version were saved as data sets in local source package census2020download
# pkg = "census2020download"
# data(bgid2fips,blockid2fips, blockpoints, blockwts, quaddata, 
#      package = pkg)
# ***  but those took up > quota of space so then LFS disabled and need to delete those files in that package
### or else...
# load(file = "~/../../R/mysource/census2020download/data/blockwts.rda")
# load(file = "~/../../R/mysource/census2020download/data/bgid2fips.rda")
# load(file = "~/../../R/mysource/census2020download/data/blockid2fips.rda")
# load(file = "~/../../R/mysource/census2020download/data/blockpoints.rda")
# load(file = "~/../../R/mysource/census2020download/data/quaddata.rda")


#################################################################################### # 

# Temporarily 9/2023 create rounded off version of the new field that only takes 5.5 MB as dataset
# to use in getblocks... until replace blockwts data.table on dmap data commons 9/2023.
# and getblocks will check and use that small temporary file if necessary until finalize update.

block_radius_miles_round_temp <- round(blockwts$block_radius_miles, 2)
usethis::use_data(block_radius_miles_round_temp)

# *** stopped here - need to save in dmap data commons to replace older .rda version that lacks block_radius_miles info


##############################################################
# how to write to AWS with new names and new format? need permission for each new file?
#############

# replace the tables in AWS with the updated blockwts that has area or block_radius_miles

# # specify the AWS bucket, etc. 
baseurl = "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/"

################### # 

## Get bucket contents if you want to explore the bucket ----
mybucket <-  'dmap-data-commons-oa' # 
bucket_contents <- data.table::rbindlist(
  aws.s3::get_bucket(bucket = mybucket, prefix = "EJAM"), 
  fill = TRUE
)
bucket_contents
# bucket_contents
#                       Key             LastModified                               ETag      Size StorageClass               Bucket
# 1:     EJAM/bgid2fips.rda 2023-06-21T21:38:26.000Z "9a349f21026dbf26a344087b68f6a311"   1175611     STANDARD dmap-data-commons-oa
# 2:  EJAM/blockid2fips.rda 2023-06-21T21:38:33.000Z "dc3c9386a7bb979cba2b2620951c00fa"  39515482     STANDARD dmap-data-commons-oa
# 3:   EJAM/blockpoints.rda 2023-06-21T21:38:18.000Z "383b1dbc2df3b9219a99e7641d513c04" 124644326     STANDARD dmap-data-commons-oa
# 4:      EJAM/blockwts.rda 2023-06-21T21:37:37.000Z "3f51df469c764c4be37cfb12233f209e"  49905827     STANDARD dmap-data-commons-oa
# 5: EJAM/lookup_states.rda 2023-06-21T21:38:43.000Z "2963248201de9c7ff43b58596355ea34"      4405     STANDARD dmap-data-commons-oa
# 6:      EJAM/quaddata.rda 2023-06-21T21:38:51.000Z "398a6980b82d98859801bf195ca4f922" 175983809     STANDARD dmap-data-commons-oa
# > 
############################################################# #
## Use arrow to UPLOAD objects and save them in data commons as .arrow FILES IN A LOOP - not tested #### 

# datawrite_to_aws( "bgid2fips" ) # as .arrow files
datawrite_to_aws(justchecking = T, ext = '.arrow', varnames = 'blockwts')
datawrite_to_aws(justchecking = T, ext = '.rda',   varnames = 'blockwts')

##############################################################

# datawrite_to_aws() # not yet working
datawrite_to_aws(justchecking = T, ext = '.arrow', varnames = 'blockwts')
datawrite_to_aws(justchecking = T, ext = '.rda',   varnames = 'blockwts')

##############################################################

# and update documentation of that blockwts data in the package help file for now
#
# and document, rebuild /reinstall the package 
# 

## To load versions already stored in Data Commons
## but only after it is uploaded with new column, 
# block_radius_miles
# dataload_from_aws(varnames = "blockwts", ext=".rda") 
# dataload_from_aws()
 ##################

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
