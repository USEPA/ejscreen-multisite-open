
#################################################################################### # 
# blockwts was originally created using a non-CRAN package called 
# census2020download, functions were something like this:
# listoftables = census2020_get_data()  # does download/unzip/clean
# census2020_save_datasets()
#
# used 
# block area and
# added precalculated block_radius_miles 
# to the blockwts data.table 
#
# to recreate them using code from the census2020download package:
# x <- census2020download  :: census2020_get_data()
# xlist <- census2020download  ::  census2020_save_datasets(x, save_as_data_for_package = FALSE)
# rm(x)
# names(xlist)
#
## To load once, after the new version were saved as data sets in local source package census2020download
# pkg = "census2020download"
# data(bgid2fips,blockid2fips, blockpoints, blockwts, quaddata, 
#      package = pkg)
# ***  but those took up > quota of space so then LFS disabled and need to delete those files in that package
### or else...
# load(file = "~/../../R/mysource/census2020download/data/blockwts.rda") # etc.
#################################################################################### # 


##############################################################



# In 2023, we Updated the code, so that when getblocksnearby() code is run, 
# it does a join to get block_radius_miles value for each relevant block,
# 
# in getblocksnearby() so you can directly use those distances as adjusted already,  
# not in doaggregate() which is where you would almost always use the results of getblocksnearby()? 
#  ***  as you join blockwts to sites2blocks, grab block_radius_miles as well, not just wts -
# -- will now try to do this join in getblocksnearby() not in doaggregate() !
#
# Also updated same code so it will use that to modify the distance of site to block.


############ #
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
#  Our investigations indicate that for any dij less than the Block Area Equivalent Radius, 0.9 times that
#  value is a reasonable representation of the average distance from the facility for all residents in the
#  block. We call this the dij corrected.
#  Our computational scheme determines the dij values as described above, tests for the comparison with
#  Block Area Equivalent Radius, and substitutes dij corrected values. We found that we needed to make
#  that correction for less than 1% of all facility/block combinations in an early testing dataset that used
#  2005-2009 ACS data.
#  ########### #
#    HOWEVER, about 8% of blocks have effective radius of at least 1 square mile !
