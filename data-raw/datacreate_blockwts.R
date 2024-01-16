#################################################################################### # 

# Census 2020 block data.table tables 
# blockpoints, blockwts, quaddata, blockid2fips
# 
# were created using a non-CRAN package (in github) called census2020download:

# devtools::install_github("ejanalysis/census2020download")
# require # (census2020download)
# ?census2020download
#
# blocks <- census2020_get_data()
# xlist  <- census2020_save_datasets(blocks, save_as_data_for_package = FALSE)
#
# rm(blocks)
# names(xlist)

# Those block data.table objecs were then saved via pins board, 
# explained in EJAM/data-raw/datacreate_pins.R

#################################################################################### # 

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
#################################################################################### # 
