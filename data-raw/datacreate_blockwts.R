#################################################################################### # 

# Census 2020 block data.table tables 
# blockpoints, blockwts, quaddata, blockid2fips
# 
# were created using a non-CRAN package (in github) called census2020download:

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

# Those block data.table objecs were then saved via pins board, 
# explained in EJAM/data-raw/datacreate_pins.R

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
