# global.R defines variables needed in global environment
# init.getdata(TRUE)  #


######################################################################################################## #
############  Things needed even if not using Shiny app, to use the package at all:############
######################################################################################################## #

############ NOTE ON LOADING PACKAGES ############

# Need to clarify how to handle installation of EJAM package if deploying this app to a server.
# would be installed by someone who gets the package to run locally.
# Normally in a shiny app you should not load packages using library or require in a package,
# and just specify in DESCRIPTION file Imports instead.
# But what about in a golem-style package that is also a Shiny app? 
# If the pkg gets installed during deployment to the server, DESCRIPTION Imports handles this.
# But to deploy a shiny app, does the server know to treat it like a package that has to be installed?
# How does a deployed shiny app like this make clear  require(EJAM) ?

library(EJAM)   # # This package's functions and data (blockgroup indicators, NAICS, etc.)

# other packages ARE HANDLED BY DESCRIPTION file once EJAM package is installed:
# library(shiny)
# library(EJAMblockdata) # has weights based on census block 2020 population counts, to estimate what fraction of parent block group population is in a buffer
# library(EJAMfrsdata)  # Has Facility Registry System FRS list of regulated sites with ID and lat/lon
# library(SearchTrees)# efficient storage of block points info and selection of those within a certain distance
# library(data.table)  # much faster than data.frame, used in doaggregate() and elsewhere
# library(pdist)
# library(sp) # https://cran.r-project.org/web/packages/sp/vignettes/over.pdf
#  # Possibly less essential?: 
# library(foreach) # main reason for using foreach::foreach() is that it supports parallel execution, that is, it can execute those repeated operations on multiple processors/cores on your computer (and there are other advantages as well)
# library(doSNOW) ; library(foreach)  # parallel processing, efficient looping?
# library(rgdal) ; library(maps) ; library(pdist) #?  # Geospatial tools
# library(RMySQL) # only if loading data from SQL or using SQL for buffering as was tested in an alternative to getblocksnearby
# # not in DESCRIPTION file
# library(parallelly) # if using getblocksnearbyviaQuadTree_Clustered()

######################################################################################################## #
############  Things needed only if running this Shiny app:############
######################################################################################################## #

## text message about ECHO facility search
echo_url <-  'https://echo.epa.gov/facilities/facility-search' # used in server.R and in message below
echo_message <- shiny::HTML(paste0('To use the ECHO website to search for and specify a list of regulated facilities, 
                                    <br>1) go to ', '<a href=\"', echo_url, '\", target=\"_blank\">', echo_url,  '</a>', ' and <br>
                                    2) under Facility Characteristics Results View select data table, click Search, then <br>
                                    3) click Customize Columns, use checkboxes to include Latitude and Longitude, then <br>
                                    4) click Download Data, then <br>
                                    5) return to this app to upload that ECHO site list.<br>'))

naics_to_pick_from <- EJAM::NAICS  # maybe avoid this in case session does not need NAICS lookups? otherwise would be lazy loaded from package as data, a list of code number and name of industry, used in ui.R

options(shiny.maxRequestSize = 9*1024^2) # not sure what this was for
server <- "127.0.0.1" # not sure what this was for

############ PARAMETERS SPECIFIC TO USER OR SERVER ############

# Specifying CPU count is irrelevant unless using getblocksnearbyviaQuadTree_Clustered() and do not want default of 1 cpu
# CountCPU <- 2
# CountCPU <- parallelly::availableCores() 
# CountCPU <- parallel::detectCores() # why detectCores() is a bad idea: https://www.r-bloggers.com/2022/12/please-avoid-detectcores-in-your-r-packages/

# Probably should get passed as user-specified parameters to functions with default values, rather than putting them in a script
# indexgridsize <- 10  # unused. need to confirm if and how this grid was actually used
# translate_fieldnames <- TRUE #unused. may depend on dataset - this is about whether to rename the columns to friendlier variable names in init.getdata.R
maxcutoff_default <- 4000 # 4000 miles seems excessive, so just check if it matters for performance. for some reason the old code in server makes this into a reactive and then uses it every time getblocksnearby() is called

############ CONSTANTS ############

earthRadius_miles <- 3959 # hard coded this elsewhere but not everywhere
crd <- function(x) {2 * sin(x / 2)}

#### MUST UPDATE FOR EACH VERSION OF EJScreen ! *** ###################################
# calculate DEMOGRAPHIC INDEX for US overall, needed later #####
#
# As seen in the lookup table:
# EJScreen 2.1  (ACS2016-2020) value is 0.347071
National_Demographic_Index <- EJAM::usastats$VSI.eo[EJAM::usastats$PCTILE == 'mean']
# same answer:
#looktemp = EJAMejscreendata::USA_2022
#looktemp$VULEOPCT[looktemp$PCTILE == 'mean']
###[1] 0.347071
#
# ## compare ways of calculating national overall avg of us pctlowinc and us pctmin:
#
#  pctlowinc.us.popwtd <- stats::weighted.mean(blockgroupstats$pctlowinc, blockgroupstats$pop, na.rm = TRUE)
#  pctmin.us.popwtd    <- stats::weighted.mean(blockgroupstats$pctmin,    blockgroupstats$pop, na.rm = TRUE)
#  pctlowinc.us.careful <- sum(blockgroupstats$lowinc, na.rm = TRUE) / sum(blockgroupstats$povknownratio, na.rm = TRUE)
#  pctmin.us.careful    <- sum(blockgroupstats$mins,   na.rm = TRUE) / sum(blockgroupstats$pop,           na.rm = TRUE)
#
#  vsi.us.popwtd.vsi <- stats::weighted.mean(blockgroupstats$VSI.eo, blockgroupstats$pop, na.rm = TRUE)
#  vsi.us.popwtd.each <- mean(c( pctlowinc.us.popwtd,  pctmin.us.popwtd),  na.rm = TRUE) # simplistic if pop not povknownratio is weight
#  vsi.us.careful     <- mean(c( pctlowinc.us.careful, pctmin.us.careful), na.rm = TRUE)
#
#  rbind(pctlowinc.us.careful, pctlowinc.us.popwtd, pctmin.us.careful,pctmin.us.popwtd)
# #[,1]  # these differ only slightly for pctlowinc, and not at all for pctmin:
# # pctlowinc.us.careful 0.3233540  # **This is what Census would report as the US overall %lowinc
# # pctlowinc.us.popwtd  0.3247405  # **This is what EJSCREEN effectively uses, since % in bg, once calculated, is treated as a % of pop.
# # pctmin.us.careful    0.3956014 # already popwtd
# # pctmin.us.popwtd     0.3956014 # already popwtd
#
# rbind(vsi.us.careful, vsi.us.popwtd.each, vsi.us.popwtd.vsi)
# # [,1]
# # vsi.us.careful     0.359 477   # ** only slighly different -- mean of the two US values that are exact overall US % of correct denominator
# # vsi.us.popwtd.each 0.360 171   # mean of the two US values that are popwtd means of bgs
# # vsi.us.popwtd.vsi  0.360 142   # mean of popwtd means is almost same as popwtd mean of means
# #
# National_Demographic_Index <-  vsi.us.careful
