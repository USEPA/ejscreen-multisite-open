  


# \preformatted{
  #   ######################################################################################
  #   # How to add Puerto Rico demographic subgroup info:
  #   ######################################################################################
  # 
  #   # For all block groups in US other than in PR,
  #   #  bg22DemographicSubgroups got B03002 using the script
  #   # but that did not include PR. Puerto Rico was missing if using ACSDownload package code.
  #   #   setdiff(substr(bg22$FIPS, 1,2), substr(bg22DemographicSubgroups2016to2020$FIPS,1,2))
  #   #   # "72" which is the FIPS code for Puerto Rico.
  # 
  #   # PR table B03002 from ACS2016-2020 block groups was obtained from
  #   # browseURL('https://data.census.gov/cedsci/table?g=0100000US_0400000US72%241500000&y=2020&tid=ACSDT5Y2020.B03002')
  # 
  #   # Get Puerto Rico table B03002
  #   prsub <- read.csv('')
  #   # rename headers
  #   # calculated variables
  #   # etc.
  # 
  #   # Merge prsub into bg22DemographicSubgroups
  #   bg22DemographicSubgroups <- merge(bg22DemographicSubgroups, prsub, by='FIPS', all=TRUE)
  #   # check it
  # }
  # \preformatted{
  #   ######################################################################################
  #   # How to merge demographic subgroup info into the basic EJSCREEN bg dataset:
  #   ######################################################################################
  # 
  #   d <- bg22DemographicSubgroups2016to2020
  #   d <- subset.data.frame(x = d, subset = !(names(d) %in% c('pop', 'mins', 'pctmin')) )
  # 
  #   # Once bg22DemographicSubgroups has PR table B03002, merge that into complete dataset:
  # 
  #   bg22plus <- merge(bg22, d, by = 'FIPS', all.x = TRUE)
  #    usethis::use_data(bg22plus)
  #   ## save(bg22plus, file = 'bg22plus EJSCREEN dataset plus race ethnic subgroups.rdata')
  #   ## write.csv(bg22plus, file = 'bg22plus EJSCREEN dataset plus race ethnic subgroups.csv')
  #   ##########################################
  # 
  #       subset.data.frame(x = names(bg22plus), subset =  !(names(bg22plus) %in% names(bg22)) )
  #   }