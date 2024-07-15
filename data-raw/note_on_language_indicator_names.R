


# p_spanish	and pct_lan_spanish
# in map headernames are two rnames for the same thing, it seems like.
# P_SPANISH from API output seems same as what acs file CALLED PCT_LAN_SPANISH, 
# so p_spanish should merge with pct_lan_spanish ? and same for other languages.
  

###############################

#  variable names for language spoken, as used in the ACS22 table from kb/vz

# x = grep("spanish", datafile_acs22acsgdbnames, ignore.case = T, value = T)
# x = cbind(x,      
#           fixcolnames(x,"old","r"), 
#           fixcolnames(x,"csv","r"), 
#           fixcolnames(x,"api","r"))
# x[order(x[,1]),]
# 
# 
#        in that acs file     old2r                 csv2r                 api2r
# 
# 
# [1,] "HLI_SPANISH"         "HLI_SPANISH"         "HLI_SPANISH"         "HLI_SPANISH"             - will not use  
# [2,] "HLI_SPANISH_LI"     ** "spanish_li"          "HLI_SPANISH_LI"      "HLI_SPANISH_LI"  * count of sp lang but only if lingiso
# [3,] "HLI_SPANISH_NLI"     "HLI_SPANISH_NLI"     "HLI_SPANISH_NLI"     "HLI_SPANISH_NLI"         - will not use 
# 
# [4,] "LAN_SPANISH"        ** "lan_spanish"         "LAN_SPANISH"         "LAN_SPANISH"     * count of sp lang at home (in total pop)   
# 

# 
# [5,] "PCT_HLI_SPANISH"     "PCT_HLI_SPANISH"     "PCT_HLI_SPANISH"     "PCT_HLI_SPANISH"          - will not use
# [6,] "PCT_HLI_SPANISH_LI"  "PCT_HLI_SPANISH_LI"  "PCT_HLI_SPANISH_LI"  "PCT_HLI_SPANISH_LI"  * what api calls  P_HLI_SPANISH_LI? as % of lingiso
# [7,] "PCT_HLI_SPANISH_NLI" "PCT_HLI_SPANISH_NLI" "PCT_HLI_SPANISH_NLI" "PCT_HLI_SPANISH_NLI"      - will not use
# 
# [8,] "PCT_LAN_SPANISH"    ** "PCT_LAN_SPANISH"  **  "PCT_LAN_SPANISH"     "PCT_LAN_SPANISH"     *  what api calls  P_SPANISH  ? as % of total pop
# 
# ############################


# # The v2.2 api documentation showed 2 API name types, both different than naming used in acs etc names above
# # 
# # 
# # 
# #  P_HLI_SPANISH_LI	    [ as % of lingiso ? ] Speak Spanish	--  Breakdown by Limited English Speaking
# # 
# # cbind("P_HLI_SPANISH_LI", fixcolnames("P_HLI_SPANISH_LI","old","r"), fixcolnames("P_HLI_SPANISH_LI","csv","r"), fixcolnames("P_HLI_SPANISH_LI","api","r"))
# # [,1]               [,2]            [,3]                    
# # [1,] "P_HLI_SPANISH_LI"       "pctspanish_li"     old2r
#                                 "P_HLI_SPANISH_LI"   csv2r 
#                                  "pctspanish_li"      api2r 
                                  
                                 
# # P_SPANISH	      [ as % of whole pop ? ]   Percent of population speaking Spanish at home  -- 	Languages Spoken at Home
# 
# # >  cbind("P_SPANISH",
# #          fixcolnames("P_SPANISH","old","r"),
# #          fixcolnames("P_SPANISH","csv","r"),
# #          fixcolnames("P_SPANISH","api","r"))
# #
# # [1,] "P_SPANISH"   **   "p_spanish"    old2r
# #                       "P_SPANISH"   csv2r
# #                       "p_spanish"  api2r
