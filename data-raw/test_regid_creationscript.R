# usethis::use_data_raw("test_regid")

## code to prepare `test_regid` dataset  

# as.numeric(dput(frs[grepl("EJAM", PRIMARY_NAME,ignore.case = TRUE),REGISTRY_ID]))

test_regid <- c(
  110019033810,
  110030509215,
  110044340807,
  110056111559,
  110056982323,
  110070538057,
  110070874073,
  110071293460
)
usethis::use_data(test_regid, overwrite = TRUE)

# # see info on those sites via helper function
# frs_from_siteid(test_regid)
# # see info on those sites via data.table directly
# frs[REGISTRY_ID %in% test_regid, ]
# # see map of those sites
# mapfast(frs_from_siteid(test_regid))

# ## EJAMejscreenapi::testids_registry_id was this 
# testids_registry_id <- c(
#   110071293460,
#   110070874073,
#   110070538057,
#   110044340807,
#   110030509215, 
#   110019033810,
#   110056111559,
#   110056982323
#   )
# usethis::use_data(testids_registry_id, overwrite = TRUE)
# ## these were the same idea, just named a bit differently

test_xtrac <- as.numeric( c(
  "110012157520", "110020822671", "110021850558", "110022674319", 
  "110022693307", "110022742264", "110022742282", "110022742308", 
  "110030905858", "110032375873", "110033681077", "110037118482", 
  "110037118598", "110037371073", "110039062698", "110044605790", 
  "110046345442", "110055617738", "110064486818", "110070429548", 
  "110070536853", "110070581169", "110070826244", "110070892116", 
  "110071357916"
  ))
usethis::use_data(test_xtrac, overwrite = TRUE)



