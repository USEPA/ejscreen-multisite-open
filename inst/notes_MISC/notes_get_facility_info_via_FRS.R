################################################################### #
# FUNCTION locate_by_id() TO QUERY FRS ########### #
# and test data and examples  
################################################################### #

################################################################### #
# API info  ####
# 
# browseURL('https://www.epa.gov/frs/frs-rest-services')
# browseURL('https://www.epa.gov/frs/frs-rest-services#ex1') # examples are shown   
# browseURL('https://www.epa.gov/frs/frs-rest-services#appendixa') 
#
# FRS lookup API's get_facilities
#   (to get lat lon by facility's registry ID or program/system ID):
#
# # Public 
#  # browseURL('https://frs-public.epa.gov/ords/frs_public2/frs_rest_services.get_facilities?pgm_sys_id=VA0088986')
#  # browseURL('https://frs-public.epa.gov/ords/frs_public2/frs_rest_services.get_facilities?registry_id=110010912496')
#  #
# # Internal use?? API URL for internal use at EPA appears to be different than public one?:
#  # browseURL('https://ofmpub.epa.gov/frs_public2/frs_rest_services.get_facilities?registry_id=110005250682')

################################################################### #
################################################################### #



################################################################### #
# TEST DATA AND EXAMPLES ############
#
if (1==0) {
  testids_registry_id <- c("110071102551", "110015787683", "110070215797", "110068643953", 
                           "110044179153", "110009560340", "110040482714", "110025017425", "110008308170")
  testids_program_sys_id <- c("AK0001413699", "AK0000000201600016", "AKU000322", "198279",
                              "CTGSI2508", "COD981540909", "0804501789", "4500122", "CTD983873928" )
  sites_found_by_registry_id <- locate_by_id(testids_registry_id[1],    type='frs') # 
  sites_found_by_program_id  <- locate_by_id(testids_program_sys_id[1], type='program')
  data.frame(example_REGID  <- t(sites_found_by_registry_id))
  data.frame(example_PGMID  <- t(sites_found_by_program_id))
  ##
  ## "RegistryId"      "FacilityName"    
  ## "LocationAddress", "CityName", "CountyName", "StateAbbr", "ZipCode"
  ## "FIPSCode", "Latitude83", "Longitude83" 
  ##
  ## RegistryId                                                     110071102551
  ## FacilityName         USDOI FWS AK MARITIME NWR: TANAGA ISLAND NAVAL STATION
  ## LocationAddress                             65 MI W. OF ADAK NAVAL FACILITY
  ## SupplementalLocation                                                       
  ## CityName                                                               ADAK
  ## CountyName                                                 ALEUTIAN ISLANDS
  ## StateAbbr                                                                AK
  ## ZipCode                                                               99546
  ## FIPSCode                                                              02010
  ## Latitude83                                                        51.671389
  ## Longitude83                                                     -178.051111
  
  # takes a bit longer: 
  sites_found_by_program_id  <- locate_by_id(testids_program_sys_id, type='program')
  sites_found_by_registry_id <- locate_by_id(testids_registry_id,    type='frs')
  # names(sites_found_by_program_id)
  ## and this is just to show agreement. The file frs.rdata was 100MB and do not really need it here.
  cbind(testids_program_sys_id,
        frs = EJAMfrsdata::frs[match(sites_found_by_program_id$RegistryId, EJAMfrsdata::frs$REGISTRY_ID) , c('PGM_SYS_ACRNMS',  'REGISTRY_ID')],
        api = sites_found_by_program_id[, c('RegistryId', 'Latitude83', 'Longitude83'  ) ] )
  # 
  
  xl <- c(-125,-66); yl <- c(17,50) # just continental US plus PR
  ## xl <- c(-170,-60), yl <- c(17,72)
  # view US map via sampling of sites in FRS:
  plot(EJAMfrsdata::frs[sample(1:NROW(EJAMfrsdata::frs), 5000), c('lon', 'lat')], xlim=xl, ylim=yl, col='gray')
  # view the queried sites among all that:
  graphics::points(cbind(LONG=sites_found_by_program_id$Longitude83, LAT=sites_found_by_program_id$Latitude83), col='red', pch=16)
}
################################################################### #

