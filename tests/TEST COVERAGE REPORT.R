# !diagnostics off
##   disables diagnostics within this document

if (1 == 0) { # so it does not do anything if source() is done by testing 
  
# # Needed only for this script:
  # 
  library(fs)
  library(tidyverse)

tdat = bind_rows(
  tibble(
    type = "R",
    path = dir_ls("R/", regexp = "\\.[Rr]$"),
    name = as.character(path_ext_remove(path_file(path))),
  ),
  tibble(
    type = "test",
    path = dir_ls("tests/testthat/", regexp = "/test[^/]+\\.[Rr]$"),
    name = as.character(path_ext_remove(str_remove(path_file(path), "^test[-_]"))),
  )
) %>%
  pivot_wider(names_from = type, values_from = path) 

tdat %>%   print(n = Inf)

################################ # 

# see all TEST FILES that were not named based on a .R file, 
#   making it hard to know which .R files really lack tests

tdat[!is.na(tdat$test) & is.na(tdat$R),  ] |> print(n = 500)

# # A tibble: 24 × 3
# name                                R          test                          9/8/24                           
# <chr>                               <fs::path> <fs::path>                                               
#   1 address_xyz                         NA         tests/testthat/test-address_xyz.R                        
# 2 ejscreenRESTbroker-functions        NA         tests/testthat/test-ejscreenRESTbroker-functions.R    

# 3 frs_from_naics                      NA         tests/testthat/test-frs_from_naics.R                     
# 4 frs_from_programid                  NA         tests/testthat/test-frs_from_programid.R                 
# 5 frs_from_regid                      NA         tests/testthat/test-frs_from_regid.R                     
# 6 frs_from_sic                        NA         tests/testthat/test-frs_from_sic.R   

# 7 getblocks_summarize_blocks_per_site NA         tests/testthat/test-getblocks_summarize_blocks_per_site.R

# 8 golem_utils_server                  NA         tests/testthat/test-golem_utils_server.R                 
# 9 golem_utils_ui                      NA         tests/testthat/test-golem_utils_ui.R   

# 10 latlon_from_sic                     NA         tests/testthat/test-latlon_from_sic.R  

# 11 mod_save_report                     NA         tests/testthat/test-mod_save_report.R                    
# 12 mod_specify_sites                   NA         tests/testthat/test-mod_specify_sites.R                  
# 13 mod_view_results                    NA         tests/testthat/test-mod_view_results.R    

# 14 naics2children                      NA         tests/testthat/test-naics2children.R                     
# 15 naics_categories                    NA         tests/testthat/test-naics_categories.R                   
# 16 naics_findwebscrape                 NA         tests/testthat/test-naics_findwebscrape.R                
# 17 naics_from_any                      NA         tests/testthat/test-naics_from_any.R                     
# 18 naics_from_code                     NA         tests/testthat/test-naics_from_code.R                    
# 19 naics_from_name                     NA         tests/testthat/test-naics_from_name.R                    
# 20 naics_subcodes_from_code            NA         tests/testthat/test-naics_subcodes_from_code.R           
# 21 naics_validation                    NA         tests/testthat/test-naics_validation.R       

# 22 regid_from_naics                    NA         tests/testthat/test-regid_from_naics.R                   
# 23 state_from_fips_bybg                NA         tests/testthat/test-state_from_fips_bybg.R    

# 24 ui_and_server                       NA         tests/testthat/test-ui_and_server.R                      
# > 


################################ # 

# see all .R files that lack a test file with exactly matching name

tdat[is.na(tdat$test) & !is.na(tdat$R) & "data_" != substr(tdat$name, 1,5), ] |> print(n = 500)
# A tibble: 161 × 3               9/8/24
# name                                       R                                              test      
# <chr>                                      <fs::path>                                     <fs::path>
#  1 0_run_app                                  R/0_run_app.R                                  NA        
#  2 aaa_onAttach                               R/aaa_onAttach.R                               NA   

#  3 acs_bybg                                   R/acs_bybg.R                                   NA        
#  4 acs_bycounty                               R/acs_bycounty.R                               NA  

#  5 app_config                                 R/app_config.R                                 NA        
#  6 app_run_EJAM                               R/app_run_EJAM.R                               NA        
#  7 app_run_EJAMejscreenapi                    R/app_run_EJAMejscreenapi.R                    NA        
#  8 app_server                                 R/app_server.R                                 NA        
#  9 app_server_EJAMejscreenapi                 R/app_server_EJAMejscreenapi.R                 NA        
# 10 app_ui                                     R/app_ui.R                                     NA        
# 11 app_ui_EJAMejscreenapi                     R/app_ui_EJAMejscreenapi.R                     NA  

# 12 batch.summarize.helpers                    R/batch.summarize.helpers.R                    NA        
# 13 batch.summarize                            R/batch.summarize.R                            NA   

# 14 boxplots_ratios                            R/boxplots_ratios.R                            NA   

# 15 build_community_report                     R/build_community_report.R                     NA   

# 16 calc_ratios_to_avg                         R/calc_ratios_to_avg.R                         NA        
# 17 calc_wtdmeans                              R/calc_wtdmeans.R                              NA  

# 18 colcounter_xyz                             R/colcounter_xyz.R                             NA  

# 19 community_report_helper_funs               R/community_report_helper_funs.R               NA  

# 20 convert_units                              R/convert_units.R                              NA  

# 21 count_sites_with_n_high_scores             R/count_sites_with_n_high_scores.R             NA  

# 22 create_filename                            R/create_filename.R                            NA  

# 23 custom_doaggregate                         R/custom_doaggregate.R                         NA   

# 24 dataload_from_aws                          R/dataload_from_aws.R                          NA        
# 25 dataload_from_entirefolder                 R/dataload_from_entirefolder.R                 NA        
# 26 dataload_from_local                        R/dataload_from_local.R                        NA        
# 27 dataload_from_package                      R/dataload_from_package.R                      NA        
# 28 dataload_from_pins                         R/dataload_from_pins.R                         NA        
# 29 datawrite_to_aws                           R/datawrite_to_aws.R                           NA        
# 30 datawrite_to_local                         R/datawrite_to_local.R                         NA        
# 31 datawrite_to_pins                          R/datawrite_to_pins.R                          NA    

# 32 distances.all                              R/distances.all.R                              NA        
# 33 distance_by_groups_by_site-DRAFT           R/distance_by_groups_by_site-DRAFT.R           NA        
# 34 distance_near_eachother                    R/distance_near_eachother.R                    NA        
# 35 distance_via_surfacedistance               R/distance_via_surfacedistance.R               NA        
# 36 echo_colids_from_num_name_group            R/echo_colids_from_num_name_group.R            NA  

# 37 EJAM-package                               R/EJAM-package.R                               NA        
# 38 ejam2excel                                 R/ejam2excel.R                                 NA        
# 39 ejam2map                                   R/ejam2map.R                                   NA        
# 40 ejam2ratios                                R/ejam2ratios.R                                NA        
# 41 ejam2report                                R/ejam2report.R                                NA        
# 42 ejam2tableviewer                           R/ejam2tableviewer.R                           NA        
# 43 ejam2table_tall                            R/ejam2table_tall.R                            NA    

# 44 ejscreenit_for_ejam                        R/ejscreenit_for_ejam.R                        NA        
# 45 ejscreenit_see_table                       R/ejscreenit_see_table.R                       NA        
# 46 ejscreenRESTbroker                         R/ejscreenRESTbroker.R                         NA        
# 47 ejscreenRESTbroker2table                   R/ejscreenRESTbroker2table.R                   NA        
# 48 ejscreen_vs_ejam_                          R/ejscreen_vs_ejam_.R                          NA   

# 49 expand.gridMatrix                          R/expand.gridMatrix.R                          NA        
# 50 fixcolnames2related                        R/fixcolnames2related.R                        NA        
# 51 fixmapheadernamescolname                   R/fixmapheadernamescolname.R                   NA        
# 52 fixnames_aliases                           R/fixnames_aliases.R                           NA        
#    53 fix_pctcols_x100   R/fix_pctcols_x100.R             renamed to be table_x100.R, table_x100()

# 54 frs_active_ids                             R/frs_active_ids.R                             NA        
# 55 frs_clean                                  R/frs_clean.R                                  NA        
# 56 frs_clean_sic                              R/frs_clean_sic.R                              NA        
# 57 frs_download                               R/frs_download.R                               NA        
# 58 frs_drop_inactive                          R/frs_drop_inactive.R                          NA        
# 59 frs_from_xyz                               R/frs_from_xyz.R                               NA        
# 60 frs_get                                    R/frs_get.R                                    NA        
# 61 frs_make_mact_lookup                       R/frs_make_mact_lookup.R                       NA        
# 62 frs_make_naics_lookup                      R/frs_make_naics_lookup.R                      NA        
# 63 frs_make_programid_lookup                  R/frs_make_programid_lookup.R                  NA        
# 64 frs_make_sic_lookup                        R/frs_make_sic_lookup.R                        NA        
# 65 frs_read                                   R/frs_read.R                                   NA        
# 66 frs_unzip                                  R/frs_unzip.R                                  NA        
# 67 frs_update_datasets                        R/frs_update_datasets.R                        NA   

# 68 getblocksnearby2-DRAFT                     R/getblocksnearby2-DRAFT.R                     NA        
# 69 getblocksnearbyviaQuadTree2-DRAFT          R/getblocksnearbyviaQuadTree2-DRAFT.R          NA        
# 70 getblocksnearbyviaQuadTree3-DRAFT          R/getblocksnearbyviaQuadTree3-DRAFT.R          NA        
# 71 getblocksnearbyviaQuadTree_Clustered-DRAFT R/getblocksnearbyviaQuadTree_Clustered-DRAFT.R NA        
# 72 getblocks_diagnostics                      R/getblocks_diagnostics.R                      NA        
# 73 getfrsnearby                               R/getfrsnearby.R                               NA        
# 74 get_blockpoints_in_shape                   R/get_blockpoints_in_shape.R                   NA   

# 75 latlon_from_mactsubpart                    R/latlon_from_mactsubpart.R                    NA        
# 76 latlon_from_map_click_demo                 R/latlon_from_map_click_demo.R                 NA        
# 77 latlon_from_naics                          R/latlon_from_naics.R                          NA        
# 78 latlon_from_program                        R/latlon_from_program.R                        NA        
# 79 latlon_from_programid                      R/latlon_from_programid.R                      NA        
# 80 latlon_from_regid                          R/latlon_from_regid.R                          NA        
# 81 latlon_from_s2b                            R/latlon_from_s2b.R                            NA        
# 82 latlon_join_on_blockid                     R/latlon_join_on_blockid.R                     NA  

# 83 mapfast                                    R/mapfast.R                                    NA        
# 84 map_google                                 R/map_google.R                                 NA        
# 85 metadata_mapping                           R/metadata_mapping.R                           NA   

# 86 MODULE_ejscreenapi                         R/MODULE_ejscreenapi.R                         NA 
# 87 MODULE_latlontypedin                       R/MODULE_latlontypedin.R                       NA        
# 88 module_latlontypedin_DEMO                  R/module_latlontypedin_DEMO.R                  NA        
# 89 mod_fips_picker-DRAFT                      R/mod_fips_picker-DRAFT.R                      NA        
# 90 mod_get_shape_from_upload-DRAFT            R/mod_get_shape_from_upload-DRAFT.R            NA        
# 91 mod_save_report-DRAFT                      R/mod_save_report-DRAFT.R                      NA        
# 92 mod_specify_sites-DRAFT                    R/mod_specify_sites-DRAFT.R                    NA        
# 93 mod_view_results-DRAFT                     R/mod_view_results-DRAFT.R                     NA  

# 94 NAICS_FUNCTIONS                            R/NAICS_FUNCTIONS.R                            NA        
# 95 naics_is.valid                             R/naics_is.valid.R                             NA        
# 96 namesbyvarlist                             R/namesbyvarlist.R                             NA        
# 97 pctiles_from_raw_lookup_DRAFT              R/pctiles_from_raw_lookup_DRAFT.R              NA  

# 98 plotblocksnearby                           R/plotblocksnearby.R                           NA        
# 99 plot_barplot_ratios                        R/plot_barplot_ratios.R                        NA        
# 100 plot_demogshare_by_distance                R/plot_demogshare_by_distance.R                NA        
# 101 plot_distance_by_pctd                      R/plot_distance_by_pctd.R                      NA        
# 102 plot_distance_cdf_by_group                 R/plot_distance_cdf_by_group.R                 NA        
# 103 plot_distance_mean_by_group                R/plot_distance_mean_by_group.R                NA        
# 104 plot_lorenz_popcount_by_site - DRAFT       R/plot_lorenz_popcount_by_site - DRAFT.R       NA        
# 105 plot_ridgeline_ratios                      R/plot_ridgeline_ratios.R                      NA        
# 106 plot_vs_us - DRAFT                         R/plot_vs_us - DRAFT.R                         NA        
# 107 popshare_xyz                               R/popshare_xyz.R                               NA   

# 108 popup_from_any                             R/popup_from_any.R                             NA        
# 109 popup_from_df                              R/popup_from_df.R                              NA        
# 110 popup_from_ejscreen                        R/popup_from_ejscreen.R                        NA        
# 111 popup_from_uploadedpoints                  R/popup_from_uploadedpoints.R                  NA        
# 112 proximity.score.in.miles                   R/proximity.score.in.miles.R                   NA        
# 113 proxistat2-DRAFT                           R/proxistat2-DRAFT.R                           NA        
# 114 read_and_clean_points                      R/read_and_clean_points.R                      NA        
# 115 read_csv_or_xl                             R/read_csv_or_xl.R                             NA  

# 116 sic_categories                             R/sic_categories.R                             NA        
# 117 sic_xyz                                    R/sic_xyz.R                                    NA    

# 118 sitepoints_from_any                        R/sitepoints_from_any.R                        NA 

# 119 speedreport                                R/speedreport.R                                NA        
# 120 statestats_query                           R/statestats_query.R                           NA        
# 121 state_from_s2b_bysite                      R/state_from_s2b_bysite.R                      NA        
# 122 state_per_site_for_doaggregate             R/state_per_site_for_doaggregate.R             NA   

# 123 tablefixed                                 R/tablefixed.R                                 NA        
# 124 table_gt_                                  R/table_gt_.R                                  NA        
# 125 table_order_variables                      R/table_order_variables.R                      NA        
# 126 table_round                                R/table_round.R                                NA        
# 127 table_rounding_info                        R/table_rounding_info.R                        NA        
# 128 table_xls_format                           R/table_xls_format.R                           NA        
# 129 table_xls_formatting_api                   R/table_xls_formatting_api.R                   NA        
# 130 table_xls_from_ejam                        R/table_xls_from_ejam.R                        NA        
# 131 table_xls_varname2color                    R/table_xls_varname2color.R                    NA   

# 132 testpoints_n                               R/testpoints_n.R                               NA

# 133 URL_FUNCTIONS_part1                        R/URL_FUNCTIONS_part1.R                        NA        
# 134 URL_FUNCTIONS_part2                        R/URL_FUNCTIONS_part2.R                        NA    

# 135 utils_ask_number                           R/utils_ask_number.R                           NA        
# 136 utils_calc_ejam                            R/utils_calc_ejam.R                            NA        
# 137 utils_check_urls_in_documentation          R/utils_check_urls_in_documentation.R          NA        
# 138 utils_datapack                             R/utils_datapack.R                             NA        
# 139 utils_draft_get_facility_info_via_ECHO     R/utils_draft_get_facility_info_via_ECHO.R     NA        
# 140 utils_dupenames                            R/utils_dupenames.R                            NA        
# 141 utils_global_or_param                      R/utils_global_or_param.R                      NA        
# 142 utils_golem_utils_server                   R/utils_golem_utils_server.R                   NA        
# 143 utils_golem_utils_ui                       R/utils_golem_utils_ui.R                       NA        
# 144 utils_indexblocks                          R/utils_indexblocks.R                          NA        
# 145 utils_input_names                          R/utils_input_names.R                          NA        
# 146 utils_linesofcode_                         R/utils_linesofcode_.R                         NA        
# 147 utils_lint_equals                          R/utils_lint_equals.R                          NA        
# 148 utils_locate_by_id                         R/utils_locate_by_id.R                         NA        
# 149 utils_makenumericdfFORSHINY                R/utils_makenumericdfFORSHINY.R                NA        
# 150 utils_names_whichlist                      R/utils_names_whichlist.R                      NA        
# 151 utils_nearRcolor                           R/utils_nearRcolor.R                           NA        
# 152 utils_pctiles_lookup_create                R/utils_pctiles_lookup_create.R                NA        
# 153 utils_pins_available                       R/utils_pins_available.R                       NA        
# 154 utils_rmost                                R/utils_rmost.R                                NA        
# 155 utils_rtips                                R/utils_rtips.R                                NA        
# 156 utils_source_this_codetext                 R/utils_source_this_codetext.R                 NA        
# 157 utils_speedtest                            R/utils_speedtest.R                            NA        
# 158 utils_structure.of.output.list             R/utils_structure.of.output.list.R             NA        
# 159 varin_map_headernames                      R/varin_map_headernames.R                      NA        
# 160 var_is_numeric_ish                         R/var_is_numeric_ish.R                         NA        
# 161 _disable_autoload                          R/_disable_autoload.R                          NA        
# > 

################################ # 


# MATCHED EXACTLY -- see all test files that exactly match name of a .R file


tdat[!is.na(tdat$test) & !is.na(tdat$R), c("R", "test")] |> print(n = 500)

# A tibble: 35 × 2
# R                                  test                                                
# <fs::path>                         <fs::path>                                          
#   1 R/doaggregate.R                    tests/testthat/test-doaggregate.R                   
# 2 R/ejam2barplot_sites.R             tests/testthat/test-ejam2barplot_sites.R            
# 3 R/ejam2shapefile.R                 tests/testthat/test-ejam2shapefile.R                
# 4 R/ejamit.R                         tests/testthat/test-ejamit.R                        
# 5 R/ejamit_compare_distances.R       tests/testthat/test-ejamit_compare_distances.R      
# 6 R/ejamit_compare_types_of_places.R tests/testthat/test-ejamit_compare_types_of_places.R
   # 7 R/ejamit_sitetype_check.R          tests/testthat/test-ejamit_sitetype_check.R         renaming to ejamit_sitetype_from_input
# 8 R/ejscreenapi.R                    tests/testthat/test-ejscreenapi.R                   
# 9 R/ejscreenapi1.R                   tests/testthat/test-ejscreenapi1.R                  
# 10 R/ejscreenapi_plus.R               tests/testthat/test-ejscreenapi_plus.R              
# 11 R/ejscreenit.R                     tests/testthat/test-ejscreenit.R                    
# 12 R/FIPS_FUNCTIONS.R                 tests/testthat/test-FIPS_FUNCTIONS.R                
# 13 R/fixcolnames.R                    tests/testthat/test-fixcolnames.R                   
# 14 R/fixcolnames_infer.R              tests/testthat/test-fixcolnames_infer.R             
# 15 R/fixnames.R                       tests/testthat/test-fixnames.R                      
# 16 R/fixnames_to_type.R               tests/testthat/test-fixnames_to_type.R              
# 17 R/frs_is_valid.R                   tests/testthat/test-frs_is_valid.R                  
# 18 R/getblocksnearby.R                tests/testthat/test-getblocksnearby.R               
# 19 R/getblocksnearbyviaQuadTree.R     tests/testthat/test-getblocksnearbyviaQuadTree.R    
# 20 R/getblocksnearby_from_fips.R      tests/testthat/test-getblocksnearby_from_fips.R     
# 21 R/latlon_as.numeric.R              tests/testthat/test-latlon_as.numeric.R             
# 22 R/latlon_df_clean.R                tests/testthat/test-latlon_df_clean.R               
# 23 R/latlon_from_address.R            tests/testthat/test-latlon_from_address.R           
# 24 R/latlon_from_anything.R           tests/testthat/test-latlon_from_anything.R          
# 25 R/latlon_from_vectorofcsvpairs.R   tests/testthat/test-latlon_from_vectorofcsvpairs.R  
# 26 R/latlon_infer.R                   tests/testthat/test-latlon_infer.R                  
# 27 R/latlon_is.valid.R                tests/testthat/test-latlon_is.valid.R               
# 28 R/MAP_FUNCTIONS.R                  tests/testthat/test-MAP_FUNCTIONS.R                 
# 29 R/pctile_from_raw_lookup.R         tests/testthat/test-pctile_from_raw_lookup.R        
# 30 R/radius_inferred.R                tests/testthat/test-radius_inferred.R               
# 31 R/shapefile_xyz.R                  tests/testthat/test-shapefile_xyz.R                 
# 32 R/state_from_latlon.R              tests/testthat/test-state_from_latlon.R             
# 33 R/state_from_sitetable.R           tests/testthat/test-state_from_sitetable.R          
# 34 R/utils_metadata_add.R             tests/testthat/test-utils_metadata_add.R            
# 35 R/varinfo.R                        tests/testthat/test-varinfo.R                       
# > 
}
