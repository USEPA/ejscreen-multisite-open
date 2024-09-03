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

# A tibble: 23 × 3
# name                                R          test                                                     
# <chr>                               <fs::path> <fs::path>      
#   
# 1 address_xyz                         NA         tests/testthat/test-address_xyz.R                        
# 2 frs_from_naics                      NA         tests/testthat/test-frs_from_naics.R                     
# 3 frs_from_programid                  NA         tests/testthat/test-frs_from_programid.R                 
# 4 frs_from_regid                      NA         tests/testthat/test-frs_from_regid.R                     
# 5 frs_from_sic                        NA         tests/testthat/test-frs_from_sic.R                       

# 6 getblocks_summarize_blocks_per_site NA         tests/testthat/test-getblocks_summarize_blocks_per_site.R

# 7 golem_utils_server                  NA         tests/testthat/test-golem_utils_server.R                 
# 8 golem_utils_ui                      NA         tests/testthat/test-golem_utils_ui.R                     

# 9 latlon_from_sic                     NA         tests/testthat/test-latlon_from_sic.R                    

# 10 mod_save_report                     NA         tests/testthat/test-mod_save_report.R                    
# 11 mod_specify_sites                   NA         tests/testthat/test-mod_specify_sites.R                  
# 12 mod_view_results                    NA         tests/testthat/test-mod_view_results.R                   

# 13 naics2children                      NA         tests/testthat/test-naics2children.R                     
# 14 naics_categories                    NA         tests/testthat/test-naics_categories.R                   
# 15 naics_findwebscrape                 NA         tests/testthat/test-naics_findwebscrape.R                
# 16 naics_from_any                      NA         tests/testthat/test-naics_from_any.R                     
# 17 naics_from_code                     NA         tests/testthat/test-naics_from_code.R                    
# 18 naics_from_name                     NA         tests/testthat/test-naics_from_name.R                    
# 19 naics_subcodes_from_code            NA         tests/testthat/test-naics_subcodes_from_code.R           
# 20 naics_validation                    NA         tests/testthat/test-naics_validation.R                   
# 21 regid_from_naics                    NA         tests/testthat/test-regid_from_naics.R                   

# 22 state_from_fips_bybg                     NA         tests/testthat/test-state_from_fips_bybg.R                    

# 23 ui_and_server                       NA         tests/testthat/test-ui_and_server.R    



################################ # 

# see all .R files that lack a test file with exactly matching name

tdat[is.na(tdat$test) & !is.na(tdat$R) & "data_" != substr(tdat$name, 1,5), ] |> print(n = 500)

# A tibble: 127 × 3
#     name                                       R                                              test      
# <chr>                                      <fs::path>                                     <fs::path>
# 1 0_run_app                                  R/0_run_app.R                                  NA        
# 2 aaa_onAttach                               R/aaa_onAttach.R                               NA        

# 3 acs_bybg                                   R/acs_bybg.R                                   NA        
# 4 acs_bycounty                               R/acs_bycounty.R                               NA        

# 5 app_config                                 R/app_config.R                                 NA        
# 6 app_run_EJAM                               R/app_run_EJAM.R                               NA        
# 7 app_server                                 R/app_server.R                                 NA        
# 8 app_ui                                     R/app_ui.R                                     NA        

# 9 boxplots_ratios                            R/boxplots_ratios.R                            NA        
# 10 build_community_report                     R/build_community_report.R                     NA        

# 11 colcounter_xyz                             R/colcounter_xyz.R                             NA        
# 12 community_report_helper_funs               R/community_report_helper_funs.R               NA        
# 13 convert_units                              R/convert_units.R                              NA        
# 14 count_sites_with_n_high_scores             R/count_sites_with_n_high_scores.R             NA        
# 15 create_filename                            R/create_filename.R                            NA        

# 16 custom_doaggregate                         R/custom_doaggregate.R                         NA        

# 17 dataload_from_aws                          R/dataload_from_aws.R                          NA        
# 18 dataload_from_entirefolder                 R/dataload_from_entirefolder.R                 NA        
# 19 dataload_from_local                        R/dataload_from_local.R                        NA        
# 20 dataload_from_package                      R/dataload_from_package.R                      NA        
# 21 dataload_from_pins                         R/dataload_from_pins.R                         NA        
# 22 datawrite_to_aws                           R/datawrite_to_aws.R                           NA        
# 23 datawrite_to_local                         R/datawrite_to_local.R                         NA        

# 24 distance_by_groups_by_site-DRAFT           R/distance_by_groups_by_site-DRAFT.R           NA        
# 25 distance_via_surfacedistance               R/distance_via_surfacedistance.R               NA        
# 26 EJAM-package                               R/EJAM-package.R                               NA        

# 27 ejam2excel                                 R/ejam2excel.R                                 NA        
# 28 ejam2map                                   R/ejam2map.R                                   NA        
# 29 ejam2ratios                                R/ejam2ratios.R                                NA        
# 30 ejam2report                                R/ejam2report.R                                NA        
# 31 ejam2shapefile                             R/ejam2shapefile.R                             NA        

# 32 ejamit_compare_distances                   R/ejamit_compare_distances.R                   NA        
# 33 ejamit_compare_types_of_places            R/ejamit_compare_types_of_places.R            NA        
# 34 ejscreenit_for_ejam                        R/ejscreenit_for_ejam.R                        NA        
# 35 ejscreen_vs_ejam_                          R/ejscreen_vs_ejam_.R                          NA  

# 36 FIPS_FUNCTIONS                             R/FIPS_FUNCTIONS.R                             NA        

# 37 fixcolnames2related                        R/fixcolnames2related.R                        NA        
# 38 fixmapheadernamescolname                   R/fixmapheadernamescolname.R                   NA        
# 39 fixnames_aliases                           R/fixnames_aliases.R                           NA        
# 40 fix_pctcols_x100                           R/fix_pctcols_x100.R                           NA        
# 41 frs_from_xyz                               R/frs_from_xyz.R                               NA        
# 42 getblocksnearby2-DRAFT                     R/getblocksnearby2-DRAFT.R                     NA        
# 43 getblocksnearbyviaQuadTree2-DRAFT          R/getblocksnearbyviaQuadTree2-DRAFT.R          NA        
# 44 getblocksnearbyviaQuadTree3-DRAFT          R/getblocksnearbyviaQuadTree3-DRAFT.R          NA        
# 45 getblocksnearbyviaQuadTree_Clustered-DRAFT R/getblocksnearbyviaQuadTree_Clustered-DRAFT.R NA        
# 46 getblocks_diagnostics                      R/getblocks_diagnostics.R                      NA        
# 47 getfrsnearby                               R/getfrsnearby.R                               NA        
# 48 get_blockpoints_in_shape                   R/get_blockpoints_in_shape.R                   NA        
# 49 latlon_from_mactsubpart                    R/latlon_from_mactsubpart.R                    NA        
# 50 latlon_from_naics                          R/latlon_from_naics.R                          NA        
# 51 latlon_from_program                        R/latlon_from_program.R                        NA        
# 52 latlon_from_programid                      R/latlon_from_programid.R                      NA        
# 53 latlon_from_regid                          R/latlon_from_regid.R                          NA        
# 54 latlon_join_on_blockid                     R/latlon_join_on_blockid.R                     NA        
# 55 mapfast                                    R/mapfast.R                                    NA   

# 56 MAP_FUNCTIONS                              R/MAP_FUNCTIONS.R   [NOW EXISTS]                           NA        

# 57 MODULE_ejscreenapi                         R/MODULE_ejscreenapi.R                         NA        
# 58 MODULE_latlontypedin                       R/MODULE_latlontypedin.R                       NA        
# 59 module_latlontypedin_DEMO                  R/module_latlontypedin_DEMO.R                  NA        
# 60 mod_fips_picker-DRAFT                      R/mod_fips_picker-DRAFT.R                      NA        
# 61 mod_get_shape_from_upload-DRAFT            R/mod_get_shape_from_upload-DRAFT.R            NA        
# 62 mod_save_report-DRAFT                      R/mod_save_report-DRAFT.R                      NA        
# 63 mod_specify_sites-DRAFT                    R/mod_specify_sites-DRAFT.R                    NA        
# 64 mod_view_results-DRAFT                     R/mod_view_results-DRAFT.R                     NA       

# 65 NAICS_FUNCTIONS                            R/NAICS_FUNCTIONS.R                            NA        

# 66 naics_is.valid                             R/naics_is.valid.R                             NA        
# 67 namesbyvarlist                             R/namesbyvarlist.R                             NA        
# 68 pctiles_from_raw_lookup_DRAFT              R/pctiles_from_raw_lookup_DRAFT.R              NA 

# 69 plotblocksnearby                           R/plotblocksnearby.R                           NA        
# 70 plot_barplot_ratios                        R/plot_barplot_ratios.R                        NA        
# 71 plot_demogshare_by_distance                R/plot_demogshare_by_distance.R                NA        
# 72 plot_distance_by_pctd                      R/plot_distance_by_pctd.R                      NA        
# 73 plot_distance_cdf_by_group                 R/plot_distance_cdf_by_group.R                 NA        
# 74 plot_distance_mean_by_group                R/plot_distance_mean_by_group.R                NA        
# 75 plot_lorenz_popcount_by_site - DRAFT       R/plot_lorenz_popcount_by_site - DRAFT.R       NA        
# 76 plot_ridgeline_ratios                      R/plot_ridgeline_ratios.R                      NA        
# 77 plot_vs_us - DRAFT                         R/plot_vs_us - DRAFT.R                         NA        

# 78 popshare_xyz                               R/popshare_xyz.R                               NA        
# 79 popup_from_any                             R/popup_from_any.R                             NA        
# 80     
# 81 popup_from_ejscreen                        R/popup_from_ejscreen.R                        NA        
# 82 popup_from_uploadedpoints                  R/popup_from_uploadedpoints.R                  NA        
# 83 proximity.score.in.miles                   R/proximity.score.in.miles.R                   NA        
# 84 proxistat2-DRAFT                           R/proxistat2-DRAFT.R                           NA        

# 85 sic_categories                             R/sic_categories.R                             NA        
# 86 sic_xyz                                    R/sic_xyz.R                                    NA        

# 87 sitepoints_from_any                        R/sitepoints_from_any.R                        NA        
# 88 statestats_query                           R/statestats_query.R                           NA        
# 89 state_from_sitetable                               R/state_from_sitetable.R                               NA        

# 91 tablefixed                                 R/tablefixed.R                                 NA        
# 92 table_gt_                                  R/table_gt_.R                                  NA        
# 93 table_order_variables                      R/table_order_variables.R                      NA        
# 94 table_round                                R/table_round.R                                NA        
# 95 table_rounding_info                        R/table_rounding_info.R                        NA        
# 96 table_tall_from_overall                    R/table_tall_from_overall.R                    NA        
# 97 table_xls_format                           R/table_xls_format.R                           NA        
# 98 table_xls_from_ejam                        R/table_xls_from_ejam.R                        NA        

# 99 testpoints_n                               R/testpoints_n.R                               NA        
    

# 101 URL_FUNCTIONS_part1                        R/URL_FUNCTIONS_part1.R                        NA        
# 102 URL_FUNCTIONS_part2                        R/URL_FUNCTIONS_part2.R                        NA        


# 103 utils_ask_number                           R/utils_ask_number.R                           NA        
# 104 utils_calc_ejam                            R/utils_calc_ejam.R                            NA        
# 105 utils_check_urls_in_documentation          R/utils_check_urls_in_documentation.R          NA        
# 106 utils_datapack                             R/utils_datapack.R                             NA        
# 107 utils_dupenames                            R/utils_dupenames.R                            NA        
# 108 utils_functions_in_pkg                     R/utils_functions_in_pkg.R                     NA        
# 109 utils_functions_that_use                   R/utils_functions_that_use.R                   NA        
# 110 utils_global_or_param                      R/utils_global_or_param.R                      NA        
# 111 utils_golem_utils_server                   R/utils_golem_utils_server.R                   NA        
# 112 utils_golem_utils_ui                       R/utils_golem_utils_ui.R                       NA        
# 113 utils_indexblocks                          R/utils_indexblocks.R                          NA        
# 114 utils_input_names                          R/utils_input_names.R                          NA        
# 115 utils_linesofcode_                         R/utils_linesofcode_.R                         NA        
# 116 utils_lint_equals                          R/utils_lint_equals.R                          NA        
# 117 utils_metadata_add                         R/utils_metadata_add.R                         NA        
# 118 utils_names_whichlist                      R/utils_names_whichlist.R                      NA        
# 119 utils_nearRcolor                           R/utils_nearRcolor.R                           NA        
# 120 utils_pctiles_lookup_create                R/utils_pctiles_lookup_create.R                NA        
# 121 utils_recursivePackageDependencies         R/utils_recursivePackageDependencies.R         NA        
# 122 utils_rmost                                R/utils_rmost.R                                NA        
# 123 utils_rtips                                R/utils_rtips.R                                NA        
# 124 utils_source_this_codetext                 R/utils_source_this_codetext.R                 NA        
# 125 utils_speedtest                            R/utils_speedtest.R                            NA        
# 126 utils_structure.of.output.list             R/utils_structure.of.output.list.R             NA        

# 127 varin_map_headernames                      R/varin_map_headernames.R                      NA        
# 128 var_is_numeric_ish                         R/var_is_numeric_ish.R                         NA        
# 129 _disable_autoload                          R/_disable_autoload.R                          NA 

################################ # 


# MATCHED EXACTLY -- see all test files that exactly match name of a .R file


tdat[!is.na(tdat$test) & !is.na(tdat$R), c("R", "test")] |> print(n = 500)

# # A tibble: 19 × 2
#               R                              test                                            

# 3 R/FIPS_FUNCTIONS.R             tests/testthat/test-FIPS_FUNCTIONS.R 

# 4 R/frs_is_valid.R               tests/testthat/test-frs_is_valid.R   

# 2 R/ejamit.R                     tests/testthat/test-ejamit.R         
# 1 R/doaggregate.R                tests/testthat/test-doaggregate.R               
# 5 R/getblocksnearby.R            tests/testthat/test-getblocksnearby.R           
# 6 R/getblocksnearbyviaQuadTree.R tests/testthat/test-getblocksnearbyviaQuadTree.R
# 7 R/getblocksnearby_from_fips.R  tests/testthat/test-getblocksnearby_from_fips.R 

#  8 R/latlon_as.numeric.R          tests/testthat/test-latlon_as.numeric.R         
#  9 R/latlon_df_clean.R            tests/testthat/test-latlon_df_clean.R           
# 10 R/latlon_from_address.R        tests/testthat/test-latlon_from_address.R       
# 11 R/latlon_from_anything.R       tests/testthat/test-latlon_from_anything.R      
# 12 R/latlon_infer.R               tests/testthat/test-latlon_infer.R              
# 13 R/latlon_is.valid.R            tests/testthat/test-latlon_is.valid.R           

# 14 R/MAP_FUNCTIONS.R              tests/testthat/test-MAP_FUNCTIONS.R             

# 15 R/pctile_from_raw_lookup.R     tests/testthat/test-pctile_from_raw_lookup.R    

# 16 R/radius_inferred.R            tests/testthat/test-radius_inferred.R           

# 17 R/shapefile_xyz.R              tests/testthat/test-shapefile_xyz.R             

# 18 R/state_from_latlon.R          tests/testthat/test-state_from_latlon.R         

# 19 R/varinfo.R                    tests/testthat/test-varinfo.R                 

}
