
# HOW MANY LINES OF CODE (AND COMMENTS) ARE IN THESE PACKAGES (on your local drive)
#  IN .R FILES
#  IN ANY FOLDER OR JUST IN THE /R/ FOLDER?

##               CODE IN THE R FOLDERS  9/2023

##   23 % of all lines in R folders are comments not code.
## One line of commenting for every 3.5 lines of actual code!

##     lines  comments      code 
#      26,942      6,052     20,890 

#                package filename lines
# 1                 EJAM      125 14,676
# 2      EJAMejscreenapi       56  6,737
# 3 EJAMbatch.summarizer       42  5,529
# 
# 
# Most of the code is in these files: 
#   
#    lines comments code              package where                           filename

# 1   2616       39 2577                 EJAM   /R/                       app_server.R *** <<<<<<<<<<<<<<<<< Can break up to modules and functions

# 2   1702        8 1694 EJAMbatch.summarizer   /R/                       app_server.R ** NOT USED BY EJAM. avoid importing / loading with EJAM? JUST import KEY FUNCTIONS?

# 3   1459       86 1373                 EJAM   /R/                      doaggregate.R *** <<<<<<<<<<<<<<<<< Can break up to modules and functions

# 5   1169       19 1150                 EJAM   /R/                           app_ui.R 
# 6   1084       16 1068 EJAMbatch.summarizer   /R/                           app_ui.R ** NOT USED BY EJAM. If do not need standalone summarizer, REMOVE ui AND JUST USE pkg for its KEY FUNCTIONS?

# 9    719        2  717      EJAMejscreenapi   /R/                       app_server.R ** NOT USED BY EJAM. avoid importing / loading with EJAM? JUST import KEY FUNCTIONS?
# 10   623       20  603      EJAMejscreenapi   /R/                           app_ui.R ** NOT USED BY EJAM. avoid importing / loading with EJAM? 

# 13   465       70  395      EJAMejscreenapi   /R/                      ejscreenapi.R *** *** possibly break up

# 15   408      170  238                 EJAM   /R/                   golem_utils_ui.R
# 16   405      170  235      EJAMejscreenapi   /R/                   golem_utils_ui.R
# 17   390      102  288                 EJAM   /R/                  utils_speedtest.R
# 18   379       27  352 EJAMbatch.summarizer   /R/                  batch.summarize.R *** possibly break up
# 19   366       59  307                 EJAM   /R/                  table_xls_format.R *** possibly break up
# 22   353       29  324      EJAMejscreenapi   /R/              popup_from_ejscreen.R *** possibly break up
# 23   333      155  178      EJAMejscreenapi   /R/                       ejscreenit.R

# 26   291      164  127      EJAMejscreenapi   /R/                    URL_FUNCTIONS.R
# 28   283       84  199      EJAMejscreenapi   /R/               ejscreenRESTbroker.R
# 30   260       20  240                 EJAM   /R/                  mod_ejscreenapi.R
# 31   260        9  251 EJAMbatch.summarizer   /R/ ascript_for_now_to_get_US_TOTALS.R

# 32    256      167   89 EJAMejscreenapi   /R/             get.distances.all.R

# 34    239       57  182    EJAM   /R/                pctile_from_raw_lookup.R ***
# 35    231       83  148 EJAMbatch.summarizer   /R/                ustotals2.R

# Full list is returned invisibly 
#  
################################################################################################################ #





################################################################################################################ #
################################################################################################################ #
################################################################################################################ #

# more details:

############################################################## #
# Those counts were done using the functions here:
#
# https://github.com/ejanalysis/analyze.stuff/blob/master/R/linesofcode.R
#     x <- analyze.stuff  coloncolon  linesofcode('..', packages = EJAM::ejampackages,  rfolderonly = TRUE)
# cat(round(100 * sum(x$comments)/sum(x$lines),1), "% of all lines in R folders are comments not code.\n")
##   23 % of all lines in R folders are comments not code.
# cat("One line of commenting for every", round(1/ (sum(x$comments)/sum(x$code)),1), "lines of actual code.\n")
## One line of commenting for every 3.5 lines of actual code!
# colSums(x[,1:3])
##     lines  comments      code 
#      26,942      6,052     20,890 
############################################################## #



# > x[x$package == "EJAMbatch.summarizer", ]

#     lines comments code              package where                                  filename
# 2    1702        8 1694 EJAMbatch.summarizer   /R/                              app_server.R ***
# 6    1084       16 1068 EJAMbatch.summarizer   /R/                                  app_ui.R 

# 18    379       27  352 EJAMbatch.summarizer   /R/                         batch.summarize.R ***

# 31    260        9  251 EJAMbatch.summarizer   /R/        ascript_for_now_to_get_US_TOTALS.R
# 35    231       83  148 EJAMbatch.summarizer   /R/                               ustotals2.R
# 65    160        0  160 EJAMbatch.summarizer   /R/                             count.above.R
# 72    147        7  140 EJAMbatch.summarizer   /R/                            tabs.compile.R
# 84    133       11  122 EJAMbatch.summarizer   /R/                       change.fieldnames.R
# 88    128       28  100 EJAMbatch.summarizer   /R/                           tabs.reformat.R
# 98    115        2  113 EJAMbatch.summarizer   /R/                               pct.above.R
# 108   105       18   87 EJAMbatch.summarizer   /R/                              tab.parsed.R
# 112   102       25   77 EJAMbatch.summarizer   /R/                  mod_plot_pctile_pctile.R

# 124    91       16   75 EJAMbatch.summarizer   /R/                             batch.clean.R
# 172    60       25   35 EJAMbatch.summarizer   /R/                             lead.zeroes.R
# 181    54       19   35 EJAMbatch.summarizer   /R/                              maphelpers.R
# 188    52        2   50 EJAMbatch.summarizer   /R/                       plotlyGraphWidget.R
# 194    49        6   43 EJAMbatch.summarizer   /R/                                 rowMaxs.R
# 195    49        6   43 EJAMbatch.summarizer   /R/                                 rowMins.R
# 209    44       19   25 EJAMbatch.summarizer   /R/                              app_config.R
# 210    44        0   44 EJAMbatch.summarizer   /R/                                 colMins.R
# 218    42       11   31 EJAMbatch.summarizer   /R/                    read_csv_or_xl_batch.R
# 237    38        0   38 EJAMbatch.summarizer   /R/                                 colMaxs.R
# 248    36       11   25 EJAMbatch.summarizer   /R/                            tab.to.batch.R
# 250    35       20   15 EJAMbatch.summarizer   /R/                                batchall.R
# 251    35        0   35 EJAMbatch.summarizer   /R/ find_nearby_using_geosphere_then_hclust.R
# 256    34       27    7 EJAMbatch.summarizer   /R/         prep.ejscreenapi.for.summarizer.R
# 259    33        9   24 EJAMbatch.summarizer   /R/                           makenumericdf.R
# 260    33       26    7 EJAMbatch.summarizer   /R/ prep.ejscreenapi.for.summarizerFORSHINY.R
# 261    33       17   16 EJAMbatch.summarizer   /R/                          ustotals2print.R
# 262    33       18   15 EJAMbatch.summarizer   /R/                        wilcoxon.pvalues.r
# 270    31       14   17 EJAMbatch.summarizer   /R/                                 flagged.R
# 271    31       20   11 EJAMbatch.summarizer   /R/                            wtd.colMeans.R
# 289    23       13   10 EJAMbatch.summarizer   /R/                      plot_pctile_pctile.R
# 296    20        0   20 EJAMbatch.summarizer   /R/                        cols.above.count.R
# 297    20       14    6 EJAMbatch.summarizer   /R/                        read_batch_clean.R
# 312    15       14    1 EJAMbatch.summarizer   /R/                         names_all_batch.R
# 327    11        0   11 EJAMbatch.summarizer   /R/                     fix.statename.field.R
# 330    10        9    1 EJAMbatch.summarizer   /R/                          varnamesinfo22.R
# 335     9        8    1 EJAMbatch.summarizer   /R/                       _disable_autoload.R
# 343     7        7    0 EJAMbatch.summarizer   /R/                             fct_helpers.R
# 352     6        1    5 EJAMbatch.summarizer   /R/                              batch.read.R
# 360     5        0    5 EJAMbatch.summarizer   /R/                         fix.miles.field.R


# > x[x$package == "EJAMejscreenapi", ]

#     lines comments code         package where                          filename
# 9     719        2  717 EJAMejscreenapi   /R/                      app_server.R ***
# 10    623       20  603 EJAMejscreenapi   /R/                          app_ui.R
# 13    465       70  395 EJAMejscreenapi   /R/                     ejscreenapi.R ***
# 16    405      170  235 EJAMejscreenapi   /R/                  golem_utils_ui.R
# 22    353       29  324 EJAMejscreenapi   /R/             popup_from_ejscreen.R ***
# 23    333      155  178 EJAMejscreenapi   /R/                      ejscreenit.R ***

# 26    291      164  127 EJAMejscreenapi   /R/                   URL_FUNCTIONS.R
# 28    283       84  199 EJAMejscreenapi   /R/              ejscreenRESTbroker.R
# 32    256      167   89 EJAMejscreenapi   /R/               get.distances.all.R
# 37    230      121  109 EJAMejscreenapi   /R/                    locate_by_id.R
# 44    213       60  153 EJAMejscreenapi   /R/                ejscreenapi_plus.R
# 45    209       45  164 EJAMejscreenapi   /R/      get_facility_info_via_ECHO.R
# 69    153       34  119 EJAMejscreenapi   /R/           read_and_clean_points.R
# 70    152       15  137 EJAMejscreenapi   /R/        ejscreenRESTbroker2table.R
# 79    141       54   87 EJAMejscreenapi   /R/                    ejscreenapi1.R
# 94    118       60   58 EJAMejscreenapi   /R/                 boxplots_ratios.R
# 99    115       40   75 EJAMejscreenapi   /R/                   convert_units.R
# 100   115       16   99 EJAMejscreenapi   /R/              table_xls_format_api.R

# 120    96       37   59 EJAMejscreenapi   /R/                     fixcolnames.R
# 126    89       51   38 EJAMejscreenapi   /R/        latlon_from_anything.api.R
# 127    89       32   57 EJAMejscreenapi   /R/               xls_varname2color.R
# 128    88       31   57 EJAMejscreenapi   /R/                   ratios_to_avg.R
# 139    78       41   37 EJAMejscreenapi   /R/                    latlon2nexus.R
# 149    74       15   59 EJAMejscreenapi   /R/     urls_clusters_and_sort_cols.R
# 155    69       43   26 EJAMejscreenapi   /R/                         run_app.R
# 159    68       21   47 EJAMejscreenapi   /R/                    latlon_infer.R
# 168    63       35   28 EJAMejscreenapi   /R/              golem_utils_server.R
# 175    57       21   36 EJAMejscreenapi   /R/ echo_colids_from_num_name_group.R
# 176    57       18   39 EJAMejscreenapi   /R/                     speedreport.R
# 186    53       18   35 EJAMejscreenapi   /R/                        fixnames.R
# 196    49       27   22 EJAMejscreenapi   /R/                         mapfast.R
# 199    48       22   26 EJAMejscreenapi   /R/                  namesbyvarlist.R
# 203    46       29   17 EJAMejscreenapi   /R/                   popup_from_df.R
# 208    45       21   24 EJAMejscreenapi   /R/                 latlon_is.valid.R
# 212    44       19   25 EJAMejscreenapi   /R/                      app_config.R
# 216    43       12   31 EJAMejscreenapi   /R/                  read_csv_or_xl.R
# 224    41       11   30 EJAMejscreenapi   /R/              fips_lead_zero.api.R
# 225    41       22   19 EJAMejscreenapi   /R/                fixnames_to_type.R
# 226    41        8   33 EJAMejscreenapi   /R/       popup_from_uploadedpoints.R
# 243    37       25   12 EJAMejscreenapi   /R/               latlon_as.numeric.R
# 244    37       13   24 EJAMejscreenapi   /R/                 latlon_df_clean.R
# 264    33       32    1 EJAMejscreenapi   /R/               varsinfo_ECHO_API.R
# 266    32        6   26 EJAMejscreenapi   /R/           makenumericdfFORSHINY.R
# 276    29       19   10 EJAMejscreenapi   /R/                  near_eachother.R
# 290    23       14    9 EJAMejscreenapi   /R/               expand.gridMatrix.R
# 298    20       13    7 EJAMejscreenapi   /R/            ejscreenit_see_table.R
# 328    11       10    1 EJAMejscreenapi   /R/                 map_headernames.R
# 331    10        9    1 EJAMejscreenapi   /R/                   testpoints_50.R
# 336     9        8    1 EJAMejscreenapi   /R/  testoutput_ejscreenapi_plus_50.R
# 339     8        7    1 EJAMejscreenapi   /R/          testids_program_sys_id.R
# 340     8        7    1 EJAMejscreenapi   /R/             testids_registry_id.R
# 345     7        7    0 EJAMejscreenapi   /R/                     fct_helpers.R
# 346     7        7    0 EJAMejscreenapi   /R/                   utils_helpers.R
# 361     5        4    1 EJAMejscreenapi   /R/ default_points_shown_at_startup.R
# 362     5        4    1 EJAMejscreenapi   /R/                 meters_per_mile.R
# 372     3        2    1 EJAMejscreenapi   /R/               _disable_autoload.R
# 
# 


# > x[x$package == "EJAM", ]

#     lines comments code package where                                                               filename
# 1    2616       39 2577    EJAM   /R/                                                           app_server.R ***
# 3    1459       86 1373    EJAM   /R/                                                          doaggregate.R ***
# 5    1169       19 1150    EJAM   /R/                                                               app_ui.R

# 15    408      170  238    EJAM   /R/                                                       golem_utils_ui.R
# 17    390      102  288    EJAM   /R/                                                      utils_speedtest.R
# 19    366       59  307    EJAM   /R/                                                      table_xls_format.R ***

# 30    260       20  240    EJAM   /R/                                                      mod_ejscreenapi.R
# 34    239       57  182    EJAM   /R/                                               pctile_from_raw_lookup.R ***
# 38    228       65  163    EJAM   /R/                                                               ejamit.R
# 46    207       49  158    EJAM   /R/                                                  url_getacs_epaquery.R
# 50    184       48  136    EJAM   /R/                                           plot_distance_cdf_by_group.R
# 51    184       61  123    EJAM   /R/                                                         url_bookmark.R
# 52    183       92   91    EJAM   /R/                                                         sic_xyz.R
# 53    177       10  167    EJAM   /R/                                                      table_gt_format.R
# 56    176      130   46    EJAM   /R/                                                   colcounter_summary.R
# 57    174       40  134    EJAM   /R/                                                         testpoints_n.R
# 59    165       42  123    EJAM   /R/                                    getblocksnearbyviaQuadTree3-DRAFT.R
# 60    164       17  147    EJAM   /R/                                 getblocksnearbyviaQuadTree_Clustered.R
# 61    164       41  123    EJAM   /R/                                    getblocksnearbyviaQuadTree2-DRAFT.R
# 62    163       59  104    EJAM   /R/                                                    dataload_from_aws.R
# 63    162       42  120    EJAM   /R/                                           getblocksnearbyviaQuadTree.R
# 64    161       89   72    EJAM   /R/                                                       naics_from_any.R
# 68    154       31  123    EJAM   /R/                                                           proxistat2.R
# 74    144       42  102    EJAM   /R/                                                   utils_metadata_add.R
# 75    143       32  111    EJAM   /R/                                             get_blockpoints_in_shape.R
# 76    143       69   74    EJAM   /R/                                                     statestats_query.R
# 78    141       96   45    EJAM   /R/                                                    state_from_latlon.R
# 80    140        5  135    EJAM   /R/                                                         aaa_onAttach.R
# 81    140       27  113    EJAM   /R/                                                      utils_dupenames.R
# 86    129        0  129    EJAM   /R/                                           naics_from_federalregister.R
# 90    126       62   64    EJAM   /R/                                          utils_pctiles_lookup_create.R
# 91    122       18  104    EJAM   /R/                                                       utils_datapack.R
# 96    116       38   78    EJAM   /R/                                        pctiles_from_raw_lookup_DRAFT.R
# 102   113       31   82    EJAM   /R/                                                getblocks_diagnostics.R
# 109   104       50   54    EJAM   /R/                                          plot_distance_mean_by_group.R
# 110   103       70   33    EJAM   /R/                                                      frs_from_   renamed to frs_from_regid
# 114   100       31   69    EJAM   /R/                                                      getblocksnearby.R

# 115    99       37   62    EJAM   /R/                                                       popup_from_any.R
# 116    98       11   87    EJAM   /R/                                                       naics_download.R
# 118    97       41   56    EJAM   /R/                                                utils_names_whichlist.R
# 122    93        0   93    EJAM   /R/        url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7.R
# 130    86       47   39    EJAM   /R/                                                   fips_counties_from.R
# 136    83       49   34    EJAM   /R/                                                 latlon_from_anything.R
# 138    80       16   64    EJAM   /R/                                                       table_xls_format.R
# 140    77       76    1    EJAM   /R/                                                       data_stateinfo.R
# 141    77        8   69    EJAM   /R/                                                         states_infer.R
# 144    75       74    1    EJAM   /R/                                                           data_bgpts.R
# 145    75       10   65    EJAM   /R/                                                       map_facilities.R
# 146    75       11   64    EJAM   /R/                                                      popweightedsums.R
# 150    70        0   70    EJAM   /R/                                       count_sites_with_n_high_scores.R
# 151    70       36   34    EJAM   /R/                                                  latlon_from_program.R
# 152    69       12   57    EJAM   /R/                                                  ejscreenit_for_ejam.R
# 153    69       12   57    EJAM   /R/                                                 map_facilities_proxy.R
# 157    68       21   47    EJAM   /R/                                                         latlon_infer.R
# 158    68       12   56    EJAM   /R/                                                              run_app.R
# 162    64       31   33    EJAM   /R/                                                           colcounter.R
# 163    64       17   47    EJAM   /R/                                               getblocksnearby2-DRAFT.R
# 165    63       35   28    EJAM   /R/                                                   golem_utils_server.R
# 166    63       12   51    EJAM   /R/                                                           url_4table.R
# 169    62       61    1    EJAM   /R/                                                    data_frs_by_naics.R
# 170    62        0   62    EJAM   /R/ url_to_get_nearby_blocks_rest_services_ejscreen_ejquery_MapServer_71.R
# 173    59       41   18    EJAM   /R/                                                        data_testdata.R
# 177    56       29   27    EJAM   /R/                                                  fips_bg_from_anyfips.R
# 190    50       11   39    EJAM   /R/                                                     plotblocksnearby.R
# 193    49       40    9    EJAM   /R/                                                     naics_categories.R
# 202    46       16   30    EJAM   /R/                                            mod_get_shape_upload.R
# 204    45       19   26    EJAM   /R/                                                           app_config.R
# 205    45       44    1    EJAM   /R/                                                             data_frs.R
# 213    43       42    1    EJAM   /R/                                                     data_frs_by_mact.R
# 214    43       28   15    EJAM   /R/                                                       naics2children.R
# 217    42        0   42    EJAM   /R/                            url_to_any_rest_services_ejscreen_ejquery.R
# 222    41       11   30    EJAM   /R/                                                       fips_lead_zero.R
# 227    40        7   33    EJAM   /R/                                                         frs_is_valid.R
# 228    40       18   22    EJAM   /R/                                                      latlon_is.valid.R
# 233    38       12   26    EJAM   /R/                                                      latlon_df_clean.R
# 240    37       36    1    EJAM   /R/                                                 data_frsprogramcodes.R
# 241    37       25   12    EJAM   /R/                                                    latlon_as.numeric.R
# 242    37       25   12    EJAM   /R/                                                    latlon_from_naics.R
# 246    36       13   23    EJAM   /R/                                            getblocksnearby_from_fips.R
# 247    36       22   14    EJAM   /R/                                                           tablefixed.R
# 249    35       11   24    EJAM   /R/                                                    utils_indexblocks.R
# 257    33       13   20    EJAM   /R/                                              latlon_from_mactsubpart.R
# 265    32       20   12    EJAM   /R/                                                  naics_findwebscrape.R
# 267    31       16   15    EJAM   /R/                                                      mod_save_report.R
# 268    31       16   15    EJAM   /R/                                                    mod_specify_sites.R
# 269    31       16   15    EJAM   /R/                                                     mod_view_results.R
# 272    30        9   21    EJAM   /R/                                               table_tall_from_overall.R
# 275    29       28    1    EJAM   /R/                                                data_frs_by_programid.R
# 277    28        7   21    EJAM   /R/                                             proximity.score.in.miles.R
# 279    27       17   10    EJAM   /R/                                                    counties_as_sites.R
# 280    27       22    5    EJAM   /R/                                                latlon_from_programid.R
# 282    26       20    6    EJAM   /R/                                         ST_by_site_from_sites2blocks.R
# 287    24       23    1    EJAM   /R/                                                 data_blockgroupstats.R
# 291    22       15    7    EJAM   /R/                                                   latlon_from_regid.R
# 292    22        0   22    EJAM   /R/                                     util_check_urls_in_documentation.R
# 293    20       13    7    EJAM   /R/                             shape_buffered_from_shapefile_points.R
# 294    20        7   13    EJAM   /R/                                                      url_get_via_url.R
# 301    18        9    9    EJAM   /R/                                                        url_naics.com.R
# 303    17       14    3    EJAM   /R/                                         distance_via_surfacedistance.R
# 307    16        6   10    EJAM   /R/                                                     util_input_names.R
# 309    15       14    1    EJAM   /R/                                                        data_quaddata.R
# 310    15        6    9    EJAM   /R/                                                     naics_validation.R
# 315    14        8    6    EJAM   /R/                                        shapefile_from_sitepoints.R
# 316    13       12    1    EJAM   /R/                                                data_states_shapefile.R
# 317    13       12    1    EJAM   /R/                                                      data_statestats.R
# 321    12       11    1    EJAM   /R/                                                            data_bgej.R
# 322    12       11    1    EJAM   /R/                                                        data_usastats.R
# 323    12        8    4    EJAM   /R/                                                    naics_url_of_code.R
# 329    10        9    1    EJAM   /R/                                                     data_regionstats.R
# 332     9        8    1    EJAM   /R/                                                      data_naicstable.R
# 333     9        8    1    EJAM   /R/                                                        data_sictable.R
# 334     9        6    3    EJAM   /R/                                                       sic_categories.R
# 338     8        7    1    EJAM   /R/                                                      data_mact_table.R
# 341     7        6    1    EJAM   /R/                                                   data_bg_cenpop2020.R
# 342     7        6    1    EJAM   /R/                                                      data_frs_by_sic.R
# 349     6        5    1    EJAM   /R/                                                           data_NAICS.R
# 350     6        5    1    EJAM   /R/                                                             data_SIC.R
# 355     5        4    1    EJAM   /R/                                                       data_avg.in.us.R
# 356     5        4    1    EJAM   /R/                                                       data_lat_alias.R
# 357     5        4    1    EJAM   /R/                                                         data_names_d.R
# 358     5        4    1    EJAM   /R/                                                         data_names_e.R
# 359     5        4    1    EJAM   /R/                                                           data_namez.R
# 363     4        3    1    EJAM   /R/                                                    data_ejampackages.R
# 364     4        3    1    EJAM   /R/                                      data_high_pctiles_tied_with_min.R
# 367     3        2    1    EJAM   /R/                                                    _disable_autoload.R
  
