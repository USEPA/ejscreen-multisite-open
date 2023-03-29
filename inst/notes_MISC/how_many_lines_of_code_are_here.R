
# HOW MANY LINES OF CODE (AND COMMENTS) ARE IN THESE PACKAGES (on your local drive)
#  IN .R FILES
#  IN ANY FOLDER OR JUST IN THE /R/ FOLDER?

# https://github.com/ejanalysis/analyze.stuff/blob/master/R/linesofcode.R


#  analyze.stuff::linesofcode('..', packages = EJAM::ejampackages )
## Including root and inst folders, etc.:
#               package filecount lines
# 1                 EJAM      115 12044
# 2 EJAMbatch.summarizer       61  7137
# 3      EJAMejscreenapi       70  6415
# 4          EJAMfrsdata       20  1213
# 5        EJAMblockdata        9   570
# 6     EJAMejscreendata        9   435



# >     x <-      linesofcode('..', packages = EJAM::ejampackages,  rfolderonly = TRUE)
#
#               package filecount lines
# 1 EJAMbatch.summarizer       45  5579
# 2                 EJAM       58  4890
# 3      EJAMejscreenapi       46  3499
# 4          EJAMfrsdata       16   863
# 5        EJAMblockdata        8   268
# 6     EJAMejscreendata        5   257
#
# Most of the code is in these files:
#
#   lines comments code              package where                           filename
# 1   1691        8 1683 EJAMbatch.summarizer   /R/                       app_server.R
# 3   1084       16 1068 EJAMbatch.summarizer   /R/                           app_ui.R
# 4    767       15  752                 EJAM   /R/                           app_ui.R
# 6    575       53  522                 EJAM   /R/                      doaggregate.R
# 9    541        8  533                 EJAM   /R/                       app_server.R
# 12   405      170  235                 EJAM   /R/                   golem_utils_ui.R
# 13   380       27  353 EJAMbatch.summarizer   /R/                  batch.summarize.R
# 15   328       27  301      EJAMejscreenapi   /R/                  make.popups.api.R renamed EJAMejscreenapi::popup_from_ejscreen
# 16   325       47  278      EJAMejscreenapi   /R/                      ejscreenapi.R
# 19   281        9  272 EJAMbatch.summarizer   /R/ ascript_for_now_to_get_US_TOTALS.R
# 22   256       96  160      EJAMejscreenapi   /R/               ejscreenapi_script.R
# 26   231       83  148 EJAMbatch.summarizer   /R/                        ustotals2.R
# 27   228      119  109      EJAMejscreenapi   /R/                     locate_by_id.R
# 29   221      133   88      EJAMejscreenapi   /R/                get.distances.all.R
# 30   216       52  164      EJAMejscreenapi   /R/       get_facility_info_via_ECHO.R
# 33   207       49  158                 EJAM   /R/                  url_getacs_epaquery.R

#
# Most of the code is in these files:
#
#    lines comments code              package                   where                                    filename
# 1   1691        8 1683 EJAMbatch.summarizer                     /R/                                app_server.R

# 3   1084       16 1068 EJAMbatch.summarizer                     /R/                                    app_ui.R
# 4    767       15  752                 EJAM                     /R/                                    app_ui.R
# 5    580        2  578      EJAMejscreenapi                       /   ***                                 server.R
# 6    575       53  522                 EJAM                     /R/                               doaggregate.R
# 7    575        7  568      EJAMejscreenapi                       /    ***                                    ui.R

# 9    541        8  533                 EJAM                     /R/                                app_server.R

# 12   405      170  235                 EJAM                     /R/                            golem_utils_ui.R
# 13   380       27  353 EJAMbatch.summarizer                     /R/                           batch.summarize.R

# 15   328       27  301      EJAMejscreenapi                     /R/                           make.popups.api.R renamed EJAMejscreenapi::popup_from_ejscreen
# 16   325       47  278      EJAMejscreenapi                     /R/                               ejscreenapi.R

# 18   281        9  272 EJAMbatch.summarizer                     /R/          ascript_for_now_to_get_US_TOTALS.R
# 19   277      148  129      EJAMejscreenapi              /data-raw/  ***                varsinfo_ECHO_API_script.R
# 20   269      149  120 EJAMbatch.summarizer                  /inst/  ***                                  global.R
# 21   256       96  160      EJAMejscreenapi                     /R/                        ejscreenapi_script.R

# 25   231       83  148 EJAMbatch.summarizer                     /R/                                 ustotals2.R
# 26   228      119  109      EJAMejscreenapi                     /R/                              locate_by_id.R

# 28   221      133   88      EJAMejscreenapi                     /R/                         get.distances.all.R
# 29   216       52  164      EJAMejscreenapi                     /R/                get_facility_info_via_ECHO.R
#
# Full list is returned invisibly





#
# > sapply(EJAM::ejampackages, FUN = function(ppp) linesofcode('..',packages = ppp, rfolderonly = TRUE))
#
#   package filecount lines
# 1    EJAM       58  4890
# Most of the code is in these files:
#
#   lines comments code package where          filename
# 2    767       15  752    EJAM   /R/          app_ui.R
# 3    575       53  522    EJAM   /R/     doaggregate.R
# 4    541        8  533    EJAM   /R/      app_server.R
# 7    405      170  235    EJAM   /R/  golem_utils_ui.R
# 14   207       49  158    EJAM   /R/ url_getacs_epaquery.R



#                package filecount lines
# 1 EJAMbatch.summarizer       45  5579
# Most of the code is in these files:
#
#   lines comments code              package where          filename
# 1  1691        8 1683 EJAMbatch.summarizer   /R/      app_server.R
# 2  1084       16 1068 EJAMbatch.summarizer   /R/          app_ui.R
# 3   380       27  353 EJAMbatch.summarizer   /R/ batch.summarize.R


#         package filecount lines
# 1 EJAMblockdata        8   268
# Most of the code is in these files:
#
#   lines comments code       package where            filename
# 2    77       75    2 EJAMblockdata   /R/ blockdata-package.R
# 3    46       45    1 EJAMblockdata   /R/          blockwts.R
# 4    43       42    1 EJAMblockdata   /R/          quaddata.R


#           package filecount lines
# 1 EJAMejscreenapi       46  3499
# Most of the code is in these files:
#
#   lines comments code         package where                     filename
# 4    328       27  301 EJAMejscreenapi   /R/            make.popups.api.R renamed EJAMejscreenapi::popup_from_ejscreen
# 5    325       47  278 EJAMejscreenapi   /R/                ejscreenapi.R
# 7    256       96  160 EJAMejscreenapi   /R/         ejscreenapi_script.R
# 8    228      119  109 EJAMejscreenapi   /R/               locate_by_id.R
# 9    221      133   88 EJAMejscreenapi   /R/          get.distances.all.R
# 10   216       52  164 EJAMejscreenapi   /R/ get_facility_info_via_ECHO.R
# 12   144       34  110 EJAMejscreenapi   /R/               ejscreenapi1.R
# 13   144       32  112 EJAMejscreenapi   /R/              varname2color.R


#            package filecount lines
# 1 EJAMejscreendata        5   257
# Most of the code is in these files:
#
#   lines comments code          package where                               filename
# 3    64       63    1 EJAMejscreendata   /R/                          States_2022.R
# 4    58       57    1 EJAMejscreendata   /R/ EJSCREEN_StatePct_with_AS_CNMI_GU_VI.R
# 5    52       51    1 EJAMejscreendata   /R/     EJSCREEN_Full_with_AS_CNMI_GU_VI.R


#       package filecount lines
# 1 EJAMfrsdata       16   863
# Most of the code is in these files:
#
#   lines comments code     package where                    filename
# 2   185      128   57 EJAMfrsdata   /R/                   frs_get.R
# 3   116       40   76 EJAMfrsdata   /R/                  frs_read.R
# 4    80        7   73 EJAMfrsdata   /R/ frs_make_programid_lookup.R
# 5    77        9   68 EJAMfrsdata   /R/     frs_make_naics_lookup.R
#
# Full list is returned invisibly
