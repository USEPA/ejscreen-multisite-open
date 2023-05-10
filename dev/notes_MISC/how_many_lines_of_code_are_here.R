
# HOW MANY LINES OF CODE (AND COMMENTS) ARE IN THESE PACKAGES (on your local drive)
#  IN .R FILES
#  IN ANY FOLDER OR JUST IN THE /R/ FOLDER?

# https://github.com/ejanalysis/analyze.stuff/blob/master/R/linesofcode.R



##               CODE IN THE R FOLDERS



#     x <- analyze.stuff::linesofcode('..', packages = EJAM::ejampackages,  rfolderonly = TRUE)
#
# cat(round(100 * sum(x$comments)/sum(x$lines),1), "% of all lines in R folders are comments not code.\n")
## 23.9 % of all lines in R folders are comments not code.
# cat("One line of commenting for every", round(1/ (sum(x$comments)/sum(x$code)),1), "lines of actual code.\n")
## One line of commenting for every 3.2 lines of actual code!
# colSums(x[,1:3])

##     lines  comments      code 
##    24,322     5,806    18,516 

#                package filename  lines
# 1                 EJAM       84 10,972
# 2      EJAMejscreenapi       52  5,869
# 3 EJAMbatch.summarizer       44  5,599
# 4          EJAMfrsdata       21  1,337
# 5        EJAMblockdata        8    279
# 6     EJAMejscreendata        5    266
# 
# 
# Most of the code is in these files: 
#   
#    lines comments code              package where                           filename
# 1   1791       26 1765                 EJAM   /R/                       app_server.R
# 2   1704        8 1696 EJAMbatch.summarizer   /R/                       app_server.R
# 4   1169       59 1110                 EJAM   /R/                      doaggregate.R
# 5   1161       59 1102                 EJAM   /R/                  doaggregate_old.R
# 6   1084       16 1068 EJAMbatch.summarizer   /R/                           app_ui.R
# 7   1006       18  988                 EJAM   /R/                           app_ui.R
# 10   619       20  599      EJAMejscreenapi   /R/                           app_ui.R
# 11   609        2  607      EJAMejscreenapi   /R/                       app_server.R
# 14   405      170  235                 EJAM   /R/                   golem_utils_ui.R
# 15   405      170  235      EJAMejscreenapi   /R/                   golem_utils_ui.R
# 16   379       27  352 EJAMbatch.summarizer   /R/                  batch.summarize.R
# 18   363       28  335      EJAMejscreenapi   /R/              popup_from_ejscreen.R
# 20   337      145  192      EJAMejscreenapi   /R/                       ejscreenit.R
# 22   331       53  278      EJAMejscreenapi   /R/                      ejscreenapi.R
# 26   281        9  272 EJAMbatch.summarizer   /R/ ascript_for_now_to_get_US_TOTALS.R
# 28   256      167   89      EJAMejscreenapi   /R/                get.distances.all.R
# 31   231       83  148 EJAMbatch.summarizer   /R/                        ustotals2.R
# 32   230      121  109      EJAMejscreenapi   /R/                     locate_by_id.R
# 
# Full list is returned invisibly 
#  
################################################################################################################ #



#               JUST FILES OF NOTES/CREATING DATA/SCRIPTS/ETC. IN .R FILES



# > analyze.stuff::linesofcode('..', packages = EJAM::ejampackages )
# 
#                package filename lines    INCLUDING R FOLDER AND OTHERS

                                         # comments
# 1                 EJAM      150 19835-10972= 8863   55% is code, not comments
# 2      EJAMejscreenapi       84  7849-5869=  1980   75% is code
# 3 EJAMbatch.summarizer       61  7242-5599=  1643   77% is code
# 4          EJAMfrsdata       30  1950-1337=   613
# 5        EJAMblockdata        9   604-279=    325
# 6     EJAMejscreendata        9   446-266=    180
# 
# 
# Most of the code is in these files: 
#   
#    lines comments code              package                   where                                    filename

# 3   1437        2 1435                 EJAM /inst/notes_intersec... NOTES_on_ejscreen_ejquery_mapservices_AP...
# 8    673       50  623                 EJAM                  /inst/                                    global.R
# 9    653       52  601                 EJAM       /inst/notes_MISC/          OBSOLETE_doaggregate_with_states.R
# 12   542        1  541      EJAMejscreenapi                  /inst/                            ui_OLDTEMPLATE.R
# 13   505       11  494                 EJAM                  /inst/                OneEPA_template_2022_11_16.R
# 17   365      100  265                 EJAM              /data-raw/        names_of_indicators_creationscript.R
# 19   361       12  349 EJAMbatch.summarizer                  /inst/                         example.not.shiny.R
# 21   331      255   76                 EJAM /inst/0- NOTES_BREAK...        0- NOTES_BREAK_EJAM_INTO_FUNCTIONS.R
# 23   325      161  164        EJAM                  /inst/datasets/               NOTES_read_blockweights2020.R
# 24   311      134  177                 EJAM /inst/notes_intersec...                NOTES_on_sf_for_shapefiles.R
# 25   310      138  172                 EJAM /inst/notes_datasets...            1_SCRIPT_EJAMejscreen_download.R
# 27   269      149  120 EJAMbatch.summarizer                  /inst/                                    global.R
# 29   246      163   83                 EJAM   /inst/notes_datasets/  NOTES_which_states_are_in_which_datasets.R
# 30   244      137  107          EJAMfrsdata                  /inst/              notes_ACTIVE_VS_INACTIVE_FRS.R
# 33   222       48  174                 EJAM /inst/notes_intersec...   NOTES_PARALLELIZATION_for_distance_calc.R
# 

################################################################################################################ #



#            DETAILS


 
# > sapply(EJAM::ejampackages, FUN = function(ppp) analyze.stuff::linesofcode('..', packages = ppp, rfolderonly = TRUE))
# 
# package filename lines
# 1    EJAM       84 10972
# 
# 
# Most of the code is in these files: 
#   
#   lines comments code package where          filename
# 1  1791       26 1765    EJAM   /R/      app_server.R
# 3  1169       59 1110    EJAM   /R/     doaggregate.R
# 4  1161       59 1102    EJAM   /R/ doaggregate_old.R
# 5  1006       18  988    EJAM   /R/          app_ui.R
# 9   405      170  235    EJAM   /R/  golem_utils_ui.R
# 
# Full list is returned invisibly 
# 
# package filename lines
# 1 EJAMbatch.summarizer       44  5599
# 
# 
# Most of the code is in these files: 
#   
#   lines comments code              package where          filename
# 1  1704        8 1696 EJAMbatch.summarizer   /R/      app_server.R
# 2  1084       16 1068 EJAMbatch.summarizer   /R/          app_ui.R
# 3   379       27  352 EJAMbatch.summarizer   /R/ batch.summarize.R
# 
# Full list is returned invisibly 
# 
# package filename lines
# 1 EJAMblockdata        8   279
# 
# 
# Most of the code is in these files: 
#   
#   lines comments code       package where            filename
# 2    77       75    2 EJAMblockdata   /R/ blockdata-package.R
# 3    46       45    1 EJAMblockdata   /R/          blockwts.R
# 4    45       44    1 EJAMblockdata   /R/          quaddata.R
# 
# Full list is returned invisibly 
# 
# package filename lines
# 1 EJAMejscreenapi       52  5869
# 
# 
# Most of the code is in these files: 
#   
#   lines comments code         package where              filename
# 1   619       20  599 EJAMejscreenapi   /R/              app_ui.R
# 2   609        2  607 EJAMejscreenapi   /R/          app_server.R
# 4   405      170  235 EJAMejscreenapi   /R/      golem_utils_ui.R
# 5   363       28  335 EJAMejscreenapi   /R/ popup_from_ejscreen.R
# 6   337      145  192 EJAMejscreenapi   /R/          ejscreenit.R
# 7   331       53  278 EJAMejscreenapi   /R/         ejscreenapi.R
# 8   256      167   89 EJAMejscreenapi   /R/   get.distances.all.R
# 9   230      121  109 EJAMejscreenapi   /R/        locate_by_id.R
# 
# Full list is returned invisibly 
# 
# package filename lines
# 1 EJAMejscreendata        5   266
# 
# 
# Most of the code is in these files: 
#   
#   lines comments code          package where                               filename
# 3    64       63    1 EJAMejscreendata   /R/                          States_2022.R
# 4    61       60    1 EJAMejscreendata   /R/     EJSCREEN_Full_with_AS_CNMI_GU_VI.R
# 5    58       57    1 EJAMejscreendata   /R/ EJSCREEN_StatePct_with_AS_CNMI_GU_VI.R
# 
# Full list is returned invisibly 
# 
# package filename lines
# 1 EJAMfrsdata       21  1337
# 
# 
# Most of the code is in these files: 
#   
#   lines comments code     package where                    filename
# 2   212      138   74 EJAMfrsdata   /R/                   frs_get.R
# 3   161       89   72 EJAMfrsdata   /R/            naics_from_any.R (moved to EJAM::)
# 4   139       19  120 EJAMfrsdata   /R/       frs_update_datasets.R
# 5   116       40   76 EJAMfrsdata   /R/                  frs_read.R
# 8    80        7   73 EJAMfrsdata   /R/ frs_make_programid_lookup.R
# 
# Full list is returned invisibly 
# EJAM         EJAMbatch.summarizer EJAMblockdata EJAMejscreenapi EJAMejscreendata EJAMfrsdata 
# lines    integer,84   integer,44           integer,8     integer,52      integer,5        integer,21  
# comments integer,84   integer,44           integer,8     integer,52      integer,5        integer,21  
# code     integer,84   integer,44           integer,8     integer,52      integer,5        integer,21  
# package  character,84 character,44         character,8   character,52    character,5      character,21
# where    character,84 character,44         character,8   character,52    character,5      character,21
# filename character,84 character,44         character,8   character,52    character,5      character,21
# > 
