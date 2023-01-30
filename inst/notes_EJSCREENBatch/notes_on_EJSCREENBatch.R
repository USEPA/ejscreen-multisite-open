library(magrittr)
library(dplyr)
library(devtools)
if (!require("EJSCREENbatch")) {
  devtools::install_github('USEPA/EJSCREENbatch', build_vignettes=TRUE)
  library(EJSCREENbatch)
}

##################################################### #
# excerpts of EJSCREENBatch code.... ####
#  annotated with notes relevant to EJAM and for comparison 
##################################################### #

#  took weighted mean of data within each buffer (shape_ID) for these indicators:
# 
# Extract key variables, 
# take ***pop-weighted*** average ####
df.var.wm <- list_data %>%
  as.data.frame() %>%
  dplyr::select(shape_ID, ACSTOTPOP, PM25, OZONE, DSLPM, CANCER,
                RESP, PTRAF, PNPL, PRMP, PRE1960PCT, PTSDF, PWDIS,
                VULEOPCT, MINORPCT, LOWINCPCT, UNDER5PCT,LESSHSPCT,
                OVER64PCT, LINGISOPCT, med_inc, frac_white, frac_black,
                frac_amerind, frac_asian, frac_pacisl, frac_hisp,
                frac_pov50, frac_pov99) %>%
  dplyr::group_by(shape_ID) %>%
  dplyr::summarize(across(PM25:frac_pov99, ~stats::weighted.mean(., w = ACSTOTPOP, na.rm = T)))
#
# 
# # ...
# 
#   EJSCREENBatch code uses ecdf function here on the entire ejscreen dataset each time?? 
#  ecdf() to estimate US, STATE percentiles ####
# 
# #Rejoin ####
df.var.wm <- df.var.wm %>%
  dplyr::left_join(df.var.state, by = 'shape_ID') %>%
  # Calc nat'l percentiles ####
  dplyr::mutate(across(PM25:LINGISOPCT,
                       list(~round(ecdf(ejscreen_data %>%
                                          as.data.frame() %>%
                                          dplyr::select(cur_column()) %>%
                                          unlist() %>%
                                          as.numeric())(.)*100
                                   ,0)),
                       # list(~ntile(., 100)),
                       .names="P_{.col}_US")) %>%
  dplyr::mutate(across(med_inc:frac_pov99,
                       list(~round(ecdf(acs_data %>%
                                          as.data.frame() %>%
                                          dplyr::select(cur_column()) %>%
                                          unlist() %>%
                                          as.numeric())(.)*100
                                   ,0)),
                       .names="P_{.col}_US"))
# 
# #Calc state percentiles ####

states <- na.omit(unique(df.var.wm$ST_ABBREV))
temp_state <- lapply(states, function(x){
  ti2 <- df.var.wm %>%
    dplyr::filter(ST_ABBREV==x) %>%
    dplyr::filter(!is.na(shape_ID))  %>%
    dplyr::mutate(across(PM25:LINGISOPCT,
                         list(~round(ecdf(na.omit(ejscreen_data %>%
                                                    as.data.frame() %>%
                                                    dplyr::filter(ST_ABBREV==x) %>%
                                                    dplyr::select(cur_column()) %>%
                                                    unlist() %>%
                                                    as.numeric()))(.)*100
                                     ,0)),
                         .names="P_{.col}_state")) %>%
    dplyr::mutate(across(med_inc:frac_pov99,
                         list(~round(ecdf(na.omit(acs_data %>%
                                                    as.data.frame() %>%
                                                    dplyr::filter(state==x) %>%
                                                    dplyr::select(cur_column()) %>%
                                                    unlist() %>%
                                                    as.numeric()))(.)*100
                                     ,0)),
                         .names="P_{.col}_state"))
})

# name the percentile columns ####
#
df.var.wm <- data.table::rbindlist(temp_state) %>%
  dplyr::rename_at(vars(rename_cols), ~paste0('P_',all_of(rename_cols),'_raw')) %>%
  tidyr::pivot_longer(cols=starts_with("P_"),
                      names_to="variable",
                      values_to = "value") %>%
  dplyr::mutate(variable=stringi::stri_replace_last_fixed(variable,'_','.')) %>%
  tidyr::separate(variable, into=c("variable","geography"), sep="\\.",extra="merge", fill = "left")   %>%
  tidyr::pivot_wider(names_from = c(variable)) %>%
  dplyr::rename('Lead'        = P_PRE1960PCT,
                'Diesel PM'         = P_DSLPM,
                'Air, Cancer'       = P_CANCER,
                'Resp. Hazard'      = P_RESP,
                'Traffic'           = P_PTRAF,
                'WW Discharge'      = P_PWDIS,
                'NPL'               = P_PNPL,
                'RMP Facility'      = P_PRMP,
                'TSD Facility'      = P_PTSDF,
                'Ozone'             = P_OZONE,
                'PM'                = P_PM25,
                'Demo. Index'       = P_VULEOPCT,
                Minority            = P_MINORPCT,
                'Low Income'        = P_LOWINCPCT,
                'Less HS Educ'      = P_LESSHSPCT,
                'Ling. Isol.'       = P_LINGISOPCT,
                'Age Under 5'       = P_UNDER5PCT,
                'Age Over 64'       = P_OVER64PCT,
                'Median Income'     = P_med_inc,
                'Caucasian (%)'     = P_frac_white,
                'Black (%)'         = P_frac_black,
                'Amer. Ind. (%)'    = P_frac_amerind,
                'Asian (%)'         = P_frac_asian,
                'Pac. Isl (%)'      = P_frac_pacisl,
                'Hispanic (%)'      = P_frac_hisp,
                '<50% P.L. (%)'     = P_frac_pov50,
                '<100% P.L. (%)'    = P_frac_pov99) %>%
  dplyr::select(-ST_ABBREV)
# 
#  sum count of pop at each site  ####
# 
# # Sum of population w/in 5miles
df.pop.sum <- list_data %>%
  dplyr::select(ACSTOTPOP, shape_ID) %>%
  dplyr::rename(`Pop. Count` = ACSTOTPOP) %>%
  dplyr::group_by(shape_ID) %>%
  dplyr::summarize_at(vars(`Pop. Count`), funs(sum))
# 
# Get lat/lon from facil_data #### 
# , (previously: URL to the facility's DFR)
df.latlon <- facil_data %>%
  dplyr::select(shape_ID, geometry) %>%
  sf::st_transform(crs = 4326)

# Merge wtd means and sum pop counts  ####
together.sf <- dplyr::inner_join(df.var.wm, df.pop.sum, by = "shape_ID") %>%
  dplyr::inner_join(df.latlon, by = 'shape_ID') %>%
  dplyr::relocate(shape_ID, `Pop. Count`,
                  `Low Income`, `Minority`, `Less HS Educ`, `Ling. Isol.`,
                  `Age Under 5`, `Age Over 64`, `Air, Cancer`, `Diesel PM`,
                  Lead, Ozone, PM, NPL, `RMP Facility`, Traffic, `TSD Facility`,
                  `WW Discharge`, `Resp. Hazard` )



