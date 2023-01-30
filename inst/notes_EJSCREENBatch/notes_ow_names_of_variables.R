ow_names1 <- c("ACSTOTPOP", "PM25", "OZONE", "DSLPM", "CANCER,
RESP", "PTRAF", "PNPL", "PRMP", "PRE1960PCT", "PTSDF", "PWDIS,
VULEOPCT", "MINORPCT", "LOWINCPCT", "UNDER5PCT,LESSHSPCT,
OVER64PCT", "LINGISOPCT", "med_inc", "frac_white", "frac_black,
frac_amerind", "frac_asian", "frac_pacisl", "frac_hisp,
frac_pov50", "frac_pov99")

x <- list(
  'Lead'              = 'P_PRE1960PCT',
  'Diesel PM'         = 'P_DSLPM',
  'Air, Cancer'       = 'P_CANCER',
  'Resp. Hazard'      = 'P_RESP',
  'Traffic'           = 'P_PTRAF',
  'WW Discharge'      = 'P_PWDIS',
  'NPL'               = 'P_PNPL',
  'RMP Facility'      = 'P_PRMP',
  'TSD Facility'      = 'P_PTSDF',
  'Ozone'             = 'P_OZONE',
  'PM'                = 'P_PM25',
  'Demo. Index'       = 'P_VULEOPCT',
  'Minority'          = 'P_MINORPCT',
  'Low Income'        = 'P_LOWINCPCT',
  'Less HS Educ'      = 'P_LESSHSPCT',
  'Ling. Isol.'       = 'P_LINGISOPCT',
  'Age Under 5'       = 'P_UNDER5PCT',
  'Age Over 64'       = 'P_OVER64PCT',
  'Median Income'     = 'P_med_inc',
  'Caucasian (%)'     = 'P_frac_white',
  'Black (%)'         = 'P_frac_black',
  'Amer. Ind. (%)'    = 'P_frac_amerind',
  'Asian (%)'         = 'P_frac_asian',
  'Pac. Isl (%)'      = 'P_frac_pacisl',
  'Hispanic (%)'      = 'P_frac_hisp',
  '<50% P.L. (%)'     = 'P_frac_pov50',
  '<100% P.L. (%)'    = 'P_frac_pov99'
)

ow_names <- data.frame(owname_nice = names(x), owname_code = unlist(x), stringsAsFactors = FALSE)
rownames(ow_names) <- NULL
rm(x)
ow_names
