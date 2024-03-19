# FORMULAS THAT WERE BEING USED IN EJAM::doaggregate()

## examples showing how it works are in ?calc_ejam()


formulas_d <- c(
  "pctover64       = 1 * ifelse(pop == 0, 0,            over64        / pop)",
  "pctunder5       = 1 * ifelse(pop == 0, 0,            under5        / pop)",

  "pcthisp         = 1 * ifelse(pop == 0, 0, as.numeric(hisp )        / pop)",
  "pctnhba         = 1 * ifelse(pop == 0, 0, as.numeric(nhba )        / pop)",
  "pctnhaiana      = 1 * ifelse(pop == 0, 0, as.numeric(nhaiana)      / pop)",
  "pctnhaa         = 1 * ifelse(pop == 0, 0, as.numeric(nhaa )        / pop)",
  "pctnhnhpia      = 1 * ifelse(pop == 0, 0, as.numeric(nhnhpia )     / pop)",
  "pctnhotheralone = 1 * ifelse(pop == 0, 0, as.numeric(nhotheralone) / pop)",
  "pctnhmulti      = 1 * ifelse(pop == 0, 0, as.numeric(nhmulti )     / pop)",
  "pctnhwa         = 1 * ifelse(pop == 0, 0, as.numeric(nhwa )        / pop)",

  "pctba         = 1 * ifelse(pop == 0, 0, as.numeric(ba )        / pop)",
  "pctaiana      = 1 * ifelse(pop == 0, 0, as.numeric(aiana)      / pop)",
  "pctaa         = 1 * ifelse(pop == 0, 0, as.numeric(aa )        / pop)",
  "pctnhpia      = 1 * ifelse(pop == 0, 0, as.numeric(nhpia )     / pop)",
  "pctotheralone = 1 * ifelse(pop == 0, 0, as.numeric(otheralone) / pop)",
  "pctmulti      = 1 * ifelse(pop == 0, 0, as.numeric(multi )     / pop)",
  "pctwa         = 1 * ifelse(pop == 0, 0, as.numeric(wa )        / pop)",

  "pctmin          = 1 * ifelse(pop == 0, 0, as.numeric(mins)         / pop)",
  "pctlowinc       = 1 * ifelse(povknownratio  == 0, 0, lowinc                 / povknownratio)",
  "pctlths         = 1 * ifelse(age25up        == 0, 0, as.numeric(lths)       / age25up)",
  "pctlingiso      = 1 * ifelse(hhlds          == 0, 0, lingiso                / hhlds)",
  "pctpre1960      = 1 * ifelse(builtunits     == 0, 0, pre1960                / builtunits)",
  "pctunemployed   = 1 * ifelse(unemployedbase == 0, 0, as.numeric(unemployed) / unemployedbase)",

  "Demog.Index = (pctlowinc + pctmin) / 2",
  # "Demog.Index.Supp  = (pctlowinc + pctunemployed + pctlths + pctlingiso + lowlifex ) / ifelse(is.na(lowlifex), 4, 5)",
  "Demog.Index.Supp  = (pctlowinc + pctunemployed + pctlths + pctlingiso + ifelse(is.na(lowlifex), 0, lowlifex) ) / ifelse(is.na(lowlifex), 4, 5)",

  "pctdisability  = ifelse(DISAB_UNIVERSE == 0, 0, DISABILITY / DISAB_UNIVERSE)",
  "pctunder18 =  ifelse(pop == 0, 0, AGE_LT18 / pop)",
  "pctover17  =  ifelse(pop == 0, 0, AGE_GT17 / pop)",
  "pctmale    =  ifelse(pop == 0, 0, MALES    / pop)",
  "pctfemale  =  ifelse(pop == 0, 0, FEMALES  / pop)",
  "p_own_occupied =  ifelse(OCCHU == 0, 0, OWNHU / OCCHU)",
  "PCT_HH_BPOV  =  ifelse(hhlds == 0, 0, HH_BPOV / hhlds)",
  "pct_lan_eng     = ifelse(LAN_UNIVERSE == 0, 0, LAN_ENG_NA   / LAN_UNIVERSE)",
  "pct_lan_spanish = ifelse(LAN_UNIVERSE == 0, 0, LAN_SPANISH  / LAN_UNIVERSE)",
  "pct_lan_ie      = ifelse(LAN_UNIVERSE == 0, 0, LAN_IE       / LAN_UNIVERSE)",
  "pct_lan_api     = ifelse(LAN_UNIVERSE == 0, 0, LAN_API      / LAN_UNIVERSE)",
  "PCT_HLI_SPANISH_LI = ifelse(lingiso == 0, 0, HLI_SPANISH_LI  /  lingiso)",
  "PCT_HLI_IE_LI      = ifelse(lingiso == 0, 0, HLI_IE_LI       /  lingiso)",
  "PCT_HLI_API_LI     = ifelse(lingiso == 0, 0, HLI_API_LI      /  lingiso)",
  "PCT_HLI_OTHER_LI   = ifelse(lingiso == 0, 0, HLI_OTHER_LI    /  lingiso)"
)

usethis::use_data(formulas_d, overwrite = TRUE)

################################################ #
