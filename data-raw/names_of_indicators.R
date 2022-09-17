## code to prepare `names_of_indicators` dataset goes here


# Define lists of names of EJScreen-related variables for use here

# ejscreen::names.e, etc. are names_e, etc. in this pkg

names_e <- c("pm", "o3", "cancer", "resp", "dpm", "pctpre1960", "traffic.score",  "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", "ust")
#  Demog.Index
names_d <- c("VSI.eo", "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5",  "pctover64", "pctunemployed")
names_d_count <- gsub('pct', '', names_d); names_d_count <- gsub('min', 'mins', names_d_count)
names_other <- c("pop","nonmins","povknownratio","age25up", "hhlds","unemployedbase", "pre1960","builtunits")

names_d_subgroups <- c("pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti")
names_d_subgroups_count <- c("nhwa", "hisp", "nhba", "nhaa", "nhaiana", "nhnhpia", "nhotheralone",  "nhmulti")

#***  names_d_subgroups_pctile and _state_pctile  are missing - not created by buffering?? or..?

names_ej <- paste0('EJ.DISPARITY.', names_e, '.eo')

names_d_pctile  <- paste0('pctile.', names_d)
names_e_pctile  <- paste0('pctile.', names_e)
names_ej_pctile <- paste0('pctile.', names_ej)

names_d_state_pctile  <- paste0('state.pctile.', names_d)
names_e_state_pctile  <- paste0('state.pctile.', names_e)
names_ej_state_pctile <- paste0('state.pctile.', names_ej)


names_all <- c(
  names_e, names_e_pctile, names_e_state_pctile, 
  names_d, names_d_pctile, names_d_state_pctile, 
  names_d_count, names_other, 
  names_d_subgroups, names_d_subgroups_count, 
  names_ej, names_ej_pctile, names_ej_state_pctile
)


usethis::use_data(
  names_e, names_e_pctile, names_e_state_pctile, 
  names_d, names_d_pctile, names_d_state_pctile, 
  names_d_count, names_other, 
  names_d_subgroups, names_d_subgroups_count, 
  names_ej, names_ej_pctile, names_ej_state_pctile,
  names_all,
  overwrite = TRUE
) 
# 
