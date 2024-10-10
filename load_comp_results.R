

library(EJAM)
#devtools::load_all()

load('ejscreen_output_testpoints_1000_3_miles.rda')
load('ejam_output_testpoints1000_3miles.rda')

new_comp <- EJAM:::ejscreen_vs_ejam_alreadyrun(
  apisite = ejscreen_res_1000$table,
  ejamsite = ejam_res_1000$results_bysite
)

EJAM:::ejscreen_vs_ejam_summary(new_comp)

EJAM:::ejscreen_vs_ejam_1var(new_comp, varname = 'state.pctile.EJ.DISPARITY.rsei.eo')

EJAM:::ejscreen_vs_ejam_summary_quantiles(new_comp, mystat = 'pctdiff', myvars = names_d_pctile)
EJAM:::ejscreen_vs_ejam_summary_quantiles(new_comp, mystat = 'pctdiff', myvars = names_d_pctile, probs=c(0, 0.01, 0.02, 0.05, 0.95, 0.98, 0.99, 1))

library(openxlsx)

varNames <- c(names_d_pctile, names_e_pctile, names_ej_pctile, names_ej_supp_pctile, names_d_state_pctile, names_e_state_pctile, names_ej_state_pctile, names_ej_supp_state_pctile)


calculate_pct_diff_summary <- function(vs, varnames, output_file) {
  results_list <- list()
  
  for (varname in varnames) {
    data <- ejscreen_vs_ejam_1var(vs, varname = varname)
    total_sites <- nrow(data)
    
    same_count <- sum(data$pctdiff == 0, na.rm = TRUE)
    same_pct <- (same_count / total_sites) * 100
    
    
    
    within_1absolute_count <- sum(abs(data$absdiff) <= 1, na.rm = TRUE)
    within_1absolute <- (within_1absolute_count / total_sites) * 100
    
    within_2absolute_count <- sum(abs(data$absdiff) <= 2, na.rm = TRUE)
    within_2absolute <- (within_2absolute_count / total_sites) * 100
    
    ejam_na_count <- sum(is.na(data$EJAM))
    ejscreen_na_count <- sum(is.na(data$EJSCREEN))
    
    results_list[[varname]] <- c(Same = same_pct, 
                                 `Within 1%` = within_1absolute, 
                                 `Within 2%` = within_2absolute,
                                 EJAM_NA_Count = ejam_na_count, 
                                 EJScreen_NA_Count = ejscreen_na_count)
  }
  
  results_df <- do.call(rbind, results_list)
  results_df <- as.data.frame(results_df)
  results_df$Variable <- rownames(results_df)
  results_df <- results_df[order(results_df$Same), ]
  write.xlsx(as.data.frame(results_df), file = output_file)
}

calculate_pct_diff_summary(new_comp,varNames,"percentile_variables.xlsx")

for(name in varNames){
  print(name)
}

