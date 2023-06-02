
# this was a quick look at comparing EJAM and EJScreen API outputs to see 
# how much the reported percentiles differ


library(EJAM)
# data(usastats,   package = "EJAM")
# data(statestats, package = "EJAM")
# data(names_d,    package = "EJAM")
# data(names_e,    package = "EJAM")

pts <- EJAM::testpoints_n(10)

ejscreenout <- EJAMejscreenapi::ejscreenit(pts, radius = 1, 
  save_map = F, save_plot = F, save_table = F, see_plot = F, see_map = F) 
ejscreen_table <- ejscreenout$table

ejamout <- EJAM::ejamit(pts, radius = 1)
ejam_table <- data.table::setDF(data.table::copy(ejamout$results_bysite))

# names_valid <-  c("Demog.Index", "pctlowinc", "pctlingiso", "pctunemployed", "pctlths", "pctunder5", "pctover64", "pctmin")
names_valid <- names_d[c(1,3:6,8:10)] 

for (vname in names_valid) {
    print(EJAM::pctile_from_raw_lookup(ejscreen_table[, vname] / 100, vname, lookup = usastats)
          - ejscreen_table[,paste0("pctile.", vname)] )
}

for (vname in c(names_e)) {
    print(pctile_from_raw_lookup(ejscreen_table[, vname], vname, lookup = usastats)
          - ejscreen_table[, paste0("pctile.", vname)] )
}

print(ejam_table[,paste0("pctile.",  names_e)])
print(ejam_table[,paste0("pctile.",  names_valid)])

                 
