
##################### # 
# morenames <- c(names_these, names_d_subgroups_alone, names_d_subgroups_nh ) # not raw EJ indexes though?
avg.in.us <-   usastats[ usastats$PCTILE == "mean", intersect(names_these, names( usastats))]

avg.in.us <- metadata_add(avg.in.us)

# note the regular name not avg. name is used in the usastats table
usethis::use_data(avg.in.us, overwrite = TRUE)

