

#' What percentage of this demographic group's population lives less than X miles from a site?
#'
#' @description This plots the cumulative share of residents found within each distance,
#'   for a single demographic group.
#'
#'   This function uses the distance of each Census block from the site in conjunction with
#'   the block group demographics, to provide a relatively detailed picture of
#'   how far away residents in each group live. In contrast, the function
#'   [distance_cdf_by_group_plot()] is based on ejamit()$results_bybg_people,
#'   which provides only block group resolution information about distance.
#'
#' @param s2b output of [getblocksnearby()], a data.table of distance from each block centroid to each site.
#'   If NULL (not provided as a parameter to the function), the function will just show an example using a random point.
#' @param sitenum one number that is the unique ID (the row number of original list of points) to look at in s2b
#' @param myvars a colname of a population count variable in blockgroupstats indicating which to plot, like "hisp" or "lowinc"
#'   and only works for one indicator at a time so far.
#' @param dpctvar a colname of usastats and statestats that is the percentage version of myvars, like "pcthisp" or "pctlowinc"
#'
#' @return returns s2b but with more columns in it, like cumpop, cumdpop, pctdwithin
#' @examples
#'   plot_distance_by_pctd()
#'
#' @export
#'
plot_distance_by_pctd <- function(s2b = NULL, sitenum = NULL, dpctvar = "pctlowinc",
                                  myvars = c(names_d_count, names_d_subgroups_count)[1],
                                  demoglabel=fixcolnames(myvars,"r","shortlabel")
                                  ) {

if (is.null(s2b)) {

  s2b <- copy(
    getblocksnearby(testpoints_n(2), radius = 6.2)
    #
    )
}
  if (!("blockid" %in% names(s2b))) {
    # maybe they provide just latlon table
    s2b <- latlon_from_anything(s2b)
    if (all("lat" %in% names(s2b), "lon" %in% names(s2b))) {
      s2b <- getblocksnearby(s2b, 10) # 10 miles arbitrarily just to include more
    }
  }

  if (is.null(sitenum)) {
   sitenum <- s2b$ejam_uniq_id[1]
  }
  # browser()
  # s2b_example <- getblocksnearby(testpoints_n(2), radius = 3.2)
  # s2b <- copy(s2b_example)
  # sitenum <- sample(unique(s2b$ejam_uniq_id), 1)
# myvars <- sample(c(names_d_count, names_d_subgroups_count), 1)

myST <- state_from_blockid(as.vector(unlist(s2b[ejam_uniq_id == sitenum, blockid[1]])))
myvarsall <- c(myvars, "pop", "bgid")
s2b <- data.table::copy(s2b[ejam_uniq_id == sitenum & blockwt > 0, ])
s2b <- merge(s2b, blockgroupstats[pop > 0, ..myvarsall], all.x = TRUE, all.y = FALSE, by = "bgid")

# FORMULAS GO HERE:
# first, hard-coded variables that are calculated only as percentage of population total
s2b[ , dpop := ..myvars]
s2b[ , blockpop  :=  pop * blockwt]
s2b$dpop <- s2b[ , ..myvars]
s2b[ , blockdpop := dpop * blockwt]

data.table::setorder(s2b, distance)

s2b[ , cumpop  := cumsum(blockpop)]
s2b[ , cumdpop := cumsum(blockdpop)]

s2b[ , pctdwithin := cumdpop / cumpop]

plot(s2b$distance, s2b$pctdwithin, type = "b",
     xlab = "Distance (miles)", ylab = "Percent Demographics within X miles",
     xlim = c(0, max(s2b$distance)),
     ylim = c(0, usastats[usastats$PCTILE == 100, dpctvar]),
     main = paste0(fixcolnames(dpctvar, "r", "shortlabel"), " as a function of distance from site number ", sitenum))

abline( h = statestats[statestats$PCTILE == "mean" & statestats$REGION == myST, dpctvar], col = "red")
abline( h = statestats[statestats$PCTILE == "75"   & statestats$REGION == myST, dpctvar], col = "red",  lty = 4, lwd = 0.5)
abline( h = usastats[    usastats$PCTILE == "mean"                            , dpctvar], col = "blue")
abline( h = usastats[  usastats$PCTILE == "75"                              , dpctvar], col = "blue", lty = 4, lwd = 0.5)
legend("topright",legend = c("State 75th %ile blockgroup", "State overall", "US 75th %ile bg", "US overall"), fill = c("red", "red", "blue", "blue"))
return(s2b)


################################################################################################################ #

## if writing more flexible general code, see doaggregate etc. for using 3 kinds of calculated variables.
## Specify Which vars are SUM OF COUNTS, vs WTD AVG, vs via FORMULA (usually ratio of sums of counts) ####
## That info is sort of stored already in map_headernames$calculation_type
##
# calctype <- function(varnames) {
#   map_headernames$calculation_type[match(varnames, map_headernames$rname)]
# }
#
# thesevars <- c(names_d_other_count, names_d_count, names_d_subgroups_count, names_d)
#
# countcols      <- thesevars[calctype(thesevars) == "sum of counts"  ]
# popmeancols    <- thesevars[calctype(thesevars) == "popwtd mean"    ]
# calculatedcols <- thesevars[calctype(thesevars) == "percent formula"]
#
# countcols_inbgstat       <- intersect(countcols,      names(blockgroupstats))
# popmeancols_inbgstats    <- intersect(popmeancols,    names(blockgroupstats))
# calculatedcols_inbgstats <- intersect(calculatedcols, names(blockgroupstats))


# myvars <- c(names_d_subgroups_count) #  "hisp"         "nhba"         "nhaa"         "nhaiana"      "nhnhpia"      "nhotheralone" "nhmulti"      "nhwa"
### or for percentages that do not always use pop as denominator, etc:
# myvars <- c(names_d_count, names_d_other_count)
# [1] "lowinc"         "lingiso"        "unemployed"     "lths"           "under5"         "over64"         "mins"           "pop"            "nonmins"        "povknownratio"  "age25up"
# [12] "hhlds"          "unemployedbase" "pre1960"        "builtunits"
###### #

}
