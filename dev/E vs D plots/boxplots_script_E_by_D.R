library(data.table)
library(EJAM)
if (interactive()) {
  myfolder <- rstudioapi::selectDirectory("Where should .png files be saved?")
} else {
  myfolder <- getwd()
}
evarnames <- names_e
dvarnames <- c(names_d, names_d_subgroups)
bg <- (data.table::copy(blockgroupstats[ , c(..evarnames, ..dvarnames)]))
setDF(bg)
bg[ , names_d_subgroups] <- bg[ , names_d_subgroups] / 100
setDT(bg)

for (i.e in (1:length(evarnames))) {
  
  evarname <- evarnames[i.e]
  evarlabel <-  fixcolnames(evarname, 'r', 'long')
  
  for (i.d in (1:length(dvarnames))) {
    
    dvarname <- dvarnames[i.d]
    ok <- !is.na(bg[ , ..evarname, drop = T]) & !is.na(bg[ , ..dvarname, drop = T])
    ok <- ok[ , 1]
    bgi <- bg[ok, c(..evarname, ..dvarname)]
    evar <- bgi[[evarname]] # as.vector(unlist(bgi[ , ..evarname]))
    dvar <- bgi[[dvarname]] # as.vector(unlist(bgi[ , ..dvarname]))
    dvarlabel <-  fixcolnames(dvarname, 'r', 'long')
    
    brks <- quantile(dvar, probs = seq(0, 1, 0.20), na.rm = T)
    validbreakn <- max(which(brks == min(brks))):6
    brks <- brks[validbreakn]
    if (min(validbreakn) > 1) {note <- paste0(" (bottom ", min(validbreakn), " quintiles)")} else {note <- ""}
    rangelabs <- paste0(paste0(round(100 * brks[1:(length(brks) - 1)], 1)), "-", paste0(round(100 * brks[2:(length(brks))], 1), "%"))
    rangelabs[1] <- paste0(rangelabs[1], note)
    cuts <- cut(dvar, breaks = brks, include.lowest = T)
    bgi[ , cuts := cuts]
    avgs <- bgi[ , lapply(.SD, mean),   keyby = "cuts", .SDcols = evarname]
    meds <- bgi[ , lapply(.SD, median), keyby = "cuts", .SDcols = evarname]
    avgs <- avgs[[evarname]]
    meds <- meds[[evarname]]
    avgall  <- bg[ , mean(  ..evar, na.rm = T)] 
    medsall <- bg[ , median(..evar, na.rm = T)] 
    
    cat(dvarname, '-', evarname, '\n')  # ; cat('breaks:  '); print(brks)
    # cat('averages:  '); print(avgs)
    # cat('medians:   '); print(meds)
    
    png(width = 1333, height = 800, filename = paste0(
      myfolder, "/", "boxplots E by D quintile - ", evarname, " vs ", dvarname, ".png")
    )
    
    boxplot(
      evar ~ cuts,
      xlab = paste0("Demographics (quintiles of block groups based on ", dvarlabel,")"),
      ylab = "Environmental Indicator scores (red = average)",
      names = rangelabs,
      main = paste0(evarlabel, " scores in each category of places\n as grouped by ", dvarlabel, "")
    )
    
    points(avgs, col = "red")
    abline(h = avgall, col = "red")
    # points(meds, col = "gray")
    abline(h = medsall, col = 'gray')
    
    dev.off()
    
  }
}

# dev.off()
