# print lists of package dependencies of my source pkgs

library(attachment)

parentfolder <- Sys.getenv('R_USER')
foles <- file.path(dir(parentfolder), 'R') 
# assume other folders may be found but all those and only those with /R are packages
folesR <- foles[dir.exists(foles)] 
foles <- gsub('/R', '', folesR)
i = 0
out <- list(rep(NA,length(folesR)))
for (here in folesR) {
  i = i + 1
  these <- attachment::att_from_rscripts(here)
  if (length(these) == 0) {these <- NA}
  cat(' ******* ', foles[i], 'depends on: *******\n')
   # print(these)
  cat(paste0(paste(these, collapse = ', '),'\n'))
  out[[i]] <- these
}
names(out) <- foles
x <- cbind(rep(names(out), sapply(out, length)), unlist(out))
rownames(x) <- NULL
colnames(x) <- c('pkg', 'depends')
x <- as.data.frame(x)
x

# if you do not need to see the full list including those with no listed dependency
x = x[!is.na(x$depends), ]

# remove self depend
x = x[x$pkg != x$depends, ]

# only see source packages in left col
print('which of the source pkgs depend on other source pkgs:')
y = x[x$depends %in% foles, ]
# sort by what it depends on
y[order(y$depends),]


 

# other approach to look into

library(deepdep)
library(shinycssloaders)
deepdep('EJAM',local = TRUE)
deepdep_shiny() # must be on CRAN
