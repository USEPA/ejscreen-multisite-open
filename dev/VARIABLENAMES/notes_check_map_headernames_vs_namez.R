# compare map_headernames and namez
#   EJAMejscreenapi::map_headernames 
#   EJAM::namez and EJAM::names_d etc.  has namez$d_avg_friendly etc.

require(EJAM)

unique(EJAMejscreenapi::map_headernames$varlist)
varlists = unique(EJAMejscreenapi::map_headernames$varlist[!grepl("x_", EJAMejscreenapi::map_headernames$varlist)])

# check for each rname in map_headernames, with something in varlist col, that 

setdiff(varlists, paste0('names_', names(namez))) # should be none 
otherlists = setdiff(paste0('names_', names(namez)), varlists) # 
grep('friendly', otherlists, value=T)
grep('these', otherlists, value=T) #  "names_these"                    "names_these_avg"                "names_these_state_avg"          "names_these_ratio_to_avg"       "names_these_ratio_to_state_avg"
grep('friendly|these', otherlists, value=T, invert = T)
# "names_all_r"             "names_wts"               "names_need_pctile"       "names_need_state_pctile"

# confirmed namez has same info as the individual varlists like names_d 
for (x in names(namez)) {cat(all.equal(namez[[x]], get(paste0('names_', x))));  cat( '  - ', x, '\n')}


# see if same exact elements and same sort order for each set of indicator names in namez and in map_headernames$rname
#
for (x in varlists) {
  cat('\n')
  ynlist = namez[[gsub('names_','' , x) ]] == map_headernames$rname[map_headernames$varlist == x]
  cat(x, ' -   ' ,  all(ynlist), 'that all match. \n\n')
  # cat('all elements of namez$', x, ' equal rname entries in map_headernames for that varlist \n')
  # print()
  if (!all(ynlist)) {
  print(  
  cbind(in.namez = namez[[gsub('names_','' , x) ]] , rname = map_headernames$rname[map_headernames$varlist == x],  match.or.not = ynlist )
  )}
}
rm(ynlist) 
 # all fixed to match now.
########################################################################################## # 

map_headernames[!grepl('bin|text|.supp|.eo', map_headernames$rname) & grepl('DISPARITY', map_headernames$rname), c('rname', 'varlist')]

map_headernames[   map_headernames$varlist %in% c('names_ej', 'names_ej_state'), c('rname', 'varlist')]

# map_headernames[   map_headernames$varlist %in% c('names_ej', 'names_ej_state'), c('rname', 'varlist',   'apiname', "csvname2.2")]
 
########################################################################################## # 

# compare friendly names in lists like namez$names_d_friendly etc.  to  map_headernames$names_friendly in matching rows matched on 
# map_headernames$varlist  matching  gsub('_friendly', '', x) 

alist_friendly = paste0('names_', grep('friendly', names(namez), value = T))
alist = gsub('_friendly', '', alist_friendly)

for (i in 1:length(alist)) {
  cat('\n\n', alist[i], '\n\n')
  aa_namez =  as.vector( unlist(namez[gsub('names_','', alist_friendly[i])]) )
  rownames(inamez) <- NULL
  bb_fr <- map_headernames[map_headernames$varlist == alist[i], c( "shortlabel")] # these are shorter versions than long...
  cc_long = map_headernames[map_headernames$varlist == alist[i], c(  "longname_tableheader")]
  print(cbind(
    aa_namez = as.vector(unlist(aa_namez)) ,
    bb_fr = bb_fr,
    cc_long = cc_long, 
    a.is.b = ifelse(aa_namez ==  bb_fr, 1,0), 
    b.is.c = ifelse(bb_fr == cc_long, 1,0), 
    a.is.c = ifelse(aa_namez == cc_long, 1,0)
    # ,"description","csvlongname")] # , "varlist" ,"rname",
    ))
  }



# need to choose which of 3 versions to use for each varlist - 
# sometimes namez is best for the friendly versions
# sometimes map_headernames$names_friendly == $shortlabel  is best
#   "long" is much longer.  

# Differ on envt ones. maphead shortlabel is shortest. namez is medium. long is very very long. 

# vary a lot on EJ shortlabel names - namez and maphead each sometimes best but shortlabel are shortest.




