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
  cbind(in.namez = namez[[gsub('names_','' , x) ]] , rnames = map_headernames$rname[map_headernames$varlist == x],  match.or.not = ynlist )
  )}
}
rm(ynlist) 

########################################################################################## #

# names_d_other_count  -    FALSE that all match.    # SORT ORDERS DIFFER but probably does not matter
# 
# in.namez         rnames           match.or.not
# [1,] "pop"            "builtunits"     "FALSE"     
# [2,] "nonmins"        "hhlds"          "FALSE"     
# [3,] "povknownratio"  "pre1960"        "FALSE"     
# [4,] "age25up"        "povknownratio"  "FALSE"     
# [5,] "hhlds"          "unemployedbase" "FALSE"     
# [6,] "unemployedbase" "age25up"        "FALSE"     
# [7,] "pre1960"        "nonmins"        "FALSE"     
# [8,] "builtunits"     "pop"            "FALSE"   
########################################################################################## # 

map_headernames[!grepl('bin|text|.supp|.eo', map_headernames$rname) & grepl('DISPARITY', map_headernames$rname), c('rname', 'varlist')]

map_headernames[   map_headernames$varlist %in% c('names_ej', 'names_ej_state'), c('rname', 'varlist')]

# map_headernames[   map_headernames$varlist %in% c('names_ej', 'names_ej_state'), c('rname', 'varlist',   'apiname', "csvname2.2")]



pop
poc
povknown
25
hhld
unem
unitsjpre60
units





# compare friendly names in lists like namez$names_d_friendly etc.  to  map_headernames$names_friendly in matching rows matched on 
# map_headernames$varlist  matching  gsub('_friendly', '', x) 

alist_friendly = paste0('names_', grep('friendly', names(namez), value = T))
alist = gsub('_friendly', '', alist_friendly)

for (i in 1:length(alist)) {
  cat('\n\n', alist[i], '\n\n')
  aa_namez =  as.vector( unlist(namez[gsub('names_','', alist_friendly[i])]) )
  rownames(inamez) <- NULL
  bb_fr <- map_headernames[map_headernames$varlist == alist[i], c( "names_friendly")]
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







