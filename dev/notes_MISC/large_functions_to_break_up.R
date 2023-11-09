
# lc <- linesofcode2('./..', packages = 'EJAM') # 
lc
lc[lc$where == "/R/", ]
lc <- lc[lc$where == "/R/" & lc$code >= 100, ]
lc <- lc[order(lc$code, decreasing = TRUE), ]
lc

# > lc
#    lines comments code package where                                     filename

# 1   2774       33 2741    EJAM   /R/                                 app_server.R ** IMPORTANT TO BREAK UP 
# 3   1612       18 1594    EJAM   /R/                                     app_ui.R ** IMPORTANT TO BREAK UP

# ** Break up app_ui.R - first, store EPA template in separate file. 
#   and then maybe use modules if appropriate for key parts of UI, but not sure if modules are really worth it & are the only option. 

# ** Break up app_server.R, using functions (but without sacrificing performance hit of passing lots of data to/from a function), and modules (only if worth it!). 
#
# Need to look through server code and decide what portions are best made into functions we can test outside shiny.


# 2   1658       95 1563    EJAM   /R/                                doaggregate.R *** break up into functions, only if speed of data.table aggregation work is not slowed down by that. maybe can use functions that modify passed data.tables by reference to update them in the parent frame without having to copy what is passed to/from the function.


# 5   1325       47 1278    EJAM   /R/                         MODULE_ejscreenapi.R

# 7    916       96  820    EJAM   /R/                           table_xls_format.R  ***
# 10   492       98  394    EJAM   /R/                                  table_gt_.R  ***

# 12   424      106  318    EJAM   /R/                            utils_speedtest.R

# 14   373       77  296    EJAM   /R/                                     ejamit.R

# 18   309       66  243    EJAM   /R/                 plot_distance_cdf_by_group.R
# 13   408      170  238    EJAM   /R/                             golem_utils_ui.R

# 22   274       54  220    EJAM   /R/                 getblocksnearbyviaQuadTree.R  *
# 23   273       54  219    EJAM   /R/          getblocksnearbyviaQuadTree2-DRAFT.R

# 36   211       49  162    EJAM   /R/                  url_getacs_epaquery-DRAFT.R

# 38   205       45  160    EJAM   /R/                       MODULE_latlontypedin.R

# 37   209       52  157    EJAM   /R/                          dataload_from_aws.R
# 28   237       81  156    EJAM   /R/                     pctile_from_raw_lookup.R * may want to clean this up
# 21   283      128  155    EJAM   /R/                           statestats_query.R
# 52   164       17  147    EJAM   /R/ getblocksnearbyviaQuadTree_Clustered-DRAFT.R

# 49   170       28  142    EJAM   /R/                        table_xls_from_ejam.R * may want to clean this up

# 34   214       73  141    EJAM   /R/                plot_distance_mean_by_group.R

