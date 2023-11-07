
# see list of package dependencies of EJAM ####

# mypackages <- EJAM::ejampackages

# for CRAN packages only, I think, tidymodels  package has  pkg_deps("EJAM", recursive = TRUE)
# rpd <- packrat  #  :::  #  recursivePackageDependencies
x1 <- rpd("EJAM", lib.loc = .libPaths(), ignores = NULL)
## and confirmed there are no others added by checking EJAMejscreenapi and EJAMbatch.summarizer packages.


# what EJAM needs

pkgs_ejam_depends_on <- x1[!(x1 %in% c("EJAMejscreenapi", "EJAMbatch.summarizer"))]
# > setdiff(x1, pkgs_ejam_depends_on)
# [1] "EJAMbatch.summarizer" "EJAMejscreenapi"  


# what each of those alone needs - to see why so many dependencies

# warning: recursivePackageDependencies looks at installed and pkg_deps looks at CRAN,
#  and somehow they do not always agree, as with sp, where pkg_deps() reports lattice is needed
#  but recursivePackageDependencies somehow misses lattice entirely, though sp version installed is same as on cran.
recurse <- FALSE
ps <- pkgs_ejam_depends_on #[1:40]  # but this is missing lattice, for example

x <- list()
for (i in 1:length(ps)) {
  if (recurse) {
    this   <- packrat:::recursivePackageDependencies( ps[i], ignores = NULL, lib.loc = .libPaths() )
  } else {
    this <- tidymodels::pkg_deps(ps[i], recursive = FALSE)$package # drops info about version, etc.
  }
if (is.null(this)) {this <- ""; cat("problem\n")}
cat(ps[i], ':  '); cat(this,'\n\n')
x[[i]] <- this
}
names(x) <- ps
xlist <- x
rm(x)
print(xlist)
xlist <- xlist[order(sapply(xlist, length))]
ps <- names(xlist) # to retain same order !
if (recurse) {
  deps_recurse <- xlist # [sapply(xlist, length) > 0]
} else {
  deps_direct <-  xlist # [sapply(xlist, length) > 0] # keeps them all since reports depends on self
}

# include others packrat failed to identify but pkg_deps did find like lattice:
ps_complete <- unique(as.vector(unlist(deps_direct)))
# "lattice"    "class"      "codetools"  "KernSmooth" "MASS"       "mgcv"     
# "cluster"    "foreign"    "nnet"       "rpart"   
ps <- ps_complete
pkgs_ejam_depends_on <- ps_complete


ddd <- function() {
  # matrix of all rows and cols not just the >0 ones from above
  deps_direct_df <- data.frame(matrix(0, nrow = length(ps), ncol = length(ps)))
  colnames(deps_direct_df) <- ps
  rownames(deps_direct_df) <- ps
  
  for (i in 1:length(ps)) {
    cat(i,"- ")
    if (ps[i] %in% names(deps_direct_df)) {
      
      needed <- as.vector(unlist(deps_direct[ps[i]]))
      # if (length(needed) == 1 & needed[1] == ps[i]) {needed <- NULL} # to leave a zero for depends on self 
      #  
        cat("needed for", ps[i], "are: ") ; print(needed)
      if (!is.null(needed)) {
        deps_direct_df[i, needed] <- 1
      }
    } else {
      browser()
      deps_direct_df[i, ] <- 0
    }
    if (any(is.na(deps_direct_df))) {browser()}
  }
  return(deps_direct_df)
}


deps_direct_df <-  ddd()

x = data.frame(pkg = colnames(deps_direct_df),  needed_by_how_many = colSums(deps_direct_df), stringsAsFactors = FALSE)
rownames(x) <- NULL
x = x[order(x$needed_by_how_many), ]
  
############################################################ # 

## another approach, that finds reverse or any type of depends, one pkg at a time, and seems to have graph plotting too
library(crandep)
library(dplyr)
library(igraph)
  
# df_to_graph(edgelist = y, nodelist =  y$pkg)
pkg <- colnames(deps_direct_df)
dtab <- list()
for (i in 1:NROW(deps_direct_df)) {
  dtab[[i]] <- get_dep(pkg[i],  c("Imports", "Depends"))
}

df0.imports <- do.call(rbind, dtab)

df0.imports <- df0.imports[ df0.imports$from %in% x$pkg[x$needed_by_how_many < 2], ]

df0.imports <- df0.imports[ (df0.imports$from %in% c( 'Hmisc')), ]

# df0.imports <- df0.imports[ !(df0.imports$from %in% c('DT', 'tidyverse')), ]

g0.imports <- igraph::graph_from_data_frame(df0.imports)

old.par <- par(mar = rep(0.0, 4))
plot(g0.imports, vertex.label.cex = 1.1)
par(old.par)




########## still hard to visualize that long list and see what drives what, 
## like which packages are needed only because of one direct dependency? 



# [1] "DBI"                  "DT"                  
#
# "EJAMbatch.summarizer"  
# "EJAMejscreenapi"  
# 
#      "Formula"                             "R6"                  
# [8] "RColorBrewer"         "RMySQL"               "Rcpp"                 "Rttf2pt1"             "SearchTrees"          "XML"                  "askpass"             
# [15] "attempt"              "aws.s3"               "aws.signature"        "backports"            "base64enc"            "bit"                  "bit64"               
# [22] "blob"                 "broom"                "bslib"                "cachem"               "callr"                "cellranger"           "checkmate"           
# [29] "classInt"             "cli"                  "clipr"                "collapse"             "colorspace"           "commonmark"           "config"              
# [36] "conflicted"           "cpp11"                "crayon"               "crosstalk"            "crul"                 "curl"                 "data.table"          
# [43] "dbplyr"               "desc"                 "digest"               "doSNOW"               "dplyr"                "dtplyr"               "e1071"               
# [50] "ellipsis"             "evaluate"             "extrafont"            "extrafontdb"          "fansi"                "farver"               "fastmap"             
# [57] "fontBitstreamVera"    "fontLiberation"       "fontawesome"          "fontquiver"           "forcats"              "foreach"              "fs"                  
# [64] "gargle"               "gdtools"              "generics"             "gfonts"               "ggplot2"              "ggridges"             "glue"                
# [71] "golem"                "googledrive"          "googlesheets4"        "gridExtra"            "gtable"               "haven"                "here"                
# [78] "highr"                "hms"                  "hrbrthemes"           "htmlTable"            "htmltools"            "htmlwidgets"          "httpcode"            
# [85] "httpuv"               "httr"                 "ids"                  "isoband"              "iterators"            "jquerylib"            "jsonlite"            
# [92] "knitr"                "labeling"             "later"                "lazyeval"             "leaflet"              "leaflet.extras2"      "leaflet.providers"   
# [99] "lifecycle"            "lubridate"            "magrittr"             "mapproj"              "maps"                 "matrixStats"          "memoise"             
# [106] "mime"                 "modelr"               "munsell"              "openssl"              "openxlsx"             "pdist"                "pillar"              
# [113] "pkgbuild"             "pkgconfig"            "pkgload"              "png"                  "prettyunits"          "processx"             "progress"            
# [120] "promises"             "proxy"                "ps"                   "purrr"                "ragg"                 "rappdirs"             "raster"              
# [127] "readr"                "readxl"               "rematch"              "rematch2"             "reprex"               "rhandsontable"        "rlang"               
# [134] "rmarkdown"            "rprojroot"            "rstudioapi"           "rvest"                "s2"                   "sass"                 "scales"              
# [141] "selectr"              "sf"                   "shiny"                "shinyBS"              "shinycssloaders"      "shinydisconnect"      "shinyjs"             
# [148] "snow"                 "sourcetools"          "sp"                   "stringi"              "stringr"              "sys"                  "systemfonts"         
# [155] "terra"                "textshaping"          "tibble"               "tidyr"                "tidyselect"           "tidyverse"            "timechange"          
# [162] "tinytex"              "triebeard"            "tzdb"                 "units"                "urltools"             "utf8"                 "uuid"                
# [169] "vctrs"                "viridis"              "viridisLite"          "vroom"                "withr"                "wk"                   "xfun"                
# [176] "xml2"                 "xtable"               "yaml"                 "zip"                 


####################################################################### # 

# VISUALIZE PACKAGE DEPENDENCIES GRAPH ####



# THIS IS NOT USEFUL:
# lib X rary X (miniCRAN )
# conflict_prefer_matching(".*", "tidyr")
conflict_prefer_matching(":", "base")
tags <- pkgs_ejam_depends_on[1:4]
x = pkgDep(
  pkg = pkgs_ejam_depends_on, 
  # availPkgs = pkgs_ejam_depends_on,
           suggests = FALSE, enhances = FALSE, depends = TRUE) #, availPk gs = cranJuly2014)
y = makeDepGraph(x)
plot(y)


####################################################################### # 

# see info about each package EJAM depends on #### 

#### from the package called riskmetric 
# https://pharmar.github.io/riskmetric/articles/riskmetric.html#creating-a-package-reference-object

package_tbl <- pkg_ref( pkgs_ejam_depends_on ) %>%
  as_tibble()

ejam_riskmetric_assessment <-  package_tbl %>%
  pkg_assess()
 
ejam_riskmetric_scores_df <- ejam_riskmetric_scores %>% dplyr::arrange(pkg_score) %>% dplyr::select(c(package, pkg_score, downloads_1yr)) %>% as.data.frame()

####################################################################### # 
#   API requests for risk metric for each package:
# 
# single package:
#   
#   curl -d \
# '{"package": {"name": "htmltools"}, "version": "2.1.2rc"}' \
# "https://api.osv.dev/v1/query"
# 
# 
# 
# 
# batch of packages:
#   
#   
#   cat <<EOF | curl -d @- "https://api.osv.dev/v1/querybatch"
# {
#   "queries": [
#     {
#       "package": {
#         "purl": "pkg:pypi/mlflow@0.4.0"
#       }
#     },
#     {
#       "commit": "6879efc2c1596d11a6a6ad296f80063b558d5e0f"
#     },
#     {
#       "package": {
#         "ecosystem": "CRAN",
#         "name": "clipr"
#       },
#       "version": "2.4.1"
#     }
#   ]
# }
# EOF
####################################################################### # 

#  ejam_riskmetric_scores_df

#               package pkg_score downloads_1yr

# 1               clipr 0.3043212     0.9739758
# 2               cpp11 0.3093741     0.9874335
# 3              config 0.3167541     0.8096118
# 4            classInt 0.3325511     0.9271788
# 5               broom 0.3458757     0.9819717
# 6                here 0.3468462     0.9294380
# 7           backports 0.3473268     0.9734928
# 8                crul 0.3500614     0.8409443
# 9                Rcpp 0.3534055     0.9867380
# 10          rprojroot 0.3545133     0.9757091
# 11               glue 0.3548480     0.9892528
# 12                 R6 0.3564960     0.9855454

# 13         data.table 0.3566542     0.9838150

# 14           magrittr 0.3575685     0.9883338
# 15          lifecycle 0.3586671     0.9930748
# 16         cellranger 0.3591666     0.9710047
# 17              units 0.3595597     0.9654781
# 18           collapse 0.3597134     0.8233155
# 19                DBI 0.3605564     0.9736778
# 20                 fs 0.3624197     0.9870056
# 21          checkmate 0.3626423     0.9512811
# 22            foreach 0.3640957     0.9469649
# 23         rstudioapi 0.3659646     0.9785323
# 24        matrixStats 0.3662368     0.9444595
# 25              later 0.3676982     0.9730101
# 26               xml2 0.3678064     0.9834141
# 27             crayon 0.3685904     0.9840528
# 28             readxl 0.3712147     0.9833504
# 29              fansi 0.3725774     0.9867926
# 30            viridis 0.3728205     0.9588892
# 31             gtable 0.3740351     0.9837635
# 32              withr 0.3743019     0.9885333
# 33               blob 0.3755183     0.9706946
# 34               xfun 0.3770068     0.9865467
# 35         conflicted 0.3774291     0.9639084
# 36        htmlwidgets 0.3778339     0.9746499
# 37        textshaping 0.3798847     0.9956084
# 38             dbplyr 0.3823655     0.9790646
# 39              callr 0.3834711     0.9851057
# 40            forcats 0.3859287     0.9762741

# 41          tidyverse 0.3872505     0.9898262

# 42               sass 0.3896394     0.9854426
# 43               httr 0.3906761     0.9866496
# 44            stringr 0.3941591     0.9901196
# 45         tidyselect 0.3947931     0.9894195
# 46           rappdirs 0.3950135     0.9803989
# 47             pillar 0.3952103     0.9907991
# 48              haven 0.3957767     0.9769845
# 49                cli 0.3965683     0.9939961
# 50              vctrs 0.3966454     0.9936386
# 51        viridisLite 0.3969424     0.9831916
# 52             dtplyr 0.3988317     0.9713250
# 53                zip 0.4003894     0.9605817
# 54              rvest 0.4016855     0.9755434
# 55          lubridate 0.4035768     0.9837225
# 56          iterators 0.4044103     0.9442974
# 57             tibble 0.4049913     0.9910687
# 58      aws.signature 0.4049926     0.6534924
# 59            memoise 0.4054671     0.9808084
# 60              purrr 0.4054938     0.9882463
# 61            fastmap 0.4066938     0.9828747
# 62           promises 0.4083581     0.9715518
# 63              tidyr 0.4085833     0.9872422
# 64           generics 0.4096922     0.9843632
# 65           progress 0.4101798     0.9762463
# 66              readr 0.4111426     0.9843563
# 67          rmarkdown 0.4113861     0.9862638
# 68              vroom 0.4114684     0.9837201
# 69        fontawesome 0.4126798     0.9794094
# 70             farver 0.4128044     0.9824092
# 71           evaluate 0.4149010     0.9835054
# 72        systemfonts 0.4156236     0.9880280
# 73        prettyunits 0.4162429     0.9798336
# 74              highr 0.4162562     0.9806287
# 75                 sf 0.4168294     0.9913175
# 76           processx 0.4192804     0.9859350
# 77            rematch 0.4193149     0.9714755
# 78           rematch2 0.4199238     0.9770403
# 79           ellipsis 0.4203244     0.9825807
# 80            ggplot2 0.4210920     0.9950163
# 81              dplyr 0.4216703     0.9925609
# 82             gfonts 0.4223488     0.7617728
# 83               tzdb 0.4238186     0.9801681
# 84             reprex 0.4247117     0.9690675
# 85         hrbrthemes 0.4250914     0.8823442
# 86                 DT 0.4252342     0.9428761
# 87        googledrive 0.4257256     0.9712129
# 88                hms 0.4260116     0.9834300
# 89             gargle 0.4263613     0.9736430
# 90              bslib 0.4264462     0.9859801
# 91            isoband 0.4268377     0.9830975
# 92            shinyjs 0.4270372     0.8469121
# 93    shinycssloaders 0.4279634     0.6568699
# 94           httpcode 0.4284131     0.8296504
# 95                ids 0.4285964     0.9616983
# 96                 ps 0.4292440     0.9847463
# 97               ragg 0.4293305     0.9957139
# 98         timechange 0.4311146     0.9796798
# 99               desc 0.4340066     0.9752489
# 100             rlang 0.4375411     0.9945706
# 101 leaflet.providers 0.4377074     0.7881164
# 102             golem 0.4394095     0.3858047
# 103          ggridges 0.4397193     0.8456140
# 104                wk 0.4424860     0.9163009
# 105        colorspace 0.4473095     0.9833776
# 106            httpuv 0.4485488     0.9718048
# 107         htmltools 0.4500353     0.9867978
# 108            digest 0.4513219     0.9849833
# 109           pkgload 0.4514556     0.9811904
# 110              mime 0.4532357     0.9806886
# 111           attempt 0.4534398     0.3705994
# 112              utf8 0.4559709     0.9864099
# 113          pkgbuild 0.4559807     0.9625679
# 114         htmlTable 0.4582623     0.9293506
# 115            modelr 0.4582710     0.9717726
# 116          openxlsx 0.4599463     0.9491194
# 117            scales 0.4605607     0.9880905
# 118           tinytex 0.4636353     0.9828798
# 119         pkgconfig 0.4689841     0.9837834
# 120             shiny 0.4727834     0.9772751
# 121         crosstalk 0.4732934     0.9556242
# 122   shinydisconnect 0.4736711     0.2165220
# 123            aws.s3 0.4736988     0.7598109
# 124     googlesheets4 0.4737468     0.9718734
# 125           askpass 0.4748366     0.9769871
# 126           leaflet 0.4770634     0.8385785
# 127        commonmark 0.4775118     0.9730446
# 128            cachem 0.4829646     0.9825198
# 129              curl 0.4831960     0.9872212
# 130                sp 0.4848927     0.9523964
# 131           selectr 0.4852131     0.9656547
# 132          Rttf2pt1 0.4875376     0.7747010
# 133           munsell 0.4921736     0.9803142
# 134         triebeard 0.4938414     0.8547806
# 135              yaml 0.4984808     0.9826142
# 136           Formula 0.5032897     0.9447516
# 137             knitr 0.5041108     0.9850326
# 138          urltools 0.5062885     0.8467043
# 139            RMySQL 0.5066168     0.8977127
# 140   leaflet.extras2 0.5085867     0.1633619
# 141                s2 0.5091842     0.9191394
# 142             proxy 0.5142306     0.9552442
# 143          lazyeval 0.5169554     0.9646928
# 144          jsonlite 0.5204851     0.9899916
# 145             terra 0.5249729     0.9232208
# 146           openssl 0.5255078     0.9848309
# 147       sourcetools 0.5305242     0.9588788
# 148         gridExtra 0.5313937     0.9638040
# 149               bit 0.5437076     0.9794322
# 150           stringi 0.5488193     0.9880158
# 151             bit64 0.5514020     0.9750902
# 152              uuid 0.5533494     0.9740659
# 153               sys 0.5634937     0.9807073
# 154     rhandsontable 0.5726417     0.3467383
# 155            xtable 0.5746380     0.9590082
# 156         jquerylib 0.5746489     0.9820049
# 157           shinyBS 0.5804150     0.7185408
# 158         extrafont 0.5915694     0.7852338
# 159       SearchTrees 0.5921111     0.0455283
# 160        fontquiver 0.5987202     0.7417458
# 161               png 0.6024997     0.9623171
# 162         base64enc 0.6036870     0.9798763
# 163             e1071 0.6078669     0.9622756
# 164              maps 0.6307494     0.8917164
# 165           gdtools 0.6343614     0.8341196
# 166            raster 0.6410306     0.9160555
# 167      RColorBrewer 0.6556444     0.9827809
# 168               XML 0.6644036     0.9483472
# 169             pdist 0.6844662     0.1762677
# 170             Hmisc 0.6844939     0.9869045
# 171          labeling 0.6848824     0.9805813
# 172              snow 0.6897417     0.8449909
# 173            doSNOW 0.6970885     0.5701168
# 174       extrafontdb 0.6979628     0.7303802
# 175           mapproj 0.7041503     0.7287375
# 176    fontLiberation 0.8029036     0.7365060
# 177 fontBitstreamVera 0.8029408     0.7357983
