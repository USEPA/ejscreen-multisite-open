## code to prepare `naicstable` dataset goes here
# usethis::use_data_raw("naicstable") 

# library(data.table)
# EJAM::NAICS is where NAICS was

sictable <- data.table(code = as.vector(SIC), name = names(SIC))
sictable[ , num_name := trimws(name)]
sictable[ , name := trimws(gsub(".* - ", "", name))]

sictable[ , n2 := substr(code,1,2)]
sictable[ , n3 := substr(code,1,3)]
sictable[ , n4 := substr(code,1,4)]

sictable <- sictable[ , .(code, n2, n3, n4, name, num_name)]
attr(sictable, "updated") <- "6/2023"
usethis::use_data(sictable, overwrite = TRUE)


# table(nchar(sictable$code))
# 
#    4 
#  1117 

# > sictable
# code n2  n3   n4                               name                                  num_name
# 1: 0700 07 070 0700              Agricultural services              0700 - Agricultural services
# 2: 0710 07 071 0710          Soil preparation services          0710 - Soil preparation services
# 3: 0720 07 072 0720                      Crop services                      0720 - Crop services
# 4: 0740 07 074 0740                Veterinary services                0740 - Veterinary services
# 5: 0750 07 075 0750 Animal services, except veterinary 0750 - Animal services, except veterinary
# ---                                                                                              
# 1113: 8742 87 874 8742     Management consulting services     8742 - Management consulting services
# 1114: 8743 87 874 8743          Public relations services          8743 - Public relations services
# 1115: 8744 87 874 8744        Facilities support services        8744 - Facilities support services
# 1116: 8748 87 874 8748        Business consulting, n.e.c.        8748 - Business consulting, n.e.c.
# 1117: 8900 89 890 8900                   Services, n.e.c.                   8900 - Services, n.e.c.
