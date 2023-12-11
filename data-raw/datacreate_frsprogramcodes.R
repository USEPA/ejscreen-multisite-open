## code to prepare `frsprogramcodes` dataset goes here

# Key Media programs ECHO uses to limit queries, and code found in frs dataset for that program:
#   
# 1 NPDES (ICIS-NPDES) = NPDES
# 2 The Integrated Compliance Information System (ICIS) for Air (ICIS-Air)  =  AIR ? OR AIRS/AFS??
# 3 Resource Conservation and Recovery Act (RCRA) = RCRAINFO ? https://www.epa.gov/enviro/rcrainfo-overview https://enviro.epa.gov/envirofacts/rcrainfo/search 
# 4 Risk Management Plan (RMP) Rule = ???
# 5 Safe Drinking Water Act = SFDW  
# 6 Superfund Enterprise Management System = SEMS
# 7 Clean Air Markets Division Business System = CAMDBS
# 8 Toxics Release Inventory Program = TRIS
# 9 Greenhouse Gas Reporting Program = E-GGRT
# 10 Emissions Inventory System = EIS
# 11 Toxic Substances Control Act = TSCA

frsprogramcodes <- data.frame(
  description = c(
    "National Pollutant Discharge Elimination System (NPDES) (ICIS-NPDES)",
    "The Integrated Compliance Information System (ICIS) for Air (ICIS-Air)",
    "The Resource Conservation and Recovery Act (RCRA) Information System",
    "Risk Management Plan (RMP) facilities",
    "The Safe Drinking Water Information System (SDWIS)",
    "The Superfund Enterprise Management System",
    "Clean Air Markets Division Business System",
   "Toxics Release Inventory Program",
   "Greenhouse Gas Reporting Program",
   "Emissions Inventory System",
   "Toxic Substances Control Act"
  ),
  code = c(
    "NPDES", "AIR", "RCRAINFO", "RMP", "SFDW", "SEMS", "CAMDBS", "TRIS", "E-GGRT", "EIS", "TSCA"
  )
)



usethis::use_data(frsprogramcodes, overwrite = TRUE)


# frs_by_programid[program %in% frsprogramcodes$code, .N, by=program]
#     program      N
# 1:     TRIS  32500
# 2:      AIR 134190
# 3:     SFDW  44844
# 4:      EIS 121468
# 5:   E-GGRT   6388
# 6:   CAMDBS    748
# 7:     TSCA  12755
# 8: RCRAINFO 518360
# 9:     SEMS  11044
# 10:    NPDES 382353

