
# list of services from arcgis.com 
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services"

# basic census 2020 info at each scale (st, county, tract, bg, block) with INTPTLAT / INTPTLON
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Census_2020_DHC_Total_Population/FeatureServer"
# P001_calc_pctPopDensity (type: esriFieldTypeDouble, alias: Population Density (people per square kilometer), SQL Type: sqlTypeOther, nullable: true, editable: true)
# P0020002 (type: esriFieldTypeInteger, alias: Urban population, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P0020003 (type: esriFieldTypeInteger, alias: Rural population, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P0020004 (type: esriFieldTypeInteger, alias: Population for whom urban and rural is not defined, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P002_calc_pct0002 (type: esriFieldTypeDouble, alias: Percent of population that is urban, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P002_calc_pct0003 (type: esriFieldTypeDouble, alias: Percent of population that is rural, SQL Type: sqlTypeOther, nullable: true, editable: true)

## blocks
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Census_2020_DHC_Blocks/FeatureServer/1"
## blocks points map! 
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Census_Block_Points/FeatureServer"

## blockgroup bounds
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Block_Groups/FeatureServer/0"     # has more data fields but cap of 1k query
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Census_BlockGroups/FeatureServer" # has only a few data fields but cap of 2k query and looks newer/ more documentation
## blockgroup points map
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Census_BlockGroup_Points_analysis/FeatureServer"

## tracts
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Census_Tracts/FeatureServer"
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Tracts/FeatureServer"

## counties 
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_and_States_with_PR/FeatureServer"
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Census_Counties/FeatureServer"
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer"
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_County_Boundaries/FeatureServer"
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_County_Boundaries/MapServer"

# states
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_State_Boundaries/FeatureServer"
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_States_Generalized/FeatureServer"

# zip codes
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_ZIP_Codes/FeatureServer"

##########  CENSUS 2020 DEMOGRAPHICS BASIC STATS BY BLOCK GROUP ETC.
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Census_2020_DHC_Race_and_Ethnicity/FeatureServer/layers"
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Census_2020_DHC_Race_and_Ethnicity/FeatureServer/4"  # block groups
# GEOID (type: esriFieldTypeString, alias: Geographic Identifier, SQL Type: sqlTypeOther, length: 1, nullable: true, editable: true)
##########   counts of total pop, hunits, hhlds
# P0010001 (type: esriFieldTypeInteger, alias: Total Population, SQL Type: sqlTypeOther, nullable: true, editable: true)
# H0010001 (type: esriFieldTypeInteger, alias: Total Housing Units, SQL Type: sqlTypeOther, nullable: true, editable: true)
# H0030002 (type: esriFieldTypeInteger, alias: Total Households (Occupied Housing Units), SQL Type: sqlTypeOther, nullable: true, editable: true)
##########   counts of race/ethnic subgroups
# P0090002 (type: esriFieldTypeInteger, alias: Hispanic or Latino Population, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P0090005 (type: esriFieldTypeInteger, alias: White alone, not Hispanic or Latino, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P0090006 (type: esriFieldTypeInteger, alias: Black or African American alone, not Hispanic or Latino, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P0090007 (type: esriFieldTypeInteger, alias: American Indian and Alaska Native alone, not Hispanic or Latino, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P0090008 (type: esriFieldTypeInteger, alias: Asian alone, not Hispanic or Latino, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P0090009 (type: esriFieldTypeInteger, alias: Native Hawaiian and Other Pacific Islander alone, not Hispanic or Latino, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P0090010 (type: esriFieldTypeInteger, alias: Some Other Race alone, not Hispanic or Latino, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P0090011 (type: esriFieldTypeInteger, alias: Population of two or more races, not Hispanic or Latino, SQL Type: sqlTypeOther, nullable: true, editable: true)
##########   percentages
# P009_calc_pctH (type: esriFieldTypeDouble, alias: Percent Hispanic, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P009_calc_pctWa (type: esriFieldTypeDouble, alias: Percent White alone, not Hispanic, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P009_calc_pctBa (type: esriFieldTypeDouble, alias: Percent Black or African American alone, not Hispanic, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P009_calc_pctAIANa (type: esriFieldTypeDouble, alias: Percent American Indian and Alaska Native alone, not Hispanic, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P009_calc_pctAsiana (type: esriFieldTypeDouble, alias: Percent Asian alone, not Hispanic, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P009_calc_pctNHPIa (type: esriFieldTypeDouble, alias: Percent Native Hawaiian and Other Pacific Islander alone, not Hispanic, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P009_calc_pctSORa (type: esriFieldTypeDouble, alias: Percent some other race alone, not Hispanic, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P009_calc_pctMulti (type: esriFieldTypeDouble, alias: Percent two or more races, not Hispanic, SQL Type: sqlTypeOther, nullable: true, editable: true)
##########   etc
# P009_calc_pctDI (type: esriFieldTypeDouble, alias: Census Diversity Index, SQL Type: sqlTypeOther, nullable: true, editable: true)
# P009_calc_txtPrevRank1 (type: esriFieldTypeString, alias: Most prevalent group, SQL Type: sqlTypeOther, length: 80, nullable: true, editable: true)
# P009_calc_txtPrevRank2 (type: esriFieldTypeString, alias: Second-most prevalent group, SQL Type: sqlTypeOther, length: 80, nullable: true, editable: true)


# block group basic data (bounds and demog)
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Block_Groups/FeatureServer"
# block group basic data (bounds and demog)
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/5"
# CBSA 
"https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/usa_cbsa/FeatureServer/layers"


#################### #

# https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/EJScreen_2_21_US_Percentiles_Block_Groups/FeatureServer/0
# ID is the field storing blockgroup FIPS

#################### #

# https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/5
# FIPS is the field storing blockgroup FIPS as 12 characters
# also has SQMI, POPULATION_2020, etc.

# OBJECTID (type: esriFieldTypeOID, alias: OBJECTID, SQL Type: sqlTypeOther, length: 0, nullable: false, editable: false)
# STATE_ABBR (type: esriFieldTypeString, alias: State Abbreviation, SQL Type: sqlTypeOther, length: 2, nullable: true, editable: true)
# STATE_FIPS (type: esriFieldTypeString, alias: State FIPS, SQL Type: sqlTypeOther, length: 2, nullable: true, editable: true)
# COUNTY_FIPS (type: esriFieldTypeString, alias: County FIPS, SQL Type: sqlTypeOther, length: 3, nullable: true, editable: true)
# STCOFIPS (type: esriFieldTypeString, alias: State and County FIPS, SQL Type: sqlTypeOther, length: 5, nullable: true, editable: true)
# TRACT_FIPS (type: esriFieldTypeString, alias: Tract FIPS, SQL Type: sqlTypeOther, length: 6, nullable: true, editable: true)
# BLOCKGROUP_FIPS (type: esriFieldTypeString, alias: Block Group FIPS, SQL Type: sqlTypeOther, length: 1, nullable: true, editable: true)
# FIPS (type: esriFieldTypeString, alias: FIPS Code, SQL Type: sqlTypeOther, length: 12, nullable: true, editable: true)
# POPULATION (type: esriFieldTypeInteger, alias: 2022 Total Population, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POP_SQMI (type: esriFieldTypeDouble, alias: 2022 Population per square mile, SQL Type: sqlTypeOther, nullable: true, editable: true)
# SQMI (type: esriFieldTypeDouble, alias: Area in square miles, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POPULATION_2020 (type: esriFieldTypeInteger, alias: 2020 Total Population, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POP20_SQMI (type: esriFieldTypeDouble, alias: 2020 Population per square mile, SQL Type: sqlTypeOther, nullable: true, editable: true)
# Shape__Area (type: esriFieldTypeDouble, alias: Shape__Area, SQL Type: sqlTypeDouble, nullable: true, editable: false)
# Shape__Length (type: esriFieldTypeDouble, alias: Shape__Length, SQL Type: sqlTypeDouble, nullable: true, editable: false)
# 
#################### #

# "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Block_Groups/FeatureServer/0"
# FIPS is the field storing blockgroup FIPS

# OBJECTID (type: esriFieldTypeOID, alias: OBJECTID, SQL Type: sqlTypeOther, length: 0, nullable: false, editable: false)
# STATE_FIPS (type: esriFieldTypeString, alias: State FIPS, SQL Type: sqlTypeOther, length: 2, nullable: true, editable: true)
# CNTY_FIPS (type: esriFieldTypeString, alias: CNTY_FIPS, SQL Type: sqlTypeOther, length: 3, nullable: true, editable: true)
# STCOFIPS (type: esriFieldTypeString, alias: County FIPS, SQL Type: sqlTypeOther, length: 5, nullable: true, editable: true)
# TRACT (type: esriFieldTypeString, alias: Tract, SQL Type: sqlTypeOther, length: 6, nullable: true, editable: true)
# BLKGRP (type: esriFieldTypeString, alias: Block Group, SQL Type: sqlTypeOther, length: 1, nullable: true, editable: true)
# FIPS (type: esriFieldTypeString, alias: FIPS, SQL Type: sqlTypeOther, length: 12, nullable: true, editable: true)
# POP2010 (type: esriFieldTypeInteger, alias: POP2010, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POP10_SQMI (type: esriFieldTypeDouble, alias: POP10_SQMI, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POP2012 (type: esriFieldTypeInteger, alias: POP2020, SQL Type: sqlTypeOther, nullable: true, editable: true)
# POP12_SQMI (type: esriFieldTypeDouble, alias: POP20_SQMI, SQL Type: sqlTypeOther, nullable: true, editable: true)
# WHITE (type: esriFieldTypeInteger, alias: WHITE, SQL Type: sqlTypeOther, nullable: true, editable: true)
# BLACK (type: esriFieldTypeInteger, alias: BLACK, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AMERI_ES (type: esriFieldTypeInteger, alias: AMERI_ES, SQL Type: sqlTypeOther, nullable: true, editable: true)
# ASIAN (type: esriFieldTypeInteger, alias: ASIAN, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HAWN_PI (type: esriFieldTypeInteger, alias: HAWN_PI, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HISPANIC (type: esriFieldTypeInteger, alias: HISPANIC, SQL Type: sqlTypeOther, nullable: true, editable: true)
# OTHER (type: esriFieldTypeInteger, alias: OTHER, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MULT_RACE (type: esriFieldTypeInteger, alias: MULT_RACE, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MALES (type: esriFieldTypeInteger, alias: MALES, SQL Type: sqlTypeOther, nullable: true, editable: true)
# FEMALES (type: esriFieldTypeInteger, alias: FEMALES, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_UNDER5 (type: esriFieldTypeInteger, alias: AGE_UNDER5, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_5_9 (type: esriFieldTypeInteger, alias: AGE_5_9, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_10_14 (type: esriFieldTypeInteger, alias: AGE_10_14, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_15_19 (type: esriFieldTypeInteger, alias: AGE_15_19, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_20_24 (type: esriFieldTypeInteger, alias: AGE_20_24, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_25_34 (type: esriFieldTypeInteger, alias: AGE_25_34, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_35_44 (type: esriFieldTypeInteger, alias: AGE_35_44, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_45_54 (type: esriFieldTypeInteger, alias: AGE_45_54, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_55_64 (type: esriFieldTypeInteger, alias: AGE_55_64, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_65_74 (type: esriFieldTypeInteger, alias: AGE_65_74, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_75_84 (type: esriFieldTypeInteger, alias: AGE_75_84, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AGE_85_UP (type: esriFieldTypeInteger, alias: AGE_85_UP, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MED_AGE (type: esriFieldTypeDouble, alias: MED_AGE, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MED_AGE_M (type: esriFieldTypeDouble, alias: MED_AGE_M, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MED_AGE_F (type: esriFieldTypeDouble, alias: MED_AGE_F, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HOUSEHOLDS (type: esriFieldTypeInteger, alias: HOUSEHOLDS, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AVE_HH_SZ (type: esriFieldTypeDouble, alias: AVE_HH_SZ, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HSEHLD_1_M (type: esriFieldTypeInteger, alias: HSEHLD_1_M, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HSEHLD_1_F (type: esriFieldTypeInteger, alias: HSEHLD_1_F, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MARHH_CHD (type: esriFieldTypeInteger, alias: MARHH_CHD, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MARHH_NO_C (type: esriFieldTypeInteger, alias: MARHH_NO_C, SQL Type: sqlTypeOther, nullable: true, editable: true)
# MHH_CHILD (type: esriFieldTypeInteger, alias: MHH_CHILD, SQL Type: sqlTypeOther, nullable: true, editable: true)
# FHH_CHILD (type: esriFieldTypeInteger, alias: FHH_CHILD, SQL Type: sqlTypeOther, nullable: true, editable: true)
# FAMILIES (type: esriFieldTypeInteger, alias: FAMILIES, SQL Type: sqlTypeOther, nullable: true, editable: true)
# AVE_FAM_SZ (type: esriFieldTypeDouble, alias: AVE_FAM_SZ, SQL Type: sqlTypeOther, nullable: true, editable: true)
# HSE_UNITS (type: esriFieldTypeInteger, alias: HSE_UNITS, SQL Type: sqlTypeOther, nullable: true, editable: true)
# VACANT (type: esriFieldTypeInteger, alias: VACANT, SQL Type: sqlTypeOther, nullable: true, editable: true)
# OWNER_OCC (type: esriFieldTypeInteger, alias: OWNER_OCC, SQL Type: sqlTypeOther, nullable: true, editable: true)
# RENTER_OCC (type: esriFieldTypeInteger, alias: RENTER_OCC, SQL Type: sqlTypeOther, nullable: true, editable: true)
# SQMI (type: esriFieldTypeDouble, alias: SQMI, SQL Type: sqlTypeOther, nullable: true, editable: true)
# Shape__Area (type: esriFieldTypeDouble, alias: Shape__Area, SQL Type: sqlTypeDouble, nullable: true, editable: false)
# Shape__Length (type: esriFieldTypeDouble, alias: Shape__Length, SQL Type: sqlTypeDouble, nullable: true, editable: false)
# 


########################### # ########################### # ########################### # ########################### # 

