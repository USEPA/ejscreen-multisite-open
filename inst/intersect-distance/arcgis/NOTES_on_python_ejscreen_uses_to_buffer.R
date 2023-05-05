
# Notes on key steps in the script that EJScreen used to do buffering
# based on
# ejscreenbatch.py  from 2016 (may be outdated)
# C:/ .... /contracts/ ... /early notes on prescreening sectors in EJSCREEN/EJSCREEnBatch_07142015/ejscreenbatch.py

# Also, note basic documentation of ESRI Arc spatial tools here:
# https://doc.arcgis.com/en/arcgis-online/analyze/perform-analysis-mv.htm

# def main(fc,distance,unit,ejws,outFC):
# .
# popfileds ={"ACSTOTPOP":"total population (ACS2012)",\
#   "ACSIPOVBAS":"Total Population for whom Poverty Status is Determined",\
#   "ACSEDUCBAS":"Total Population Age 25 up",\
#   "ACSTOTHH":"Total Households",\
#   "PRE1960":"Houses Built Pre 1960"\
# }
# .
# arcpy.MakeFeatureLayer_management('EJScreen_Full', 'ej_lyr')   #######  full EJScreen dataset of indicators ####
# with arcpy.da.SearchCursor(fc, ["SHAPE@"]) as cursor: 
# .
# for rec in cursor:  ##### each buffer (each site) 
#   .
#   buff = distance + " " + unit                         ### radius
#   poly = arcpy.Buffer_analysis(geom, temPol, buff)[0]  ### buffering creates poly using buff radius around geom?####
#   bgweightobj = calBGweight(poly)   ################ get blockgroup weight(s) for this buffer via calBGweight() that gives sum of block wts; ####
#        ### POP_WEIGHT12 via 'groupByFieldsForStatistics':'STCNTRBG12','
#   .
#   arcpy.SelectLayerByLocation_management('ej_lyr', "INTERSECT", poly, '', "NEW_SELECTION")   ### join EJScreen data to buffer?####
#   .
#   arcpy.AddMessage("Feature #" + str(feaCount) + ": " + str(fcount) + " BGs")   ### note which buffer number and how many blockgroups in it####
#   feaCount += 1
#   .
#   arr = arcpy.da.FeatureClassToNumPyArray("ej_lyr",['*'])
#   for pf in popfileds:
#     tpop =calPop(arr, bgweightobj,pf)
#     .
#   for fld in ejdataobj:
#   .
#   ejdataobj[fld]["raw_value"] =  weight_Avg(arr, fld,bgweightobj)
#   .
#   calPercentile(stabbr,regnum)
#   populateFields_2(rows,geom,stabbr,stname,regnum,popList)
# 
# arcpy.AddMessage("Processed completed.")  
#   
# 
# 
#   # calc BG WTD **AVERAGE** OF SCORES IN BUFFER ####
#   
#   def        weight_Avg(    fsetw,    att,    bgweightobj):
#     .
#   popfieldname = ejdataobj[att]["denominator"]
#   if popfieldname is None:
#     popfieldname = "ACSTOTPOP"
#   .
#   for r in fsetw:
#     
#       popvalue =r[popfieldname]
#       .
#       bgpop = bgwt * popvalue
#       popsum = popsum + bgpop
#       .
#       vsum = vsum + fieldvalue * bgpop
#   .
#   finalvalue = vsum / popsum
#   .
#   return finalvalue
#   
#   
#   
#   # calc BG WTD **SUM OF COUNTS** IN BUFFER ####
#   
#   def      calPop(    fsetw,    bgweightobj,   popfieldname):
#   .
#   bgwt = bgweightobj[bgnum]
#   .
#       popvalue =r[popfieldname]
#       .
#       bgpop = bgwt * popvalue
#       popsum = popsum + bgpop
#   .
#   return round(popsum,0)
#   
#   
