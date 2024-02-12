# Script to replace variables in html template from EJScreen with R code in template, ui, and server 
#  that will insert results where appropriate

# First, got html of EJScreen community report template
#  and used regex in RStudio find/replace to replace each element like <span id="RAW_E_PM25"  >NA</span>
#  with something like {{RAW_E_PM25_ht}} which is what an html template uses to insert values into the template.

##################################################################################################################### # 
# Then used regex in RStudio to create this cleaned list of variables:
{
  vars <- c(
# "TOTALPOP",     # this was changed manually to say something different on the ejam version of the report
# "LOCATIONSTR",  # ditto

"inputAreaMiles", # not used on ejam version
"RAW_E_PM25", 
"S_E_PM25", 
"S_E_PM25_PER", 
"N_E_PM25", 
"N_E_PM25_PER", 
"RAW_E_O3", 
"S_E_O3", 
"S_E_O3_PER", 
"N_E_O3", 
"N_E_O3_PER", 
"RAW_E_DIESEL", 
"S_E_DIESEL", 
"S_E_DIESEL_PER", 
"N_E_DIESEL", 
"N_E_DIESEL_PER", 
"RAW_E_CANCER", 
"S_E_CANCER", 
"S_E_CANCER_PER", 
"N_E_CANCER", 
"N_E_CANCER_PER", 
"RAW_E_RESP", 
"S_E_RESP", 
"S_E_RESP_PER", 
"N_E_RESP", 
"N_E_RESP_PER", 
"RAW_E_RSEI_AIR", 
"S_E_RSEI_AIR", 
"S_E_RSEI_AIR_PER", 
"N_E_RSEI_AIR", 
"N_E_RSEI_AIR_PER", 
"RAW_E_TRAFFIC", 
"S_E_TRAFFIC", 
"S_E_TRAFFIC_PER", 
"N_E_TRAFFIC", 
"N_E_TRAFFIC_PER", 
"RAW_E_LEAD", 
"S_E_LEAD", 
"S_E_LEAD_PER", 
"N_E_LEAD", 
"N_E_LEAD_PER", 
"RAW_E_NPL", 
"S_E_NPL", 
"S_E_NPL_PER", 
"N_E_NPL", 
"N_E_NPL_PER", 
"RAW_E_RMP", 
"S_E_RMP", 
"S_E_RMP_PER", 
"N_E_RMP", 
"N_E_RMP_PER", 
"RAW_E_TSDF", 
"S_E_TSDF", 
"S_E_TSDF_PER", 
"N_E_TSDF", 
"N_E_TSDF_PER", 
"RAW_E_UST", 
"S_E_UST", 
"S_E_UST_PER", 
"N_E_UST", 
"N_E_UST_PER", 
"RAW_E_NPDES", 
"S_E_NPDES", 
"S_E_NPDES_PER", 
"N_E_NPDES", 
"N_E_NPDES_PER", 
"RAW_D_DEMOGIDX2", 
"S_D_DEMOGIDX2", 
"S_D_DEMOGIDX2_PER", 
"N_D_DEMOGIDX2", 
"N_D_DEMOGIDX2_PER", 
"RAW_D_DEMOGIDX5", 
"S_D_DEMOGIDX5", 
"S_D_DEMOGIDX5_PER", 
"N_D_DEMOGIDX5", 
"N_D_DEMOGIDX5_PER", 
"RAW_D_PEOPCOLOR", 
"S_D_PEOPCOLOR", 
"S_D_PEOPCOLOR_PER", 
"N_D_PEOPCOLOR", 
"N_D_PEOPCOLOR_PER", 
"RAW_D_INCOME", 
"S_D_INCOME", 
"S_D_INCOME_PER", 
"N_D_INCOME", 
"N_D_INCOME_PER", 
"RAW_D_UNEMPLOYED", 
"S_D_UNEMPLOYED", 
"S_D_UNEMPLOYED_PER", 
"N_D_UNEMPLOYED", 
"N_D_UNEMPLOYED_PER", 
"RAW_D_LING", 
"S_D_LING", 
"S_D_LING_PER", 
"N_D_LING", 
"N_D_LING_PER", 
"RAW_D_LESSHS", 
"S_D_LESSHS", 
"S_D_LESSHS_PER", 
"N_D_LESSHS", 
"N_D_LESSHS_PER", 
"RAW_D_UNDER5", 
"S_D_UNDER5", 
"S_D_UNDER5_PER", 
"N_D_UNDER5", 
"N_D_UNDER5_PER", 
"RAW_D_OVER64", 
"S_D_OVER64", 
"S_D_OVER64_PER", 
"N_D_OVER64", 
"N_D_OVER64_PER", 
"RAW_D_LIFEEXP", 
"S_D_LIFEEXP", 
"S_D_LIFEEXP_PER", 
"N_D_LIFEEXP", 
"N_D_LIFEEXP_PER", 
"NUM_NPL", 
"NUM_TSDF", 
"NUM_WATERDIS", 
"NUM_AIRPOLL", 
"NUM_BROWNFIELD", 
"NUM_TRI", 
"NUM_SCHOOL", 
"NUM_HOSPITAL", 
"NUM_CHURCH", 
"YESNO_AIRNONATT", 
"YESNO_IMPWATERS", 
  "YESNO_TRIBAL",
"YESNO_CEJSTDIS", 
"YESNO_IRADIS"
  )
  }  # long list of variables as named in EJScreen website code

##################################################################################################################### # 
rnames <- fixcolnames(vars, 'api', 'r')
# DISABLE THESE UNTIL AVAILABLE IN EJAM:
setdiff(rnames, names(EJAM::testoutput_ejamit_10pts_1miles$results_overall))
# "inputAreaMiles"  "count.NPL"       "count.TSDF"     
# [6] "num_waterdis"    "num_airpoll"     "num_brownfield"  "num_tri"         "num_school"     
# [11] "num_hospital"    "num_church"      "yesno_airnonatt"
# "yesno_impwaters" "yesno_cejstdis" 
# [16] "yesno_iradis"   

##################################################################################################################### # 
# for UI 
for_htmlTemplate <- function(apiname) {
  spacer <- sapply(apiname, function(thisone) paste0(rep(" ", max(nchar(apiname)) - nchar(thisone)), collapse = ""))
  paste0(apiname, "_ht ", spacer, "= textOutput('", apiname,"_out', ", spacer, "inline = TRUE)")
  }
cat(paste(for_htmlTemplate(vars), collapse = ",\n")) ; cat("\n\n")
# and copied results from console pane into actual code
# e.g., # S_E_PM25_ht = textOutput('S_E_PM25_out', inline = TRUE),

##################################################################################################################### # 
# for SERVER
for_server <- function(apiname) {
  rnames <- fixcolnames(vars, 'api', 'r')
  spacer <- sapply(apiname, function(thisone) paste0(rep(" ", max(nchar(apiname)) - nchar(thisone)), collapse = ""))
  paste0("output$", apiname, "_out ", spacer, "<- renderText(overall$", rnames, ")")
}
cat(paste(for_server(vars), collapse = "\n")) ; cat("\n\n")
# and copied results from console pane into actual code
# e.g.
# output$TOTALPOP_out <- renderText(prettyNum(round(ejam_output()$results_overall$pop,0), big.mark = ","))

##################################################################################################################### # 

# then copied from console pane after cat() and PASTED this INTO THE UI CODE  (as uncommented text) : 
{
  # inputAreaMiles_ht     = textOutput('inputAreaMiles_out',     inline = TRUE),
  # RAW_E_PM25_ht         = textOutput('RAW_E_PM25_out',         inline = TRUE),
  # S_E_PM25_ht           = textOutput('S_E_PM25_out',           inline = TRUE),
  # S_E_PM25_PER_ht       = textOutput('S_E_PM25_PER_out',       inline = TRUE),
  # N_E_PM25_ht           = textOutput('N_E_PM25_out',           inline = TRUE),
  # N_E_PM25_PER_ht       = textOutput('N_E_PM25_PER_out',       inline = TRUE),
  # RAW_E_O3_ht           = textOutput('RAW_E_O3_out',           inline = TRUE),
  # S_E_O3_ht             = textOutput('S_E_O3_out',             inline = TRUE),
  # S_E_O3_PER_ht         = textOutput('S_E_O3_PER_out',         inline = TRUE),
  # N_E_O3_ht             = textOutput('N_E_O3_out',             inline = TRUE),
  # N_E_O3_PER_ht         = textOutput('N_E_O3_PER_out',         inline = TRUE),
  # RAW_E_DIESEL_ht       = textOutput('RAW_E_DIESEL_out',       inline = TRUE),
  # S_E_DIESEL_ht         = textOutput('S_E_DIESEL_out',         inline = TRUE),
  # S_E_DIESEL_PER_ht     = textOutput('S_E_DIESEL_PER_out',     inline = TRUE),
  # N_E_DIESEL_ht         = textOutput('N_E_DIESEL_out',         inline = TRUE),
  # N_E_DIESEL_PER_ht     = textOutput('N_E_DIESEL_PER_out',     inline = TRUE),
  # RAW_E_CANCER_ht       = textOutput('RAW_E_CANCER_out',       inline = TRUE),
  # S_E_CANCER_ht         = textOutput('S_E_CANCER_out',         inline = TRUE),
  # S_E_CANCER_PER_ht     = textOutput('S_E_CANCER_PER_out',     inline = TRUE),
  # N_E_CANCER_ht         = textOutput('N_E_CANCER_out',         inline = TRUE),
  # N_E_CANCER_PER_ht     = textOutput('N_E_CANCER_PER_out',     inline = TRUE),
  # RAW_E_RESP_ht         = textOutput('RAW_E_RESP_out',         inline = TRUE),
  # S_E_RESP_ht           = textOutput('S_E_RESP_out',           inline = TRUE),
  # S_E_RESP_PER_ht       = textOutput('S_E_RESP_PER_out',       inline = TRUE),
  # N_E_RESP_ht           = textOutput('N_E_RESP_out',           inline = TRUE),
  # N_E_RESP_PER_ht       = textOutput('N_E_RESP_PER_out',       inline = TRUE),
  # RAW_E_RSEI_AIR_ht     = textOutput('RAW_E_RSEI_AIR_out',     inline = TRUE),
  # S_E_RSEI_AIR_ht       = textOutput('S_E_RSEI_AIR_out',       inline = TRUE),
  # S_E_RSEI_AIR_PER_ht   = textOutput('S_E_RSEI_AIR_PER_out',   inline = TRUE),
  # N_E_RSEI_AIR_ht       = textOutput('N_E_RSEI_AIR_out',       inline = TRUE),
  # N_E_RSEI_AIR_PER_ht   = textOutput('N_E_RSEI_AIR_PER_out',   inline = TRUE),
  # RAW_E_TRAFFIC_ht      = textOutput('RAW_E_TRAFFIC_out',      inline = TRUE),
  # S_E_TRAFFIC_ht        = textOutput('S_E_TRAFFIC_out',        inline = TRUE),
  # S_E_TRAFFIC_PER_ht    = textOutput('S_E_TRAFFIC_PER_out',    inline = TRUE),
  # N_E_TRAFFIC_ht        = textOutput('N_E_TRAFFIC_out',        inline = TRUE),
  # N_E_TRAFFIC_PER_ht    = textOutput('N_E_TRAFFIC_PER_out',    inline = TRUE),
  # RAW_E_LEAD_ht         = textOutput('RAW_E_LEAD_out',         inline = TRUE),
  # S_E_LEAD_ht           = textOutput('S_E_LEAD_out',           inline = TRUE),
  # S_E_LEAD_PER_ht       = textOutput('S_E_LEAD_PER_out',       inline = TRUE),
  # N_E_LEAD_ht           = textOutput('N_E_LEAD_out',           inline = TRUE),
  # N_E_LEAD_PER_ht       = textOutput('N_E_LEAD_PER_out',       inline = TRUE),
  # RAW_E_NPL_ht          = textOutput('RAW_E_NPL_out',          inline = TRUE),
  # S_E_NPL_ht            = textOutput('S_E_NPL_out',            inline = TRUE),
  # S_E_NPL_PER_ht        = textOutput('S_E_NPL_PER_out',        inline = TRUE),
  # N_E_NPL_ht            = textOutput('N_E_NPL_out',            inline = TRUE),
  # N_E_NPL_PER_ht        = textOutput('N_E_NPL_PER_out',        inline = TRUE),
  # RAW_E_RMP_ht          = textOutput('RAW_E_RMP_out',          inline = TRUE),
  # S_E_RMP_ht            = textOutput('S_E_RMP_out',            inline = TRUE),
  # S_E_RMP_PER_ht        = textOutput('S_E_RMP_PER_out',        inline = TRUE),
  # N_E_RMP_ht            = textOutput('N_E_RMP_out',            inline = TRUE),
  # N_E_RMP_PER_ht        = textOutput('N_E_RMP_PER_out',        inline = TRUE),
  # RAW_E_TSDF_ht         = textOutput('RAW_E_TSDF_out',         inline = TRUE),
  # S_E_TSDF_ht           = textOutput('S_E_TSDF_out',           inline = TRUE),
  # S_E_TSDF_PER_ht       = textOutput('S_E_TSDF_PER_out',       inline = TRUE),
  # N_E_TSDF_ht           = textOutput('N_E_TSDF_out',           inline = TRUE),
  # N_E_TSDF_PER_ht       = textOutput('N_E_TSDF_PER_out',       inline = TRUE),
  # RAW_E_UST_ht          = textOutput('RAW_E_UST_out',          inline = TRUE),
  # S_E_UST_ht            = textOutput('S_E_UST_out',            inline = TRUE),
  # S_E_UST_PER_ht        = textOutput('S_E_UST_PER_out',        inline = TRUE),
  # N_E_UST_ht            = textOutput('N_E_UST_out',            inline = TRUE),
  # N_E_UST_PER_ht        = textOutput('N_E_UST_PER_out',        inline = TRUE),
  # RAW_E_NPDES_ht        = textOutput('RAW_E_NPDES_out',        inline = TRUE),
  # S_E_NPDES_ht          = textOutput('S_E_NPDES_out',          inline = TRUE),
  # S_E_NPDES_PER_ht      = textOutput('S_E_NPDES_PER_out',      inline = TRUE),
  # N_E_NPDES_ht          = textOutput('N_E_NPDES_out',          inline = TRUE),
  # N_E_NPDES_PER_ht      = textOutput('N_E_NPDES_PER_out',      inline = TRUE),
  # RAW_D_DEMOGIDX2_ht    = textOutput('RAW_D_DEMOGIDX2_out',    inline = TRUE),
  # S_D_DEMOGIDX2_ht      = textOutput('S_D_DEMOGIDX2_out',      inline = TRUE),
  # S_D_DEMOGIDX2_PER_ht  = textOutput('S_D_DEMOGIDX2_PER_out',  inline = TRUE),
  # N_D_DEMOGIDX2_ht      = textOutput('N_D_DEMOGIDX2_out',      inline = TRUE),
  # N_D_DEMOGIDX2_PER_ht  = textOutput('N_D_DEMOGIDX2_PER_out',  inline = TRUE),
  # RAW_D_DEMOGIDX5_ht    = textOutput('RAW_D_DEMOGIDX5_out',    inline = TRUE),
  # S_D_DEMOGIDX5_ht      = textOutput('S_D_DEMOGIDX5_out',      inline = TRUE),
  # S_D_DEMOGIDX5_PER_ht  = textOutput('S_D_DEMOGIDX5_PER_out',  inline = TRUE),
  # N_D_DEMOGIDX5_ht      = textOutput('N_D_DEMOGIDX5_out',      inline = TRUE),
  # N_D_DEMOGIDX5_PER_ht  = textOutput('N_D_DEMOGIDX5_PER_out',  inline = TRUE),
  # RAW_D_PEOPCOLOR_ht    = textOutput('RAW_D_PEOPCOLOR_out',    inline = TRUE),
  # S_D_PEOPCOLOR_ht      = textOutput('S_D_PEOPCOLOR_out',      inline = TRUE),
  # S_D_PEOPCOLOR_PER_ht  = textOutput('S_D_PEOPCOLOR_PER_out',  inline = TRUE),
  # N_D_PEOPCOLOR_ht      = textOutput('N_D_PEOPCOLOR_out',      inline = TRUE),
  # N_D_PEOPCOLOR_PER_ht  = textOutput('N_D_PEOPCOLOR_PER_out',  inline = TRUE),
  # RAW_D_INCOME_ht       = textOutput('RAW_D_INCOME_out',       inline = TRUE),
  # S_D_INCOME_ht         = textOutput('S_D_INCOME_out',         inline = TRUE),
  # S_D_INCOME_PER_ht     = textOutput('S_D_INCOME_PER_out',     inline = TRUE),
  # N_D_INCOME_ht         = textOutput('N_D_INCOME_out',         inline = TRUE),
  # N_D_INCOME_PER_ht     = textOutput('N_D_INCOME_PER_out',     inline = TRUE),
  # RAW_D_UNEMPLOYED_ht   = textOutput('RAW_D_UNEMPLOYED_out',   inline = TRUE),
  # S_D_UNEMPLOYED_ht     = textOutput('S_D_UNEMPLOYED_out',     inline = TRUE),
  # S_D_UNEMPLOYED_PER_ht = textOutput('S_D_UNEMPLOYED_PER_out', inline = TRUE),
  # N_D_UNEMPLOYED_ht     = textOutput('N_D_UNEMPLOYED_out',     inline = TRUE),
  # N_D_UNEMPLOYED_PER_ht = textOutput('N_D_UNEMPLOYED_PER_out', inline = TRUE),
  # RAW_D_LING_ht         = textOutput('RAW_D_LING_out',         inline = TRUE),
  # S_D_LING_ht           = textOutput('S_D_LING_out',           inline = TRUE),
  # S_D_LING_PER_ht       = textOutput('S_D_LING_PER_out',       inline = TRUE),
  # N_D_LING_ht           = textOutput('N_D_LING_out',           inline = TRUE),
  # N_D_LING_PER_ht       = textOutput('N_D_LING_PER_out',       inline = TRUE),
  # RAW_D_LESSHS_ht       = textOutput('RAW_D_LESSHS_out',       inline = TRUE),
  # S_D_LESSHS_ht         = textOutput('S_D_LESSHS_out',         inline = TRUE),
  # S_D_LESSHS_PER_ht     = textOutput('S_D_LESSHS_PER_out',     inline = TRUE),
  # N_D_LESSHS_ht         = textOutput('N_D_LESSHS_out',         inline = TRUE),
  # N_D_LESSHS_PER_ht     = textOutput('N_D_LESSHS_PER_out',     inline = TRUE),
  # RAW_D_UNDER5_ht       = textOutput('RAW_D_UNDER5_out',       inline = TRUE),
  # S_D_UNDER5_ht         = textOutput('S_D_UNDER5_out',         inline = TRUE),
  # S_D_UNDER5_PER_ht     = textOutput('S_D_UNDER5_PER_out',     inline = TRUE),
  # N_D_UNDER5_ht         = textOutput('N_D_UNDER5_out',         inline = TRUE),
  # N_D_UNDER5_PER_ht     = textOutput('N_D_UNDER5_PER_out',     inline = TRUE),
  # RAW_D_OVER64_ht       = textOutput('RAW_D_OVER64_out',       inline = TRUE),
  # S_D_OVER64_ht         = textOutput('S_D_OVER64_out',         inline = TRUE),
  # S_D_OVER64_PER_ht     = textOutput('S_D_OVER64_PER_out',     inline = TRUE),
  # N_D_OVER64_ht         = textOutput('N_D_OVER64_out',         inline = TRUE),
  # N_D_OVER64_PER_ht     = textOutput('N_D_OVER64_PER_out',     inline = TRUE),
  # RAW_D_LIFEEXP_ht      = textOutput('RAW_D_LIFEEXP_out',      inline = TRUE),
  # S_D_LIFEEXP_ht        = textOutput('S_D_LIFEEXP_out',        inline = TRUE),
  # S_D_LIFEEXP_PER_ht    = textOutput('S_D_LIFEEXP_PER_out',    inline = TRUE),
  # N_D_LIFEEXP_ht        = textOutput('N_D_LIFEEXP_out',        inline = TRUE),
  # N_D_LIFEEXP_PER_ht    = textOutput('N_D_LIFEEXP_PER_out',    inline = TRUE),
  # NUM_NPL_ht            = textOutput('NUM_NPL_out',            inline = TRUE),
  # NUM_TSDF_ht           = textOutput('NUM_TSDF_out',           inline = TRUE),
  # NUM_WATERDIS_ht       = textOutput('NUM_WATERDIS_out',       inline = TRUE),
  # NUM_AIRPOLL_ht        = textOutput('NUM_AIRPOLL_out',        inline = TRUE),
  # NUM_BROWNFIELD_ht     = textOutput('NUM_BROWNFIELD_out',     inline = TRUE),
  # NUM_TRI_ht            = textOutput('NUM_TRI_out',            inline = TRUE),
  # NUM_SCHOOL_ht         = textOutput('NUM_SCHOOL_out',         inline = TRUE),
  # NUM_HOSPITAL_ht       = textOutput('NUM_HOSPITAL_out',       inline = TRUE),
  # NUM_CHURCH_ht         = textOutput('NUM_CHURCH_out',         inline = TRUE),
  # YESNO_AIRNONATT_ht    = textOutput('YESNO_AIRNONATT_out',    inline = TRUE),
  # YESNO_IMPWATERS_ht    = textOutput('YESNO_IMPWATERS_out',    inline = TRUE),
  # YESNO_TRIBAL_ht       = textOutput('YESNO_TRIBAL_out',       inline = TRUE),
  # YESNO_CEJSTDIS_ht     = textOutput('YESNO_CEJSTDIS_out',     inline = TRUE),
  # YESNO_IRADIS_ht       = textOutput('YESNO_IRADIS_out',       inline = TRUE)


}

# then copied from console pane after cat() and PASTED this INTO THE SERVER CODE  (as uncommented text) :
{
 ### # output$TOTALPOP_out <- renderText(overall$TOTALPOP)
 ### # output$LOCATIONSTR_out <- renderText(overall$LOCATIONSTR)
  
  # output$inputAreaMiles_out     <- renderText(overall$inputAreaMiles)
  # output$RAW_E_PM25_out         <- renderText(overall$pm)
  # output$S_E_PM25_out           <- renderText(overall$state.avg.pm)
  # output$S_E_PM25_PER_out       <- renderText(overall$state.pctile.pm)
  # output$N_E_PM25_out           <- renderText(overall$avg.pm)
  # output$N_E_PM25_PER_out       <- renderText(overall$pctile.pm)
  # output$RAW_E_O3_out           <- renderText(overall$o3)
  # output$S_E_O3_out             <- renderText(overall$state.avg.o3)
  # output$S_E_O3_PER_out         <- renderText(overall$state.pctile.o3)
  # output$N_E_O3_out             <- renderText(overall$avg.o3)
  # output$N_E_O3_PER_out         <- renderText(overall$pctile.o3)
  # output$RAW_E_DIESEL_out       <- renderText(overall$dpm)
  # output$S_E_DIESEL_out         <- renderText(overall$state.avg.dpm)
  # output$S_E_DIESEL_PER_out     <- renderText(overall$state.pctile.dpm)
  # output$N_E_DIESEL_out         <- renderText(overall$avg.dpm)
  # output$N_E_DIESEL_PER_out     <- renderText(overall$pctile.dpm)
  # output$RAW_E_CANCER_out       <- renderText(overall$cancer)
  # output$S_E_CANCER_out         <- renderText(overall$state.avg.cancer)
  # output$S_E_CANCER_PER_out     <- renderText(overall$state.pctile.cancer)
  # output$N_E_CANCER_out         <- renderText(overall$avg.cancer)
  # output$N_E_CANCER_PER_out     <- renderText(overall$pctile.cancer)
  # output$RAW_E_RESP_out         <- renderText(overall$resp)
  # output$S_E_RESP_out           <- renderText(overall$state.avg.resp)
  # output$S_E_RESP_PER_out       <- renderText(overall$state.pctile.resp)
  # output$N_E_RESP_out           <- renderText(overall$avg.resp)
  # output$N_E_RESP_PER_out       <- renderText(overall$pctile.resp)
  # output$RAW_E_RSEI_AIR_out     <- renderText(overall$rsei)
  # output$S_E_RSEI_AIR_out       <- renderText(overall$state.avg.rsei)
  # output$S_E_RSEI_AIR_PER_out   <- renderText(overall$state.pctile.rsei)
  # output$N_E_RSEI_AIR_out       <- renderText(overall$avg.rsei)
  # output$N_E_RSEI_AIR_PER_out   <- renderText(overall$pctile.rsei)
  # output$RAW_E_TRAFFIC_out      <- renderText(overall$traffic.score)
  # output$S_E_TRAFFIC_out        <- renderText(overall$state.avg.traffic.score)
  # output$S_E_TRAFFIC_PER_out    <- renderText(overall$state.pctile.traffic.score)
  # output$N_E_TRAFFIC_out        <- renderText(overall$avg.traffic.score)
  # output$N_E_TRAFFIC_PER_out    <- renderText(overall$pctile.traffic.score)
  # output$RAW_E_LEAD_out         <- renderText(overall$pctpre1960)
  # output$S_E_LEAD_out           <- renderText(overall$state.avg.pctpre1960)
  # output$S_E_LEAD_PER_out       <- renderText(overall$state.pctile.pctpre1960)
  # output$N_E_LEAD_out           <- renderText(overall$avg.pctpre1960)
  # output$N_E_LEAD_PER_out       <- renderText(overall$pctile.pctpre1960)
  # output$RAW_E_NPL_out          <- renderText(overall$proximity.npl)
  # output$S_E_NPL_out            <- renderText(overall$state.avg.proximity.npl)
  # output$S_E_NPL_PER_out        <- renderText(overall$state.pctile.proximity.npl)
  # output$N_E_NPL_out            <- renderText(overall$avg.proximity.npl)
  # output$N_E_NPL_PER_out        <- renderText(overall$pctile.proximity.npl)
  # output$RAW_E_RMP_out          <- renderText(overall$proximity.rmp)
  # output$S_E_RMP_out            <- renderText(overall$state.avg.proximity.rmp)
  # output$S_E_RMP_PER_out        <- renderText(overall$state.pctile.proximity.rmp)
  # output$N_E_RMP_out            <- renderText(overall$avg.proximity.rmp)
  # output$N_E_RMP_PER_out        <- renderText(overall$pctile.proximity.rmp)
  # output$RAW_E_TSDF_out         <- renderText(overall$proximity.tsdf)
  # output$S_E_TSDF_out           <- renderText(overall$state.avg.proximity.tsdf)
  # output$S_E_TSDF_PER_out       <- renderText(overall$state.pctile.proximity.tsdf)
  # output$N_E_TSDF_out           <- renderText(overall$avg.proximity.tsdf)
  # output$N_E_TSDF_PER_out       <- renderText(overall$pctile.proximity.tsdf)
  # output$RAW_E_UST_out          <- renderText(overall$ust)
  # output$S_E_UST_out            <- renderText(overall$state.avg.ust)
  # output$S_E_UST_PER_out        <- renderText(overall$state.pctile.ust)
  # output$N_E_UST_out            <- renderText(overall$avg.ust)
  # output$N_E_UST_PER_out        <- renderText(overall$pctile.ust)
  # output$RAW_E_NPDES_out        <- renderText(overall$proximity.npdes)
  # output$S_E_NPDES_out          <- renderText(overall$state.avg.proximity.npdes)
  # output$S_E_NPDES_PER_out      <- renderText(overall$state.pctile.proximity.npdes)
  # output$N_E_NPDES_out          <- renderText(overall$avg.proximity.npdes)
  # output$N_E_NPDES_PER_out      <- renderText(overall$pctile.proximity.npdes)
  # output$RAW_D_DEMOGIDX2_out    <- renderText(overall$Demog.Index)
  # output$S_D_DEMOGIDX2_out      <- renderText(overall$state.avg.Demog.Index)
  # output$S_D_DEMOGIDX2_PER_out  <- renderText(overall$state.pctile.Demog.Index)
  # output$N_D_DEMOGIDX2_out      <- renderText(overall$avg.Demog.Index)
  # output$N_D_DEMOGIDX2_PER_out  <- renderText(overall$pctile.Demog.Index)
  # output$RAW_D_DEMOGIDX5_out    <- renderText(overall$Demog.Index.Supp)
  # output$S_D_DEMOGIDX5_out      <- renderText(overall$state.avg.Demog.Index.Supp)
  # output$S_D_DEMOGIDX5_PER_out  <- renderText(overall$state.pctile.Demog.Index.Supp)
  # output$N_D_DEMOGIDX5_out      <- renderText(overall$avg.Demog.Index.Supp)
  # output$N_D_DEMOGIDX5_PER_out  <- renderText(overall$pctile.Demog.Index.Supp)
  # output$RAW_D_PEOPCOLOR_out    <- renderText(overall$pctmin)
  # output$S_D_PEOPCOLOR_out      <- renderText(overall$state.avg.pctmin)
  # output$S_D_PEOPCOLOR_PER_out  <- renderText(overall$state.pctile.pctmin)
  # output$N_D_PEOPCOLOR_out      <- renderText(overall$avg.pctmin)
  # output$N_D_PEOPCOLOR_PER_out  <- renderText(overall$pctile.pctmin)
  # output$RAW_D_INCOME_out       <- renderText(overall$pctlowinc)
  # output$S_D_INCOME_out         <- renderText(overall$state.avg.pctlowinc)
  # output$S_D_INCOME_PER_out     <- renderText(overall$state.pctile.pctlowinc)
  # output$N_D_INCOME_out         <- renderText(overall$avg.pctlowinc)
  # output$N_D_INCOME_PER_out     <- renderText(overall$pctile.pctlowinc)
  # output$RAW_D_UNEMPLOYED_out   <- renderText(overall$pctunemployed)
  # output$S_D_UNEMPLOYED_out     <- renderText(overall$state.avg.pctunemployed)
  # output$S_D_UNEMPLOYED_PER_out <- renderText(overall$state.pctile.pctunemployed)
  # output$N_D_UNEMPLOYED_out     <- renderText(overall$avg.pctunemployed)
  # output$N_D_UNEMPLOYED_PER_out <- renderText(overall$pctile.pctunemployed)
  # output$RAW_D_LING_out         <- renderText(overall$pctlingiso)
  # output$S_D_LING_out           <- renderText(overall$state.avg.pctlingiso)
  # output$S_D_LING_PER_out       <- renderText(overall$state.pctile.pctlingiso)
  # output$N_D_LING_out           <- renderText(overall$avg.pctlingiso)
  # output$N_D_LING_PER_out       <- renderText(overall$pctile.pctlingiso)
  # output$RAW_D_LESSHS_out       <- renderText(overall$pctlths)
  # output$S_D_LESSHS_out         <- renderText(overall$state.avg.pctlths)
  # output$S_D_LESSHS_PER_out     <- renderText(overall$state.pctile.pctlths)
  # output$N_D_LESSHS_out         <- renderText(overall$avg.pctlths)
  # output$N_D_LESSHS_PER_out     <- renderText(overall$pctile.pctlths)
  # output$RAW_D_UNDER5_out       <- renderText(overall$pctunder5)
  # output$S_D_UNDER5_out         <- renderText(overall$state.avg.pctunder5)
  # output$S_D_UNDER5_PER_out     <- renderText(overall$state.pctile.pctunder5)
  # output$N_D_UNDER5_out         <- renderText(overall$avg.pctunder5)
  # output$N_D_UNDER5_PER_out     <- renderText(overall$pctile.pctunder5)
  # output$RAW_D_OVER64_out       <- renderText(overall$pctover64)
  # output$S_D_OVER64_out         <- renderText(overall$state.avg.pctover64)
  # output$S_D_OVER64_PER_out     <- renderText(overall$state.pctile.pctover64)
  # output$N_D_OVER64_out         <- renderText(overall$avg.pctover64)
  # output$N_D_OVER64_PER_out     <- renderText(overall$pctile.pctover64)
  # output$RAW_D_LIFEEXP_out      <- renderText(overall$lowlifex)
  # output$S_D_LIFEEXP_out        <- renderText(overall$state.avg.lowlifex)
  # output$S_D_LIFEEXP_PER_out    <- renderText(overall$state.pctile.lowlifex)
  # output$N_D_LIFEEXP_out        <- renderText(overall$avg.lowlifex)
  # output$N_D_LIFEEXP_PER_out    <- renderText(overall$pctile.lowlifex)
  # output$NUM_NPL_out            <- renderText(overall$count.NPL)
  # output$NUM_TSDF_out           <- renderText(overall$count.TSDF)
  # output$NUM_WATERDIS_out       <- renderText(overall$num_waterdis)
  # output$NUM_AIRPOLL_out        <- renderText(overall$num_airpoll)
  # output$NUM_BROWNFIELD_out     <- renderText(overall$num_brownfield)
  # output$NUM_TRI_out            <- renderText(overall$num_tri)
  # output$NUM_SCHOOL_out         <- renderText(overall$num_school)
  # output$NUM_HOSPITAL_out       <- renderText(overall$num_hospital)
  # output$NUM_CHURCH_out         <- renderText(overall$num_church)
  # output$YESNO_AIRNONATT_out    <- renderText(overall$yesno_airnonatt)
  # output$YESNO_IMPWATERS_out    <- renderText(overall$yesno_impwaters)
  # output$YESNO_TRIBAL_out       <- renderText(overall$yesno_tribal)
  # output$YESNO_CEJSTDIS_out     <- renderText(overall$yesno_cejstdis)
  # output$YESNO_IRADIS_out       <- renderText(overall$yesno_iradis)
}

