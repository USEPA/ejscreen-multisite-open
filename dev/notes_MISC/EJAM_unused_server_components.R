#############################################################################  # 
#############################################################################  # 


#############################################################################  # 

## static preview of uploaded dataset - not used currently
# output$print_test2 <- renderTable({
#     req(data_uploaded())
#     
#     if (current_upload_method() == 'NAICS') {
#       #takes NAICS codes selected, finds NAICS descriptions, and presents them  
#       dt_result_by_naic = data_uploaded()[, .(Count = .N), by = NAICS]
#       naics_desc = EJAM::NAICS[EJAM::NAICS %in% dt_result_by_naic$NAICS]
#       dt_names = data.frame("NAICS"=naics_desc,"Description"=names(naics_desc))
#       naicsdt = merge(x = dt_result_by_naic, y = dt_names, by='NAICS')
#       naics_reorder = data.frame(naicsdt$Description,naicsdt$Count)
#       colnames(naics_reorder) = c("NAICS Code","Facility Count")
#       
#       naics_reorder
#       #print(naics_reorder,row.names=FALSE)
#     } else {
#       head(data_uploaded())
#     }
#   })

#############################################################################  # 
# ~ ####
# ______ SOME EXEC SUM TEXT  ####

## output: demographic executive summary text ### #
# output$exec_summ_d <- renderUI({
#   
#   req(data_summarized())
#   
#   ## can use a dropdown to select indicator, or..
#   ## or just use the most extreme one
#   type <- 'max' # 'choose'
#   
#   ## if only care about max indicator - this is not currently used in favor of the 
#   ## dropdown option (type == 'choose')
#   if (type == 'max') {
#     max.ratio.d <- max(ratio.to.us.d())
#     max.name.d <- names(ratio.to.us.d())[which.max(ratio.to.us.d())]
#     
#     max.name.d.friendly <- names_d_batch_friendly[which.max(ratio.to.us.d())]  # xxx from EJAMbatch.summarizer ::

############################################################################################################################################### #
# > c(names_d_friendly, names_d_subgroups_nh_friendly)

# [1] "Demog.Ind."                                                             "Suppl Demog Index"                                                     
# [3] "% Low-inc."                                                             "% Limited English"         ** different sort order                                                    
# [5] "% Unemployed"                                                           "% < High School"                                                       
# [7] "Low life expectancy"                                                    "% < age 5"                                                             
# [9] "% > age 64"                                                             "% People of Color"       

# [11] "% Hispanic or Latino (any race)"                                        "% Black or African American, single race, non-Hispanic"                
# [13] "% Asian, single race, non-Hispanic"                                     "% American Indian or Alaska Native, single race, non-Hispanic"         
# [15] "% Native Hawaiian or Other Pacific Islander, single race, non-Hispanic" "% Other race, single race, non-Hispanic"                               
# [17] "% Multirace (two or more races), non-Hispanic"                          "% White, single race, non-Hispanic"        


# >  names_d_batch_friendly # from EJAMbatch.summarizer ::   ** different sort order than above, and worded differently

# [1] "Demog.Ind."                                                                          "Suppl Demog Index"                                                                  
# [3] "% Low-inc."                                                                          "% People of Color"                                                           
# [5] "% <High School"                                                                      "% Linguistic Isol."                                                                 
# [7] "% < age 5"                                                                           "% > age 64"                                                                         
# [9] "% unemployed"                                                                        "Low life expectancy"  

# [11] "% White nha (White non-Hispanic, single race)"                                       "% Hisp (Hispanic or Latino)"                                                        
# [13] "% Black nha (Black or African American non-Hispanic, single race)"                   "% Asian nha (Asian non-Hispanic, single race)"                                      
# [15] "% AmIndian nha (American Indian and Alaska Native non-Hispanic, single race)"        "% Hawaii nha (Native Hawaiian and Other Pacific Islander non-Hispanic, single race)"
# [17] "% Other nha (Other race non-Hispanic, single race)"                                  "% Multi nha (Two or more races non-Hispanic)"          
############################################################################################################################################### #

#     median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',max.name.d)]
#     
#     exec_text_d <-paste0(
#       'Key demographic factor: <strong>',
#       max.name.d.friendly, 
#       '</strong><br>',
#       'People who live near (within ', input$bt_rad_buff,' mile(s)',#, input$radius_units,
#       ' of any of) these ', nrow(data_processed()$results_bysite),
#       ' sites are <strong>', round(max.ratio.d, 1), ' times</strong> as likely to be ', 
#       max.name.d.friendly, ' as the average person in the US (',
#       round(100 * data_summarized()$rows['Average person', max.name.d]), '% vs. ', 
#       round(100 * avg.in.us[,max.name.d]),
#       '%). The other demographic indicators have lower ratios.',
#       '<br>',
#       'The median (50th percentile) site here is at the <strong>', 
#       scales::label_ordinal()(median.pctile.in.us),
#       '</strong> percentile of all US residents for ', max.name.d.friendly
#     )
#   } else if (type == 'choose') {
#     
#     cur.name.d <-  input$key_ind_d # names(ratio.to.us.d())[which.max(ratio.to.us.d())]
#     cur.ratio.d <- ratio.to.us.d()[cur.name.d]
#     
#     ## get friendly version of D name
#     cur.name.d.friendly <- EJAMejscreenapi::map_headernames %>%        # replace with EJAM::names_d_friendly, names_d_subgroups_friendly  XXX
#       filter(newnames_ejscreenapi == cur.name.d) %>% 
#       pull(names_friendly)
#     
#     ## grab D percentile of median site ##  ##
#     median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',cur.name.d)]
#     
#     exec_text_d <-paste0(
#       'Key demographic factor: <strong>',
#       cur.name.d.friendly, 
#       '</strong><br>',
#       'People who live near (within ', input$bt_rad_buff,' mile(s)',#, input$radius_units,
#       ' of any of) these ', nrow(data_processed()$results_bysite),
#       ' sites have, on average, <strong>', round(cur.ratio.d, 1), ' times</strong> as high indicator values for <strong>', 
#       cur.name.d.friendly, '</strong> as the average person in the US (',
#       round(100 * data_summarized()$rows['Average person', cur.name.d]), '% vs. ', 
#       round(100 * avg.in.us[,cur.name.d]),
#       '%)',
#       '<br>',
#       'The median (50th percentile) site here is at the <strong>', 
#       scales::label_ordinal()(median.pctile.in.us),
#       '</strong> percentile of all US residents for <strong>', cur.name.d.friendly, '</strong>'
#     )
#   }
#   
#   HTML(exec_text_d)
#   ## need to add state multiplier?
#   ## need to add top X% stat?
# })
# 
# ## _  output: environmental executive summary text ###  #
# output$exec_summ_e <- renderUI({
#   
#   req(data_summarized())
#   
#   ## can use a dropdown to select indicator
#   ## or just use the most extreme one
#   type <- 'choose' #  "max" 
#   
#   
#   ## if only care about max indicator - this is not currently used in favor of the 
#   ## dropdown option (type == 'choose')
#   if (type == 'max') {
#     max.ratio.e <- max(ratio.to.us.e())
#     max.name.e <- names(ratio.to.us.e())[which.max(ratio.to.us.e())]
#     
#     max.name.e.friendly <- EJAM::names_e_friendly[which.max(ratio.to.us.e())]
#     
#     median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',max.name.e)]    # *** SHOULD DOUBLE CHECK THIS 
#     
#     exec_text_e <-paste0(
#       'Key environmental factor: <strong>',
#       max.name.e.friendly, 
#       '</strong><br>',
#       'People who live near (within ', input$bt_rad_buff,' miles', #input$radius_units,
#       ' of any of) these ', nrow(data_processed()$results_bysite),
#       ' sites have, on average, <strong>', round(max.ratio.e, 1), ' times</strong> as high indicator values for <strong>', 
#       max.name.e.friendly, '</strong> as the average person in the US (',
#       
#       round(data_summarized()$rows['Average person', max.name.e], 2), ' vs. ',    # *** SHOULD DOUBLE CHECK THIS 
#       round(avg.in.us[,max.name.e], 2),
#       
#       '). The other environmental indicators have lower ratios.',
#       '<br>',
#       'The median (50th percentile) site here is at the <strong>', 
#       scales::label_ordinal()(median.pctile.in.us),
#       '</strong> percentile of all US residents for <strong>', max.name.e.friendly
#     )
#     
#   } else if (type == 'choose') {
#     cur.name.e <- input$key_ind_e # names(ratio.to.us.d())[which.max(ratio.to.us.d())]
#     cur.ratio.e <- ratio.to.us.e()[cur.name.e]
#     
#     cur.name.e.friendly <- EJAMejscreenapi::map_headernames %>%    #  # replace with EJAM::names_e_friendly XXX
#       filter(newnames_ejscreenapi == cur.name.e) %>% 
#       pull(names_friendly)
#     
#     ## _ grab E percentile of median site ###  #
#     median.pctile.in.us <- data_summarized()$rows['Median site', paste0('pctile.',cur.name.e)]
#     
#     exec_text_e <-paste0(
#       'Key environmental factor: <strong>',
#       cur.name.e.friendly, 
#       '</strong><br>',
#       'People who live near (within ', input$bt_rad_buff,' mile(s)',#, input$radius_units,
#       ' of any of) these ', nrow(data_processed()$results_bysite),
#       ' sites have, on average, <strong>', round(cur.ratio.e, 1), ' times</strong> as high indicator values for ', 
#       cur.name.e.friendly, ' as the average person in the US (',
#       round( data_summarized()$rows['Average person', cur.name.e], 2), ' vs. ', 
#       round(avg.in.us[,cur.name.e], 2),
#       ')',
#       '<br>',
#       'The median (50th percentile) site here is at the <strong>', 
#       scales::label_ordinal()(median.pctile.in.us),
#       '</strong> percentile of all US residents for <strong>', cur.name.e.friendly, '</strong>'
#     )
#   }
#   
#   HTML(exec_text_e)
#   ## need to add state multiplier?
#   ## need to add top X% stat?
# })

## show modal with report outline ####
# observeEvent(input$show_outline, {    # I DONT THINK THIS POPUP OUTLINE LOOKS GREAT AND IT IS HARD TO KEEP IN SYNC WITH ACTUAL RMD OUTLINE
#   showModal(
#     modalDialog(
#       #HTML(report_outline)
#       HTML(
#         stringr::str_replace(report_outline, 
#                              'Broad overview of findings',
#                              '<mark>Broad overview of findings</mark>'
#         )
#       )
#     )
#   )
# })

## show welcome modal on app startup - commented out for now
# showModal(
#   modalDialog(
#     title = 'Welcome to EJAM!', 
#     "Add basic message here",
#     footer = actionButton('ui_close_welcome', 'Close')
#   )
# )
## close welcome modal on button click
# observeEvent(input$ui_close_welcome,{
#   removeModal()
# })
