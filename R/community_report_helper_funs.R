

#' fill_tbl_row - write a demog. or envt. indicator to an html table row
#'@param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#'@param var_value, variable name of indicator to pull from results, such as 'pm', 'pctlowinc', 'Demog.Index'
#'@param var_name, nicer name of indicator to use in table row; can include HTML sub/superscripts
#'@export
fill_tbl_row <- function(output_df, var_value, var_name){
  txt <- '<tr>'
  
  id_col <- 'selected-variables'
  txt <- paste0(txt, '\n','<td headers="data-indicators-table-',
                id_col,'">',
                var_name,'</td>')
  
  hdr_names <- c('value','state-average',
                 'percentile-in-state','usa average','percentile-in-usa')
  
  var_values <- paste0(c('','state.avg.','state.pctile.', 'avg.','pctile.'), var_value)
  
  for(j in 1:length(var_values)){
    cur_var <-var_values[j]
    if('data.table' %in% class(output_df)){
      cur_val <- output_df[, ..cur_var]#round(output_df[,..cur_var],2)
    } else {
      cur_val <- output_df[, cur_var] #round(output_df[,cur_var],2)
    }
    txt <- txt <- paste0(txt, '\n','<td headers="data-indicators-table-',
                         hdr_names[j],'">',
                         cur_val,'</td>')
    
  }
  txt <- paste0(txt, '\n','</tr>')
  return(txt)
}

#' fill_tbl_row_ej - write an EJ or EJ supp index to an html table row
#'@param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#'@param var_value, variable name of indicator to pull from results, such as 'pm', 'pctlowinc', 'Demog.Index'
#'@param var_name, nicer name of indicator to use in table row; can include HTML sub/superscripts
#'@export
fill_tbl_row_ej <- function(output_df, var_value, var_name){
  txt <- '<tr>'
  
  id_col <- 'selected-variables'
  txt <- paste0(txt, '\n','<td headers="data-indicators-table-',
                id_col,'">',
                var_name,'</td>')
  
  hdr_names <- c('value',
                 'percentile-in-state','percentile-in-usa')
  
  var_values <- paste0(c('','state.pctile.','pctile.'), var_value)
  
  for(j in 1:length(var_values)){
    cur_var <-var_values[j]
    if('data.table' %in% class(output_df)){
      cur_val <- output_df[, ..cur_var] #round(output_df[,..cur_var],2)
    } else {
      cur_val <- output_df[, cur_var]#round(output_df[,cur_var],2)
    }
    txt <- txt <- paste0(txt, '\n','<td headers="data-indicators-table-',
                         hdr_names[j],'">',
                         cur_val,'</td>')
    
  }
  txt <- paste0(txt, '\n','</tr>')
  return(txt)
}

#' fill_tbl_full - create full demog. or envt. HTML table of indicator rows
#'@param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#'@export
fill_tbl_full <- function(output_df){
 
  # css_head <-'
  #    <link href="https://fonts.googleapis.com/css2?family=Heebo:wght@500;600" rel="stylesheet">
  # <link href="https://fonts.googleapis.com/css2?family=Oswald:wght@300;400;500;700&amp;display=swap" rel="stylesheet">
  # <link href="https://fonts.googleapis.com/css2?family=Noto+Sans&amp;display=swap" rel="stylesheet">
  # '
  
  full_html <- ''
  
  tbl_head <- '<table id="data-indicators-table"        class="color-alt-table"  summary="EJScreen environmental and socioeconomic indicators data">
  <thead id="data-indicators-table-header" class="color-alt-table-header">
  <tr>
  <th id="data-indicators-table-selected-variables" scope="col">SELECTED VARIABLES</th>
  <th id="data-indicators-table-value" scope="col">VALUE</th>
  <th id="data-indicators-table-state-average" scope="col">STATE<br> AVERAGE</th>
  <th id="data-indicators-table-percentile-in-state" scope="col">PERCENTILE<br> IN STATE</th>
  <th id="data-indicators-table-usa average" scope="col">USA AVERAGE</th>
  <th id="data-indicators-table-percentile-in-usa" scope="col">PERCENTILE<br> IN USA</th>
  </tr>
  </thead>
  <tbody>
  <tr class="color-alt-table-subheader">
  <th colspan="7">Pollution and Sources</th>
  </tr>'
  
  tbl_head2 <- '<tr class="color-alt-table-subheader">
<th colspan="7">Socioeconomic Indicators</th>
  </tr>'
  
  full_html <- paste(full_html, tbl_head, sep='\n')
  
  var_values_e <- c('pm','o3','dpm','cancer','resp','rsei','traffic.score','pctpre1960',
                  'proximity.npl','proximity.rmp','proximity.tsdf','ust','proximity.npdes')
  
  var_names_e <- c('Particulate Matter&nbsp;&nbsp;(μg/m<span class="table-superscript"><sup>3</sup></span>)',
                 'Ozone&nbsp;&nbsp;(ppb)',
                 'Diesel Particulate Matter&nbsp;&nbsp;(μg/m<span class="table-superscript"><sup>3</sup></span>)',
                 'Air Toxics Cancer Risk*&nbsp;&nbsp;(lifetime risk per million)',
                 'Air Toxics Respiratory HI*',
                 'Toxic Releases to Air',
                 'Traffic Proximity&nbsp;&nbsp;(daily traffic count/distance to road)',
                 'Lead Paint&nbsp;&nbsp;(% Pre-1960 Housing)',
                 'Superfund Proximity&nbsp;&nbsp;(site count/km distance)',
                 'RMP Facility Proximity&nbsp;&nbsp;(facility count/km distance)',
                 'Hazardous Waste Proximity&nbsp;&nbsp;(facility count/km distance)',
                 'Underground Storage Tanks&nbsp;&nbsp;(count/km<span class="table-superscript"><sup>3</sup></span>)',
                 'Wastewater Discharge&nbsp;&nbsp;(toxicity-weighted concentration/m distance)'
                 )
 
  tbl_rows_e <- sapply(1:length(var_values_e), function(x) fill_tbl_row(output_df, 
                                                                        var_value = var_values_e[x], 
                                                                        var_name=var_names_e[x]))
  full_html <- paste(full_html, 
                     paste(tbl_rows_e , collapse='\n'),
 
   sep='', collapse='\n')
  full_html <- paste(full_html, tbl_head2,collapse='\n')
  
  
  var_values_d <- c('Demog.Index', 'Demog.Index.Supp','pctmin','pctlowinc','pctunemployed',
                    'pctlingiso','pctlths','pctunder5','pctover64','lowlifex')
  
  var_names_d <- c('Demographic Index',
                   'Supplemental Demographic Index',
                   'People of Color',
                   'Low Income',
                   'Unemployment Rate',
                   'Limited English Speaking Households',
                   'Less Than High School Education',
                   'Under Age 5',
                   'Over Age 64',
                   'Low Life Expectancy'
  )
 
  tbl_rows_d <- sapply(1:length(var_values_d), function(x) fill_tbl_row(output_df, 
                                                                        var_value = var_values_d[x], 
                                                                        var_name=var_names_d[x]))
  full_html <- paste(full_html, 
                     paste(tbl_rows_d, collapse='\n'),
   sep='', collapse='\n')

full_html <- paste(full_html, '</tbody>
</table>', collapse='\n')
  
  return(full_html)
}

#' fill_tbl_full_ej - create full EJ + EJ supp index HTML table of indicator rows
#'@param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#'@export
fill_tbl_full_ej <- function(output_df){

  tbl_head <- '<table id="data-indicators-table"        class="color-alt-table"  summary="EJScreen environmental and socioeconomic indicators data">
  <thead id="data-indicators-table-header" class="color-alt-table-header">
  <tr>
  <th id="data-indicators-table-selected-variables" scope="col">SELECTED VARIABLES</th>
  <th id="data-indicators-table-value" scope="col">VALUE</th>
  <th id="data-indicators-table-percentile-in-state" scope="col">PERCENTILE<br> IN STATE</th>
  <th id="data-indicators-table-percentile-in-usa" scope="col">PERCENTILE<br> IN USA</th>
  </tr>
  </thead>
  <tbody>
  <tr class="color-alt-table-subheader">
  <th colspan="7">EJ Indexes</th>
  </tr>'
  
  tbl_head2 <- '<tr class="color-alt-table-subheader">
<th colspan="7">Supplemental EJ Indexes</th>
  </tr>'
  
  full_html <- tbl_head #paste(full_html, tbl_head, sep='\n')
  
  
  full_html <- paste(full_html, 
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.pm.eo', var_name='Particulate Matter'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.o3.eo', var_name='Ozone'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.dpm.eo', var_name='Diesel Particulate Matter'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.cancer.eo', var_name='Air Toxics Cancer Risk'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.resp.eo', var_name='Air Toxics Respiratory HI'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.rsei.eo', var_name='Toxic Releases to Air'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.traffic.score.eo', var_name='Traffic Proximity'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.pctpre1960.eo', var_name='Lead Paint'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.proximity.npl.eo', var_name='Superfund Proximity'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.proximity.rmp.eo', var_name='RMP Facility Proximity'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.proximity.tsdf.eo', var_name='Hazardous Waste Proximity'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.ust.eo', var_name='Underground Storage Tanks'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.proximity.npdes.eo', var_name='Wastewater Discharge')
                     , sep='', collapse='\n')
  full_html <- paste(full_html, tbl_head2,collapse='\n')
  
  full_html <- paste(full_html, 
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.pm.supp', var_name='Particulate Matter'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.o3.supp', var_name='Ozone'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.dpm.supp', var_name='Diesel Particulate Matter'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.cancer.supp', var_name='Air Toxics Cancer Risk'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.resp.supp', var_name='Air Toxics Respiratory HI'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.rsei.supp', var_name='Toxic Releases to Air'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.traffic.score.supp', var_name='Traffic Proximity'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.pctpre1960.supp', var_name='Lead Paint'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.proximity.npl.supp', var_name='Superfund Proximity'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.proximity.rmp.supp', var_name='RMP Facility Proximity'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.proximity.tsdf.supp', var_name='Hazardous Waste Proximity'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.ust.supp', var_name='Underground Storage Tanks'),
                     fill_tbl_row_ej(output_df, 'EJ.DISPARITY.proximity.npdes.supp', var_name='Wastewater Discharge')
                     , sep='', collapse='\n')
  
  full_html <- paste(full_html, '</tbody>
</table>', collapse='\n')
  
  return(full_html)
}


#' fill_tbl_row_subgroup - write a demographic subgroup indicator to an html table row
#'@param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#'@param var_value, variable name of indicator to pull from results, such as 'pm', 'pctlowinc', 'Demog.Index'
#'@param var_name, nicer name of indicator to use in table row; can include HTML sub/superscripts
#'@export
fill_tbl_row_subgroups <- function(output_df, var_value, var_name){
  txt <- '<tr>'
  
  id_col <- 'selected-variables'
  txt <- paste0(txt, '\n','<td headers="data-indicators-table-',
                id_col,'">',
                var_name,'</td>')
  
  hdr_names <- c('value')
  
  var_values <- paste0(c(''), var_value)
  
  for(j in 1:length(var_values)){
    cur_var <-var_values[j]
    if('data.table' %in% class(output_df)){
      cur_val <- round(100*output_df[, ..cur_var], 1)#round(output_df[,..cur_var],2)
    } else {
      cur_val <- round(100*output_df[, cur_var],1) #round(output_df[,cur_var],2)
    }
    txt <- txt <- paste0(txt, '\n','<td headers="data-indicators-table-',
                         hdr_names[j],'">',
                         cur_val,'</td>')
    
  }
  txt <- paste0(txt, '\n','</tr>')
  return(txt)
}

#' fill_tbl_full - create full demog subgroup HTML table of indicator rows
#'@param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#'@export
fill_tbl_full_subgroups <- function(output_df){
  
  # css_head <-'
  #    <link href="https://fonts.googleapis.com/css2?family=Heebo:wght@500;600" rel="stylesheet">
  # <link href="https://fonts.googleapis.com/css2?family=Oswald:wght@300;400;500;700&amp;display=swap" rel="stylesheet">
  # <link href="https://fonts.googleapis.com/css2?family=Noto+Sans&amp;display=swap" rel="stylesheet">
  # '
  
  full_html <- ''
  
  tbl_head <- '<table id="data-indicators-table"        class="color-alt-table"  summary="EJScreen environmental and socioeconomic indicators data">
  <thead id="data-indicators-table-header" class="color-alt-table-header">
  <tr>
  <th id="data-indicators-table-selected-variables" scope="col">SELECTED VARIABLES</th>
  <th id="data-indicators-table-value" scope="col">VALUE</th>
  </tr>
  </thead>
  <tbody>
  <tr class="color-alt-table-subheader">
  <th colspan="7">Breakdown by Race</th>
  </tr>'
  
  tbl_head2 <- '<tr class="color-alt-table-subheader">
<th colspan="7">Breakdown by Gender</th>
  </tr>'
  
  tbl_head3 <- '<tr class="color-alt-table-subheader">
<th colspan="7">Limited English Speaking Breakdown</th>
  </tr>'
  
  full_html <- paste(full_html, tbl_head, sep='\n')
  
  var_values_d_race <- c('pctnhwa','pctnhba','pctnhaa','pcthisp',
                     'pctnhaiana','pctnhnhpia','pctnhotheralone',
                     'pctnhmulti')
  var_names_d_race <- c('% White', '% Black','% Asian','% Hispanic',
                        '% American Indian','% Hawaiian/Pacific Islander',
                        '% Other Race','% Two or more races')
  
  tbl_rows_d_race <- sapply(1:length(var_values_d_race), function(x) fill_tbl_row_subgroups(output_df, 
                                                                        var_value = var_values_d_race[x], 
                                                                        var_name=var_names_d_race[x]))
  full_html <- paste(full_html, 
                     paste(tbl_rows_d_race , collapse='\n'),
                     
                     sep='', collapse='\n')
  full_html <- paste(full_html, tbl_head2,collapse='\n')
  
  
  var_values_d_gender <- c('pctmale','pctfemale')
  
  var_names_d_gender <- c('% Male', '% Female')
  
  tbl_rows_d_gender <- sapply(1:length(var_values_d_gender), function(x) fill_tbl_row_subgroups(output_df, 
                                                                        var_value = var_values_d_gender[x], 
                                                                        var_name=var_names_d_gender[x]))
  full_html <- paste(full_html, 
                     paste(tbl_rows_d_gender, collapse='\n'),
                     sep='', collapse='\n')
  
  #full_html <- paste(full_html, tbl_head3,collapse='\n')
  
  # var_values_d_lim <- c('pctmale','pctfemale')
  # 
  # var_names_d_lim <- c('% Male', '% Female')
  # 
  # tbl_rows_lim <- sapply(1:length(var_values_d_lim), function(x) fill_tbl_row(output_df, 
  #                                                                              var_value = var_values_d_lim[x], 
  #                                                                            var_name=var_names_d_lim[x]))
  # full_html <- paste(full_html, 
  #                    paste(tbl_rows_d_lim, collapse='\n'),
  #                    sep='', collapse='\n')
  
  
  full_html <- paste(full_html, '</tbody>
</table>', collapse='\n')
  
  return(full_html)
}

#' generate_html_header - build HTML header for community report
#' @param analysis_title, title to use in header of report
#' @param totalpop, total population included in location(s) analyzed
#' @param locationstr, description of the location(s) analyzed
#' @param in_shiny, whether the function is being called in or outside of shiny - affects location of header
#'@export
generate_html_header <- function( analysis_title, totalpop, locationstr, in_shiny = FALSE){
  
  if(in_shiny){
    shift_hsb <- 630
    shift_hpb <- 600
    shift_hbd <- 560
  } else {
    shift_hsb <- 70
    shift_hpb <- 40
    shift_hbd <- 0
  }
  
  # img_html <- paste0('<img src="',app_sys('report/community_report/EPA_logo_white.png'),
  #                    '" alt="EPA logo" width="110" height="35" style="position: absolute; left: 950px; top: ',shift_hbd+90,'px">')
  # 
  
  paste0(' 
  <link href="https://fonts.googleapis.com/css2?family=Heebo:wght@500;600" rel="stylesheet">
  <link href="https://fonts.googleapis.com/css2?family=Oswald:wght@300;400;500;700&amp;display=swap" rel="stylesheet">
  <link href="https://fonts.googleapis.com/css2?family=Noto+Sans&amp;display=swap" rel="stylesheet">

 <link rel="stylesheet"  type="text/css" media="all" href="communityreport.css" />  
   <link rel="stylesheet"  type="text/css" media="all" href="main.css"            /> 
<div id="header-secondary-background" ','style="top: ',shift_hsb,'px;"', '></div>
<div id="header-primary-background" ', 'style="top: ',shift_hpb,'px;"','></div> 
<div id="header-background-detail" ', 'style="top: ',shift_hbd,'px;"','></div>',

#img_html,
'<h1 id="title" tabindex="0" style="white-space: nowrap; position: absolute; color: white;left: 140px; top: ',shift_hbd+80,'px">EJAM Community Report</h1>
<p style="
            color: white;
            position: absolute;
            font-family: heebo, Arial, sans-serif;
            font-size: 20px;
            top: ',shift_hbd+170,'px;
            left: 240px;
            text-align: center;
                white-space: nowrap;
        ">
            This report provides environmental and socioeconomic information for
            user-defined areas,<br>
            and combines that data into environmental justice and supplemental
            indexes.
        </p>  
        <!-- This report provides...  -->


       <div class="header" style="
            margin-top: 302px;
            width: 100%;
            font-weight: bold;
            background-color: #0e98d7;
            color: white;
            height: 115px;
            clear: both;
            ;">
         <div style="width: 45%; float:left">
  
    <h2 id="placename" style="
                 font-size: 45px;padding-left: 50px;max-width: 100%;line-height: 1.15em;text-align: center;max-height: 115px;margin: 0;
                 ">', analysis_title , '</h2> 
  </div>',
'<div style="width: 55%; float:right">
   <h5 style="
                  font-family: heebo, Arial, sans-serif;text-align: center;font-size: 22px;line-height: 29px;text-align: center;">',
'Population: <span id="TOTALPOP">',totalpop,'</span><br>',
locationstr,'<br>
                  <!--  Area in square miles: {{inputAreaMiles_ht}}  -->
                </h5>
</div>
</div>
', sep='', collapse='')
  # Population: <span id="TOTALPOP">',totalpop,'</span><br>',
}

#' generate_demog_header - build header for demog. + envt. tables in community report
#'@export
generate_demog_header <- function(){
  '<div id="page-3-header" class="header" style="
            background-color: #0e98d7;
            color: white;
            height: 85px;
            clear: both;
            margin-top: 7px;">
    <h2 tabindex="12" style="text-align: center; padding-top: 35px; font-size: 32px; padding-left: 20px;">
      Environmental and Socioeconomic Indicators Data
    </h2>
      </div>'
}

#' generate_ej_header - build header for EJ index table in community report
#'@export
generate_ej_header <- function(){
  '<br>

 <div id="page-2-header"
             class="header"
             style="
            background-color: #0e98d7;
            color: white;
            text-align: center;
            padding: 20px 32px 10px 32px;
            margin: 10px 0 -23px 0;
        ">
            <h2 tabindex="8" style="font-size: 30px; margin-bottom: -5px">
                Environmental Justice & Supplemental Indexes
            </h2>
            <p style="font-family: Oswald, Arial, sans-serif; font-size: 15px; padding-left: 20px;">
                The environmental justice and supplemental indexes are a combination of environmental 
                and socioeconomic information. There are thirteen EJ indexes and supplemental indexes 
                in EJScreen reflecting the 13 environmental indicators. The indexes for a selected area 
                are compared to those for all other locations in the state or nation. For more information 
                and calculation details on the EJ and supplemental indexes, please visit the 
                <a tabindex="9" href="https://www.epa.gov/ejscreen" style="color: white">EJScreen website</a>. 
            </p>
        </div>
        <div style="
            background-color: #71bf44;
            color: white;
            text-align: center;
            padding: 0 32px 7px 32px;
        ">
            <h3  tabindex="10" style="
            padding-top: 10px;
            margin-bottom: -10px;
            font-family: Arial, sans-serif;
            font-size: 23px;
            ">
                EJ INDEXES
            </h3>
            <p style="font-family: Oswald, Arial, sans-serif; font-weight: 300; font-size: 16px; margin: 15px 15% -2px 15%">
                The EJ indexes help users screen for potential EJ concerns. To do this,
                the EJ index combines data on low income and people of color populations
                with a single environmental indicator.
            </p>
        </div>'
}

#' generate_ej_supp_header - build header for EJ supp indexes in community report
#'@export
generate_ej_supp_header <- function(){
  '<div style="
                background-color: #71bf44;
                color: white;
                text-align: center;
                padding: 0 32px 7px 32px;
            ">
    <h3 tabindex="11" style="
                padding-top: 10px;
                margin-bottom: -10px;
                font-family: Arial, sans-serif;
                font-size: 23px;
                ">
      SUPPLEMENTAL INDEXES
    </h3>
      <p style="font-family: Oswald, Arial, sans-serif; font-weight: 300; font-size: 16px; margin-bottom: -2px; padding-left: 20px;">
        The supplemental indexes offer a different perspective on
      community-level vulnerability. They combine data on percent low-income,
      percent linguistically isolated, percent less than high school
      education, percent unemployed, and low life expectancy with a single
      environmental indicator.
      </p>
        </div>'
}

