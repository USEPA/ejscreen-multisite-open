

#' DRAFT - Plot distribution of data among residents at sites vs reference area (US, etc.)
#' @description DRAFT - Visualize indicator values (scores) as distributed across places
#' 
#' @details
#' 
#'   Not yet population weighted, so it is the distribution across sites not residents.
#' 
#'   Could be recoded to allow multiple types of sites and/or reference zones 
#'   more generally like for ejamit_compare_types_of_places()
#'   
#' @param bysite table of results from ejamit()$results_bysite, like testoutput_ejamit_1000pts_1miles$results_bysite
#' @param varname name of column in bysite, like  "Demog.Index"
#' @param type "box", "plotly", or "ggplot"
#' @param colorfills two colors for boxplot
#' @examples
#'  # plot_vs_us(testoutput_ejamit_1000pts_1miles$results_bysite, type = 'box')
#'  # plot_vs_us(testoutput_ejamit_1000pts_1miles$results_bysite, varname = "pctlingiso", type =  'box', ylim=c(0,20))
#'  # plot_vs_us(testoutput_ejamit_1000pts_1miles$results_bysite, varname = "pctlingiso", type =  'ggplot')
#'  # plot_vs_us(testoutput_ejamit_1000pts_1miles$results_bysite, varname = "pctnhaa", type =  'ggplot')
#'  # plot_vs_us(testoutput_ejamit_1000pts_1miles$results_bysite, varname = "pctnhaa", type = 'box', ylim = c(0,20))
#' @return plots
#' 
#' @export
#'
#' @examples dontrun{
#'   out <- testoutput_ejamit_1000pts_1miles
#'   plot_vs_us(out$results_bysite, type = 'box')
#'   plot_vs_us(out$results_bysite, varname = "pctlingiso", type =  'box', ylim=c(0, 20))
#'   plot_vs_us(out$results_bysite, varname = "pctlingiso", type =  'ggplot')
#'   plot_vs_us(out$results_bysite, varname = "pctnhaa", type =  'ggplot')
#'   plot_vs_us(out$results_bysite, varname = "pctnhaa", type = 'box', ylim = c(0, 20))
#'   }
plot_vs_us <- function(bysite = NULL, # ejamit()$results_bysite, 
                       varname = "pctlowinc", 
                       type = "box",
                       refarealabel = "In Avg. Blockgroup Nationwide", siteslabel = "At Avg. Site Analyzed", xlabels = c(refarealabel, siteslabel),
                       # could recode to allow multiple types of sites and/or reference zones more generally like for ejamit_compare_types_of_places()
                       refdata = NULL, 
                       nsample = 5000, fix_pctcols = TRUE,
                       colorfills = c("lightblue", "orange"),
                       ...) {
  
  stop("draft function - needs 0-100 vs 0-1 scaling fixed")
  
  stopifnot(NCOL(varname) == 1, NROW(varname) == 1, "character" %in% class(varname), all(varname %in% colnames(bysite)))
  
  # bysite
  if (is.null(bysite)) {
    if (interactive()) {
      bysite <- ejamit()$results_bysite
    } else {
      stop("bysite is required")
    }
  }
  if ("results_bysite" %in% names(bysite)) {
    # looks like full output of ejamit() was provided, not just the results_bysite data.table
    bysite <- copy(bysite$results_bysite)
  } else {
    bysite <- copy(bysite)
  }
  # correct for different 0-1 or 0-100 scaling in blockgroupstats and ejamit()$results_bysite
  bysite <- table_x100(bysite, cnames = names_pct_as_fraction_ejamit)
  data.table::setDT(bysite)
  sites <- cbind(bysite[ , c("pop", varname), with = FALSE], Locations = siteslabel)
  
  # refdata -- If no reference area is specified, use all US block groups
  if (is.null(refdata)) {
    if (!(varname %in% names(blockgroupstats))) {stop(varname, "must be a column name in refdata (which is blockgroupstats by default)")}
    if (is.data.table(blockgroupstats)) {
      setDF(blockgroupstats)
      refdata <- blockgroupstats[!is.na(blockgroupstats$pop) & !is.na(blockgroupstats[ , varname]), c("pop", varname)]
      setDT(blockgroupstats)
    } else {
      refdata <- blockgroupstats[!is.na(blockgroupstats$pop) & !is.na(blockgroupstats[ , varname]), c("pop", varname)]
    }
    if (!fix_pctcols) {warning("if using default refdata, blockgroupstats, fix_pctcols must be TRUE and ignored if set FALSE")}
    refdata <- table_x100(refdata, cnames = names_pct_as_fraction_blockgroupstats)
  } else {
    if (!(varname %in% names(refdata))) {stop(varname, "must be a column name in refdata")}
    if (fix_pctcols) {
      message("assuming refdata provided was a subset of blockgroupstats, so rescaling some indicators to ensure all percentages are scaled as 0-100 not 0-1")
      refdata <- table_x100(refdata, cnames = names_pct_as_fraction_blockgroupstats)
    }
  }
  data.table::setDT(refdata)
  refdata$Locations <- refarealabel
  refdata <- refdata[!is.na(pop), ]
  
  # Combine reference area and specified locations (e.g. near these sites)
  # as full dataset but also a smaller sampling
  both.sample <- rbind(refdata[sample(1:NROW(refdata), nsample), ], sites)
  both        <- rbind(refdata, sites)
  
  setnames(both,        varname, 'literalvarname')
  setnames(both.sample, varname, 'literalvarname')
  
  bothmeans <- both[ , .(mean(literalvarname, na.rm = T)), by = "Locations"]$V1
  # both75 <- both[ , .(quantile(literalvarname, na.rm = T, probs = 0.75, type = 1))]$V1
  # both25 <- both[ , .(quantile(literalvarname, na.rm = T, probs = 0.25, type = 1))]$V1
  
  varlabel <- fixcolnames(varname, 'r', 'shortlabel')
  maintitle <- paste0("Comparison of ", varlabel, " among Residents ", siteslabel, " versus ", refarealabel)
  
  if (type == 'box') {
    
    boxplot(literalvarname ~ Locations, data = both, ylab = varlabel, col = colorfills,
            main = paste0(maintitle, "\nDistribution over sites, not population weighted quantiles!"), ...)
    points(1:2, bothmeans, col = 'black', pch = 22, bg = "white", cex = 3)
    abline(h = bothmeans[1], col = "white")
    abline(h = bothmeans[2], col = 'white')
    points(jitter(1 + (siteslabel == both.sample$Locations)), both.sample$literalvarname, pch = 20, col = "darkgray", cex = 0.7) # pch = "."
    
  } else {
    
    if (type == 'plotly') {
      
      setnames(both.sample, "literalvarname", "Indicator")
      d <- plotly::highlight_key(both.sample)
      # scatt <- plot_ly(d, x = ~Locations, y = ~Indicator) %>%
      #   add_markers(color = I("black"))
      
      # 'statistical trace types'
      # hist <- plotly::plot_ly(d, x = ~factor(Locations)) %>% 
      #   plotly::add_histogram(color = I("black"))
      # box <- plotly::plot_ly(d, x = ~Locations, y = ~Indicator, color = I("black")) %>% 
      #   plotly::add_boxplot(name = " ")
      
      violin <- plotly::plot_ly(d, x = ~Locations, y = ~Indicator, color = I("blue")) %>%
        plotly::add_trace(type = "violin", name = " ")
      plotly::subplot(
        # scatt, box, 
        violin, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
        # subplot(hist, widths = c(.75, .25), titleX = TRUE, titleY = TRUE) %>%
        plotly::layout(
          barmode = "overlay", 
          title = maintitle,
          showlegend = FALSE
        ) %>%
        plotly::highlight("plotly_selected")
      
    } else {
      
      if (type == 'ggplot') {
        
        # https://r-graph-gallery.com/violin.html
        
        ggplot2::ggplot(both.sample, aes(x = Locations, y = literalvarname, color = Locations)) +
          ggplot2::geom_violin(aes(fill = Locations, 
                                   weight = pop), 
                               alpha = 0.05) +
          ggplot2::geom_jitter(size = 1, width = 0.05) +
          geom_boxplot(aes(x = Locations, y = literalvarname, 
                           weight = pop, 
                           alpha = 0.03, col = "gray")) +
          
          theme_bw() +
          theme(panel.grid = element_blank()) +
          xlab("Locations") +
          ylab(varlabel) +
          # labs(fill = "Locations", color = "Locations") +
          ggtitle(maintitle, subtitle = "WEIGHTED NOT YET IMPLEMENTED FOR Population weighted distribution (quantiles of all residents not sites)")
      }
    }
  }}

