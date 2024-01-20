
#' draft code to plot distribution of a variable among residents near set of sites vs nationwide
#' Not yet population weighted
#' @param bysite table of results from ejamit()$results_bysite, like testoutput_ejamit_1000pts_1miles$results_bysite
#' @param varname name of column in bysite, like  "Demog.Index"
#' @param type "box", "plotly", or "ggplot"
#' @param colorfills two colors for boxplot
#'
#' @return plots
#' @export
#'
#' @examples dontrun{
#'   plot_vs_us(testoutput_ejamit_1000pts_1miles$results_bysite, type = 'box')
#'   plot_vs_us(testoutput_ejamit_1000pts_1miles$results_bysite, varname = "pctlingiso", type =  'box', ylim=c(0,20))
#'   plot_vs_us(testoutput_ejamit_1000pts_1miles$results_bysite, varname = "pctlingiso", type =  'ggplot')
#'   plot_vs_us(testoutput_ejamit_1000pts_1miles$results_bysite, varname = "pctnhaa", type =  'ggplot')
#'   plot_vs_us(testoutput_ejamit_1000pts_1miles$results_bysite, varname = "pctnhaa", type = 'box', ylim = c(0,20))
#'   }
plot_vs_us <- function(bysite = ejamit()$results_bysite, varname = "Demog.Index", refdt = NULL, type = "box", colorfills = c("lightblue", "orange"), ...) {
  if (is.null(refdt)) {
    refdt <- copy(blockgroupstats[ , c("pop", varname), with = FALSE])
  }
  nsample <- 5000
  # bysite <- testoutput_ejamit_1000pts_1miles$results_bysite
  ## or
  # bysite <- ejamit(testpoints_1000)
  # bysite <- bysite$results_bysite
  
  # correct for different 0-1 or 0-100 scaling in blockgroupstats and ejamit()$results_bysite
  refdt <- fix_pctcols_x100(refdt)
  # x100_in_blockgroupstats <- names_pct_as_fraction_blockgroupstats
  # setDF(refdt)
  # refdt[ , x100_in_blockgroupstats] <- 100 * refdt[ , x100_in_blockgroupstats]
  # setDT(refdt)
  
  # correct for different scaling in blockgroupstats and ejamit()$results_bysite
  bysite <- fix_pctcols_x100(bysite)
  # x100varnames = c(
  #   names_d, names_d_avg, names_d_state_avg,
  #   names_d_subgroups, names_d_subgroups_avg, names_d_subgroups_state_avg,
  #   "pctdisability",  "p_own_occupied", 
  #   "pctunder18", "pctover17", "pctmale", "pctfemale")
  # if (varname %in% x100varnames) {
  #   setDF(bysite)
  #   bysite[ , varname] <- 100 * bysite[ , varname]
  #   setDT(bysite)
  #   }
  
  sites <- cbind(bysite[ , c("pop", varname), with = FALSE], Locations = "Near these sites")

  us <- cbind(refdt, Locations = "Nationwide")
  us.sample <- us[sample(1:NROW(us), nsample), ]
  both.sample <- rbind(us.sample, sites)
  both <- rbind(us, sites)
  setnames(both, varname, 'literalvarname')
  setnames(both.sample, varname, 'literalvarname')
  
  bothmeans <- both[ , .(mean(literalvarname, na.rm = T)), by = "Locations"]$V1
  # both75 <- both[ , .(quantile(literalvarname, na.rm = T, probs = 0.75, type = 1))]$V1
  # both25 <- both[ , .(quantile(literalvarname, na.rm = T, probs = 0.25, type = 1))]$V1
  # 
  varlabel <- fixcolnames(varname, 'r', 'long')
  maintitle <- paste0("Comparison of ", varlabel, " among residents near these sites versus nationwide")
  
  if (type == 'box') {
    
    boxplot(literalvarname ~ Locations, data = both, ylab = varlabel, col = colorfills,
            main = paste0(maintitle, "\nDistribution over sites, not population weighted quantiles!"), ...)
    points(1:2, bothmeans, col = 'black', pch = 22, bg = "white", cex = 3)
    abline(h = bothmeans[1], col = "white")
    abline(h = bothmeans[2], col = 'white')
    points(jitter(1 + ("Near these sites" == both.sample$Locations)), both.sample$literalvarname, pch = 20, col = "darkgray", cex = 0.7) # pch = "."
    
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

plot_box_per_column <- function(dt) {
  
  data.table::sh
}

