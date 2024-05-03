

#' Barplot of ratios of demographic (or other) scores to averages - simpler syntax
#'
#' @param out like from [ejamit()]
#' @param varnames vector of indicator names that are ratios to avg, like 
#'   c(names_d_ratio_to_avg , names_d_subgroups_ratio_to_avg)
#'   but could be c(names_d_ratio_to_state_avg , names_d_subgroups_ratio_to_state_avg)
#' @param main title of plot - must change to note it vs. State if not comparing to US avg.
#' @param ... passed to [plot_barplot_ratios()]
#' @examples 
#' 
#' # Check a long list of indicators for any that are elevated
#' 
#' plot_barplot_ratios_ez(testoutput_ejamit_100pts_1miles,
#'   varnames = names_these_ratio_to_avg,
#'   main = "Envt & Demog Indicators at Selected Sites Compared to State Averages")
#'   
#' plot_barplot_ratios_ez(testoutput_ejamit_100pts_1miles,
#'   varnames = names_these_ratio_to_state_avg,
#'   main = "Envt & Demog Indicators at Selected Sites Compared to State Averages")
#' 
#' # Demographics only
#' 
#' # vs nationwide avg
#' plot_barplot_ratios_ez(testoutput_ejamit_100pts_1miles)
#' 
#' # vs statewide avg
#' plot_barplot_ratios_ez(testoutput_ejamit_1000pts_1miles, 
#'   varnames = c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg),
#'   main = "Demographics at Selected Sites Compared to State Averages")
#' 
#' # Environmental only
#' 
#' plot_barplot_ratios_ez(testoutput_ejamit_100pts_1miles,
#'   varnames = c(names_e_ratio_to_avg, names_e_ratio_to_state_avg),
#'   main = "Environmental Indicators at Selected Sites Compared to Averages")
#'   
#' @return ggplot
#'
#' @export
#'
plot_barplot_ratios_ez = function(out, varnames = c(names_d_ratio_to_avg , names_d_subgroups_ratio_to_avg),
                                  main = "Demographics at the Analyzed Locations Compared to US Overall", ...) {

  plot_barplot_ratios(unlist(out$results_overall[ , varnames, with = FALSE]), main = main, ...)
  # plot_barplot_ratios(unlist(out$results_overall[ , c(..names_d_ratio_to_avg , ..names_d_subgroups_ratio_to_avg) ]))
  }
############################################################################################# #


#' Barplot of ratios of demographic (or other) scores to averages (or other references)
#'
#' @details
#'
#'   **SOME GENERAL NOTES, DURING EJAM DEVELOPMENT**
#'
#'   For plots in general, see:
#'
#'   - <https://echarts4r.john-coene.com/articles/themes.html>
#'   - <https://exts.ggplot2.tidyverse.org/gallery>
#'
#'
#'   **For BARPLOTS, see/ merge/consolidate:**
#'
#'   - output$view1_summary_plot <- renderPlot({v1_summary_plot()}) and v1_summary_plot <- reactive({ })
#'     in EJAM server for Short Report if  bar type
#'   - output$summ_display_bar <- renderPlot({  }) contains its own plot code not a reactive
#'     in EJAM server for tab showing barplots in Detailed Results
#'   - plot_barplot_ratios() drafted function in EJAM
#'
#'
#'   **For BOXPLOTS, see:**
#'
#'   - v1_summary_plot <- reactive({ })     and output$view1_summary_plot <- renderPlot({v1_summary_plot()})
#'      - in EJAM server for SHORT report if box type, and
#'      - in EJAM server for LONG report passed as a parameter
#'   - boxplots_ratios()   in EJAMejscreenapi
#'      (NOT in EJAM server for Detailed Results interactive views)
#'   - ejscreenapi_script() code also relevant? in EJAMejscreenapi
#'   - box/scatter examples in ggplot, <https://r-graph-gallery.com/89-box-and-scatter-plot-with-ggplot2.html>
#'   - boxplots in base R, <https://www.r-bloggers.com/2023/09/how-to-reorder-boxplots-in-r-a-comprehensive-guide>
#'
#'   **For HISTOGRAMS, see:**
#'
#'   - output$summ_display_hist <- renderPlot   in EJAM server for interactive views
#'   - the histograms code and discussion  in EJAMbatch.summarizer package
#'
#'
#' @param ratio.to.us.d.overall named list of a few ratios to plot, but see [plot_barplot_ratios_ez()]
#'   for an easier way to specify which indicator to show.
#' @param names2plot_friendly names to use for plot - should be same length as named list ratio.to.us.d.overall
#' @param mycolorsavailable leave as default
#' @param main title for plot, like "Demographics at the Analyzed Locations Compared to US Overall"
#' @examples
#'   
#'   plot_barplot_ratios_ez(testoutput_ejamit_100pts_1miles)
#'   
#'   plot_barplot_ratios(unlist(testoutput_ejamit_1000pts_1miles$results_overall[ , c(..names_d_ratio_to_avg , ..names_d_subgroups_ratio_to_avg) ]))
#'
#' @seealso [table_xls_format()] [plot_barplot_ratios]
#' @return ggplot should be returned
#' @export
plot_barplot_ratios <- function(ratio.to.us.d.overall,
                                names2plot_friendly = NULL,
                                mycolorsavailable=c("gray", "yellow", "orange", "red"),
                                main = "Demographics at the Analyzed Locations Compared to US Overall") {
  
  

  # ratio.to.us.d.overall <-   unlist(  out$results_overall[ , c(..names_d_ratio_to_avg, ..names_d_subgroups_ratio_to_avg )]  )
    # ratio.to.us.d.overall <- ratio.to.us.d()  # reactive already available
  # if ( all.equal(  names(ratio.to.us.d.overall), c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg))) {
  #
  # #supershortnames <- substr(gsub(" |-|age","",gsub("People of Color","POC", names2plot_friendly)),1,6)

  # }
  if (is.null(names2plot_friendly)) {
    names2plot_friendly <- fixcolnames(names(ratio.to.us.d.overall), oldtype = "r", newtype = "shortlabel")
    supershortnames <- gsub(' \\(.*', '', gsub("People of Color","POC", names2plot_friendly))
    names(ratio.to.us.d.overall) <- supershortnames
  }
    names(ratio.to.us.d.overall) <- names2plot_friendly


  ratio.to.us.d.overall[is.infinite(ratio.to.us.d.overall)] <- 0
  # use yellow/orange/red for ratio >= 1x, 2x, 3x  #  work in progress
  mycolors <- mycolorsavailable[1 + findInterval(ratio.to.us.d.overall, c(1.01, 2, 3))]

  # barplot(ratio.to.us.d.overall,
  #         main = 'Ratio vs. US Average for Demographic Indicators',
  #         cex.names = 0.7,
  #         col = mycolors)
  # abline(h=1, col="gray")

thisdata <-  data.frame(name = names(ratio.to.us.d.overall),
             value = ratio.to.us.d.overall,
             color = mycolors) %>%
    ## drop any indicators with Inf or NaNs
    dplyr::filter(is.finite(value))

thisdata$name <- factor(thisdata$name, levels = thisdata$name)


thisplot <- thisdata %>%
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = color)) +
    ggplot2::geom_bar(stat = 'identity') +
    ## way to add legend in future - needs tweaking
    #ggplot2::scale_fill_identity(guide='legend', labels = c('gray'='0-1','yellow'='1-2', 'orange'='2-3', 'red'='> 3'),) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = NULL, y = 'Ratio vs. Average', #fill = 'Legend',
                  title = main) +
    #scale_x_discrete(labels = scales::label_wrap(7)) +    # requires scales package
    #scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +

  # add horizontal line at ratio = 1
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1)) +

    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05), add = c(0, 0))) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0,100,0,0), "points"),
                   plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 10 , angle = -30, hjust = 0, vjust = 1)) + #
    NULL

return(thisplot)

  # ggplot2::ggplot(
  #   ratio.to.us.d.overall,
  #   aes(x = indicator, y = value)
  # ) +
  #   geom_boxplot() +
  #   geom_hline(aes(yintercept = 1)) +
  #   labs(x = "",
  #        y = "Ratio of Indicator values for avg. person in selected locations\n vs. US average value",
  #        title = 'Ratio vs. US Average for Demographic Indicators')
}
