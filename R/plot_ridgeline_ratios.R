
#' Make ridgeline plot of ratios of demographic score to its average
#' 
#' @param ratio.to.us.d.overall named list of a few ratios to plot (data.frame)
#' @param names2plot_friendly names to use for plot - should be same length as named list ratio.to.us.d.overall
#' 
plot_ridgeline_ratios <- function(ratio.to.us.d.bysite, names2plot_friendly = NULL) {
  # if (is.null(dim(ratio.to.us.d.bysite))) {
  #   # seems like only 1 variable as vector ?
  #   ratio.to.us.d.bysite <- data.frame(Indicator = ratio.to.us.d.bysite)
  # }
  if (is.null(names2plot_friendly)) {
    names2plot_friendly <- fixcolnames(names(ratio.to.us.d.bysite), oldtype = "r", newtype = "long")
    supershortnames <- gsub(' \\(.*', '', gsub("People of Color","POC", names2plot_friendly))
    names(ratio.to.us.d.bysite) <- supershortnames
  }
  
  ## assign column names (could use left_join like elsewhere)
  # names(ratio.to.us.d.bysite) <-  c(
  #   names_d_friendly, 
  #   names_d_subgroups_friendly
  # ) # long_names_d$var_names[match( names_d_fixed, long_names_d$vars)] #names_d_fixed and long_names_d no longer exist. use names_d_friendly, etc.
  
  
## pivot data from wide to long - now one row per indicator 
ratio.to.us.d.bysite <- ratio.to.us.d.bysite %>% 
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'indicator') %>% 
  ## replace Infs with NAs - these happen when indicator at a site is equal to zero
  dplyr::mutate(value = dplyr::na_if(value, Inf)) #%>%
# NOTE NOW ratio.to.us.d.bysite IS A tibble, not data.frame, and is in LONG format now. !!!

# ridgeline Plot - need to adjust xlim so max is about a ratio of 3.0 (or less if none are >=3x)
ggplot(ratio.to.us.d.bysite, aes(x = `value`, y = `indicator`, fill = ..x..)) +
  ggridges::geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  viridis::scale_fill_viridis(name = "Ratio to US Overall Value", option = "C") +
  labs(title = 'Ratio to US Overall for each Demographic Indicator across these Sites') +
  hrbrthemes::theme_ipsum() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

}

# https://r-graph-gallery.com/294-basic-ridgeline-plot.html#color
# https://r-graph-gallery.com/294-basic-ridgeline-plot.html#shape
#  (ggplot2)
#  (ggridges) # listed in DESCRIPTION file Imports
#  (viridis) # listed in DESCRIPTION file Imports
#  (hrbrthemes) # listed in DESCRIPTION file Imports
