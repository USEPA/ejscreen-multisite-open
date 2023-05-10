# EXAMPLES OF USING gtExtras package for plots ####

# for gtExtras, see https://thomasmock.quarto.pub/gtextras-overview

# advice on plots, tables: see http://www.stephen-few.com/smtn.php for great advice on (tables and) plots

#  Advanced dplyr tidy evaluation (used in tidyverse 
#   and in gtExtras, 
#   like to pipe params via 2braces {{blah}} or 3dots ...)
#    see https://dplyr.tidyverse.org/articles/programming.html 
#  which makes interactive data exploration fast and fluid, 
#  but adds challenges when you attempt to use them indirectly such as in a for loop or a function. 
# 1. Data masking lets you just say myvar not dataset$myvar
# 2. Tidy selection lets you easily choose variables based on their position, name, or type.


library(dplyr)
library(tidyr)
library(gt)
library(palmerpenguins)

# COMPARING TWO HISTOGRAMS / DENSITY PLOTS NICELY OVERLAID ####

pen_qu <- penguins |>
  na.omit() |>
  dplyr::group_by(species, sex) |>
  dplyr::summarise(
    quantile = list(
      stats::quantile(body_mass_g,
               c(0.25, 0.5, 0.75),
               q = c(0.25, 0.5, 0.75)
      )
    ),
    plot = list(body_mass_g),
    .groups = "drop"
  ) |>
  #   but also note data.table::dcast() and data.table::melt()
  tidyr::unnest_wider(quantile) |>
  dplyr::arrange(species, desc(sex))


gt(pen_qu,
   rowname_col = "sex",
   groupname_col = "species"
) |>
  gtExtras::gt_plt_dist(
    plot, 
    fig_dim = c(6,40)) |>
  fmt_number(
    where(is.numeric), 
    decimals = 1
  )
 
# PERCENTILES COMPARED IN A PLOT ####

dot_plt <- dplyr::tibble(x = c(seq(10, 90, length.out = 5))) %>%
  gt() %>%
  gt_duplicate_column(x, dupe_name = "dot_plot") %>%
  gt_plt_percentile(dot_plot)


# your own style ####

gt_theme_basic <- function(gt_object, ...){
  
  gt_object %>%
    tab_options(
      heading.align = "left",
      data_row.padding = px(7),
      column_labels.font.size = px(12),
      ...
    ) %>%
    tab_style(
      style = cell_text(
        color = "darkgrey",
        font = google_font("Source Sans Pro"),
        transform = "uppercase"
      ),
      locations = cells_column_labels(everything())
    )
}

# gt + PLOT 

library(ggplot2)

plot_object <-
  ggplot(
    data = gtcars,
    aes(x = hp, y = trq, size = msrp)
  ) +
  geom_point(color = "blue") +
  theme(legend.position = "none")
plot_object

# but pre-made: 

# gtExtras + Plots
# 3 step process:
  
  # list() data by row/group
# For specific column, create a tiny ggplot2 graph and save to disk
# Read image back in, and embed image into HTML
# Step infinity: protect against user-input/error handling



