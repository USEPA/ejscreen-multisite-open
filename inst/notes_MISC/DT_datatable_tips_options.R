# useful settings for datatables 
# https://clarewest.github.io/blog/post/making-tables-shiny/

# datatable(
#   mydataframe,
#   options = list(
#     paging = TRUE,    ## paginate the output
#     pageLength = 15,  ## number of rows to output for each page
#     scrollX = TRUE,   ## enable scrolling on X axis
#     scrollY = TRUE,   ## enable scrolling on Y axis
#     autoWidth = TRUE, ## use smart column width handling
#     server = FALSE,   ## use client-side processing
#     dom = 'Bfrtip',
#     buttons = c('csv', 'excel'),
#     columnDefs = list(list(targets = '_all', className = 'dt-center'),
#                       list(targets = c(0, 8, 9), visible = FALSE))
#   ),
#   extensions = 'Buttons',
#   selection = 'single', ## enable selection of a single row
#   filter = 'bottom',    ## include column filters at the bottom
#   rownames = FALSE      ## don't show row numbers/names
# )

# Heatmap-like fill effect:
## Colour and values for table colour formatting
# brks <- seq(5, 320000, 1000)
# clrs <- colorRampPalette(c("white", "#6baed6"))(length(brks) + 1)
# 
# datatable(  
#   asdf...
#   ) %>%
#   formatStyle(c("sell_value", "buy_value"), backgroundColor = styleInterval(brks, clrs))

# abbreviate long values and show the full text in a tooltip.
# 
# To do this, you can use JavaScript to format the column to show a substring with “…” 
# and the full string in a tooltip (<span title="Long string">Substring...</span) 
# when values are longer than N characters (in this case 10 characters). 
# You can do this using columnDefs and pass JavaScript with the JS() function:
#   
# datatable(pic_items,
#           options = list(
#  scrollX = TRUE,
#  columnDefs = list(
#   list(
#     targets = 1,
#     render = JS(
#       "function(data, type, row, meta) {",
#       "return type === 'display' && data.length > 10 ?",
#       "'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
#       "}")))),
# escape = FALSE,
# rownames = FALSE)
