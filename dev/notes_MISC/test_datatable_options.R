# testing datatable options


# library(shiny)
# library(DT)
# shinyApp(
#   ui = fluidPage(fluidRow(column(12, DTOutput('tbl')))),
#   server = function(input, output) {
#     output$tbl = renderDT(
#       data.frame(iris,iris,iris), 
#       options = list(
#         
#         initComplete = JS('function(setting, json) { alert("done"); }'),
#         
#         scrollX = TRUE, 
#         scrollY = TRUE, # fails
#         fixedHeader = TRUE,  # fails
#         lengthChange = FALSE        
#
#         )
#       )
#   }
# )
# # }