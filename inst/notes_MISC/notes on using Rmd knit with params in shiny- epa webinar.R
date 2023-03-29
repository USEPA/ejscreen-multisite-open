Travis Linscome-Hatfield (Guest - 
contact through  R User group  Vega.

in console: 

# useful to not do shiny app first, just do Rmd work.
# create a paramslist object and test it:
testparamslist = list(blah= 2, asdfk=data.frame(lak;sdf))
# useful to check params list object like this: 
print(str(testparamslist))
saveRDS(testparamslist, file="testparamslist.RDS")

in the Rmd file:  like  EJAM/www/report.Rmd  or  EJAM/www/brief_summary.Rmd

```{r, as-is=TRUE}     # it often helps to say as is


readRDS("../testparamslist.RDS") # predefined version of parameters list, for testing without shiny yet, but you MUST comment out the params: section in header of .Rmd file
params <- testparameterslist

```

in the shiny app, data could be updated/filtered like this

data_updated <- reactive({
   x <- subset(x, asdflasfj)
})

# generate report
output:  # in header of rmd file
  html specified here 
In the app server code:
if (input$blah)  {paramslist = list("dat" = blah, "vehicles" = input$vehicles} else {paramslist= whatever}

# knit either format report depending on what user wants

if (input$reportformat == "RMD") {
  rmarkdown::render("../samplereport.Rmd", output_file=file, 
       params = paramslist,
       envir = new.env(parent = globalenv())) 
} else if (input$reportformat == "html", output_file=file,
        params = etc 
 