# test script for feeding in points
# Author: Parker Malek

#load test points


localtree <- SearchTrees::createTree(
  EJAMblockdata::quaddata, treeType = "quad", dataType = "point"
)


radius <- c(1,3,10,20)

#time to create quadtree 1.116 sec


## This function just combines getblocksnearby() and doaggregate()

sitepoints <- read.csv(file="./data/SamplePoints100k.csv",stringsAsFactors=FALSE)

for(i in radius){
elapsed <- system.time({
  mysites2blocks <- EJAM::getblocksnearby(
    sitepoints=sitepoints,
    cutoff=i, maxcutoff=31.07,
    avoidorphans=TRUE,
    quadtree=localtree)

  out2 <- EJAM::doaggregate(sites2blocks = mysites2blocks)
  
  outsum <- EJAMbatch.summarizer::batch.summarize(
    sitestats = data.frame(data_processed()$results_bysite),
    popstats =  data.frame(data_processed()$results_bysite),
    ## user-selected quantiles to use
    #probs = as.numeric(input$an_list_pctiles),
    threshold = list(95) # compare variables to 95th %ile
  )



})
print(paste("Elapsed processing time for radius:",i))
#write time elapsed to csv
write.csv(t(data.matrix(elapsed)),file=paste0("./data/time_radius_",i,"_100k.csv"))
}

