# Random points - avg facility or avg resident
#   or avg census block

# could be used in testing/ benchmarking
#  EJScreen/ejscreenapi vs EJAM vs other analyses

# also see   proxistat::testpoints()


    n <- 1e4 # sample size
    
    # RANDOM FACILITIES (EPA-regulated facilities in FRS)
    sites <- EJAMfrsdata::frs
    rownum <- sample(x=1:NROW(sites), size = n, replace = FALSE)
    pts_frs <- sites[rownum, ]
    
    # RANDOM PEOPLE (US RESIDENTS)  
    sites <- EJAMblockdata::blockwts
    rownum <- sample(x=1:NROW(sites), size = n, replace = FALSE, prob = EJAMblockdata::blockwts)
    pts_pop <- sites[rownum, ]
    # pts <-  testpoints(n=n, weighting = "people")
    
    # RANDOM BLOCKS
    sites <- EJAMblockdata::blockpoints
    rownum <- sample(x=1:NROW(sites), size = n, replace = FALSE)
    pts_block <- sites[rownum, ]
    
    
