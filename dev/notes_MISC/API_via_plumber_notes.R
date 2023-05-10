# 
# # Notes on using the R plumber package to provide an API
# 
# # https://www.rplumber.io/index.html
# # https://raw.githubusercontent.com/rstudio/cheatsheets/main/plumber.pdf
# 
# # what to offer via API, for example:
# #
# #  [to be updated...]
#  EJScreen API's output? table (or partial bits of) output of EJScreen-like buffering
# #    for at least 1 circular buffer at a point.
# #  ejscreenapi output: table of ej stats for a list of buffer sites
# #  map layer? 
# # batch.summarizer functions, like 
# #  - summary table or stats from batch summarize (e.g. % low income for avg person nearby)
# #  - graphics like plots of buffer results
# 
# 
# # how
# 
# # - write myapi.R code with #* annotations and the plumber package
# 
# # can pass parameters/etc. 3 ways:
# # 0. The endpoint (url.com/myfunc1) 
# # 1. A request route (url.com/myfunc1/3/subtype2) 
# # 2. A query (url.com?myparam=10&param2="hello") 
# # 3. A request body can be used to pass info about request
# 
# # # 0. specify endpoint function to handle a given get request, and it looks for a matching one
# # #* @get /myendpoint1
# # function(){
# #   ...
# # }
# # Visiting via browser or submitting GET to  theurl/myendpoint1  will return what that function returns
# 
# #* @get /myfunc1
# #  function(){
#   # paste0("this is what I am returning, just the date:", Sys.Date())
# # }
# # Visiting http://localhost:8000/myfunc1  will print that info.
# 
#   #  dynamic routes: 
#   # users <- data.frame(
#   #   uid=c(12,13),
#   #   username=c("kim", "john")
#   # )
#   # 
#   # #* Lookup a user
#   # #* @get /users/<id>
#   # function(id){
#   #   subset(users, uid %in% id)
#   # }
#   # request that via    /users/john
#   
#   #* @post /user
#   # function(req, id, name) {
#   #   # this would just return a list of what was requested in the post that included a request body:
#   #   list(
#   #     id = id,
#   #     name = name,
#   #     body = req$body,
#   #     raw = req$bodyRaw
#   #   )
#   # } 
# 
# # Query Strings: 
# # A query string may be appended to a URL in order to convey information 
# # beyond just the request route.
# # 
# # Request body allows for upload of a whole file but is harder to code.
# #   maximum size of a request body depends on client, proxies, etc. 
# #   but is typically at least 2MB – much larger than a query string. 
# #   This approach is most commonly seen with PUT and POST requests 
# 
# 
# # ?myparam1=somevalue&x=10 
# # The myparam1=somevalue portion of the URL sets the myparam1 parameter to somevalue, and x to 10.
# # More details on how Plumber processes inputs are available in https://www.rplumber.io/articles/routing-and-input.html 
# # https://raw.githubusercontent.com/rstudio/cheatsheets/main/plumber.pdf
# # 
# # Plumber will automatically forward information from the 
# # query string into the function being executed by aligning the 
# # name of the query string with the name of the function parameter. 
# # for example: 
# #
# #* @get /
# # search <- function(q="", pretty=0){
# #   paste0("The q parameter is '", q, "'. ",
# #          "The pretty parameter is '", pretty, "'.")
# # }
# # Visiting http://localhost:8000/?q=bread&pretty=1 will print that info.
# 
#  
#   
# #  myapiroot <- pr('myapi.R')
# #  myapiroot %>% pr_run()
# # If you’re running this code locally on your personal machine, you should be able to open 
# # http://localhost:8000/echo or http://localhost:8000/plot in a web browser to test your new API endpoints.
# # hosting:
# #  If you’re using a tool like RStudio Server to run your R code on a remote machine, 
# # see https://www.rplumber.io/articles/security.html#networking 
# 
# # Plumber will attempt to parse the request body in one using allowed parsers. 
# # Any fields provided in the message body will be passed through as parameters to the function.
# 
# #* Log some information about the incoming request
# #* @filter logger
# function(req){
#   cat(as.character(Sys.time()), "-",
#       req$REQUEST_METHOD, req$PATH_INFO, "-",
#       req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
#   plumber::forward()
# }
# 
# #* Echo the parameter that was sent in
# #* @param msg The message to echo back.
# #* @get /echo
# function(msg=""){
#   list(msg = paste0("The message is: '", msg, "'"))
# }
# 
# #* Plot out data from the iris dataset
# #* @param spec If provided, filter the data to only this species (e.g. 'setosa')
# #* @get /plot
# #* @serializer png
# function(spec){
#   myData <- iris
#   title <- "All Species"
#   
#   # Filter if the species was specified
#   if (!missing(spec)){
#     title <- paste0("Only the '", spec, "' Species")
#     myData <- subset(iris, Species == spec)
#   }
#   
#   plot(myData$Sepal.Length, myData$Petal.Length,
#        main=title, xlab="Sepal Length", ylab="Petal Length")
# }
