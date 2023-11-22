lint_equals <- function(x) {
  cat('see lintr package and the RStudio add-in that can be assigned e.g., ctrl-option-L  \n')
  x <- gsub("([^ |=])==", "\\1 ==", x)
  x <- gsub("==([^ |=])", "== \\1", x)
  # 
  x <- gsub("([^ |=])=", "\\1 =", x)
  x <- gsub("=([^ |=])", "= \\1", x)
  return(x)
  
#   ### example / test:
# 
#   testjunk <-
#     "
# 
# #  **lorem blah blah *
# #
# # x= 1
# # x =1
# #
# blah <- function(x) {
# 
#   x=sdf  ;   x=sdf
# 
#   x=s; y=f
# 
#   x =4; x =4,
# 
#   y= 5 ;   y= 7
# 
#   y==5 ;  y==5
# 
#   y ==5 ;  y ==5
# 
#   y== 5  ; y== 5
# 
#   y == 5 ;  y == 5
# 
# # = == = ==  ==2= =2== ==2== =2= 
# 
#   #   |==================================================|
#   "
# 
#   cat(testjunk, '\n')
# 
#   cat(lint_equals(testjunk), '\n')
# 
}
