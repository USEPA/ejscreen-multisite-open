# Create a sample data frame 
# 
# data <- data.frame(
#   StudentName = c("Leanord", "Penny", "Howard", "Bernadette", "Sheldon", "Raj", "Amy"), 
#   Class = rep("X", 7),
#   Section = c("B", "C", "A", "D", "A", "B", "B"),
#   Percentage = c(91.2, 63.5, 90.23, 92.7, 98.2, 88.1, 95.0) 
# )
# 
# # Use the pretty function to compute "pretty" breakpoints for the Percentage column 
# 
# pretty_breaks <- pretty(data$Percentage, n = 4)
# 
# # Use the formattable() function to create a stylish table with the "pretty" breakpoints 
# 
# formattable(data, list( Percentage = color_tile("white", "orange") ))
# 
