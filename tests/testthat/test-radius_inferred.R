
## to try 50 trials for example, of 100 sites per analysis, to see how often function infers radius accurately
for (i in 1:50) {
  x <- getblocksnearby(
    testpoints_n(100), 
    radius = 6.246 , quiet = T)
  print(
    # system.time(
      inferred_radius(x)
      # )
    )
  }
