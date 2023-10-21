rm(list = ls())
golem::detach_all_attached()
library(EJAM)

# To test just getblocksnearby() and doaggregate() rather than ejamit()

speeds = EJAM::speedtest(n = 100, radii = c(1,3.1,5,6.2,10))


# To test ejamit() 


################################################################################
# first time if had not loaded datasets or done index

rad <- 1

system.time({out = EJAM::ejamit(
  testpoints_100, 
  radius = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})
################################################################################
# subsequently 

################
rad <- 1

system.time({out = EJAM::ejamit(
  testpoints_100, 
  radius = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})
system.time({x = ejam2excel(ejamitout = out, radius_or_buffer_in_miles = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})

system.time({out = EJAM::ejamit(
  testpoints_1000, radius = rad,
  ok2plot = FALSE, save_now = FALSE, interactive_console = F)})
system.time({x = ejam2excel(ejamitout = out, radius_or_buffer_in_miles = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})

################
rad <- 3.1

system.time({out = EJAM::ejamit(
  testpoints_100,
  radius = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})
system.time({x = ejam2excel(ejamitout = out, radius_or_buffer_in_miles = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})

system.time({out = EJAM::ejamit(
  testpoints_1000, 
  radius = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})
system.time({x = ejam2excel(ejamitout = out, radius_or_buffer_in_miles = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})

#################
rad <- 5

system.time({out = EJAM::ejamit(
  testpoints_100, 
  radius = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})
system.time({x = ejam2excel(ejamitout = out, radius_or_buffer_in_miles = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})

system.time({out = EJAM::ejamit(
  testpoints_1000, 
  radius = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})
system.time({x = ejam2excel(ejamitout = out, radius_or_buffer_in_miles = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})

#################
rad <- 6.2

system.time({out = EJAM::ejamit(
  testpoints_100, 
  radius = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})
system.time({x = ejam2excel(ejamitout = out, radius_or_buffer_in_miles = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})

system.time({out = EJAM::ejamit(
  testpoints_1000, 
  radius = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})
system.time({x = ejam2excel(ejamitout = out, radius_or_buffer_in_miles = rad, ok2plot = FALSE, save_now = FALSE, interactive_console = F)})


#################
