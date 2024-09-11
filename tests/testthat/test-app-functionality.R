test_that("{shinytest2} recording: EJAM", {
  app <- AppDriver$new(
    variant = platform_variant(), 
    name = "EJAM", 
    seed=12345, 
    load_timeout=2e+05,
    options = list(shiny.reactlog = TRUE, shiny.trace = TRUE, shiny.testmode = TRUE)
  )
  
  print("About to upload")
  # app$upload_file(ss_upload_latlon = EJAM:::app_sys("testdata/latlon/testpoints_100.xlsx"))
  app$set_inputs(ss_choose_method = "dropdown", wait_ = FALSE)
  app$set_inputs(ss_select_naics = "1111", wait_ = FALSE)#, timeout_ = 10000)
  
  print("About to get results")
  app$wait_for_idle(timeout = 20000)
  app$click("bt_get_results", wait_ = TRUE, timeout_ = 20000)
  print("done pulling results")
  # write.csv(app$get_logs(), "/media/gdrive/silvermanA/EJAM-test/shinytest2_log.csv")
  
  print("taking a screenshot")
  app$expect_values()
  app$expect_screenshot()
  
  print("going to Site Selection tab")
  app$set_inputs(all_tabs = "Site Selection")
  app$set_inputs(bt_rad_buff = 1.5)
  
  print("set analysis title to Summary of EJ Analysis2")
  app$set_inputs(analysis_title = "Summary of EJ Analysis2")
  
  print("repulling results")
  app$click("bt_get_results", wait_ = TRUE, timeout_ = 20000)
  
  print("another screenshot after repulling")
  app$expect_values()
  app$expect_screenshot()
  
  print("about to do community download")
  app$wait_for_idle(timeout = 20000)
  # 8/16/24 - Can skip this
  app$expect_download("community_download_all")
  
  print("going to details tab")
  app$set_inputs(results_tabs = "Details")
  app$wait_for_idle(timeout = 20000)
  
  # print("downloading results table from details tab")
  # app$expect_download("download_results_table")
  # 
  # print("screenshotting that")
  # app$expect_values()
  # app$expect_screenshot()
  # # DETAILS > PLOT AVERAGE SCORES
  # print("going to plot_average details subtab")
  # app$set_inputs(details_subtabs = "plot_average")
  # app$expect_values()
  # app$expect_screenshot()
  # app$wait_for_idle(timeout = 20000)
  # 
  # print("Demographic summ_bar-ind")
  # app$set_inputs(summ_bar_ind = "Demographic")
  # app$expect_values()
  # app$expect_screenshot()
  # app$wait_for_idle(timeout = 20000)
  # 
  # print("EJ summ_bar-ind")
  # app$set_inputs(summ_bar_ind = "EJ")
  # app$expect_values()
  # app$expect_screenshot()
  # app$wait_for_idle(timeout = 20000)
  # 
  # print("EJ supplemental")
  # app$set_inputs(summ_bar_ind = "EJ Supplemental")
  # app$expect_values()
  # app$expect_screenshot()
  # app$wait_for_idle(timeout = 20000)
  # 
  # # DETAILS > PLOT RANGE OF SCORES
  # print("going to plot_range details subtab")
  # app$set_inputs(details_subtabs = "plot_range")
  # app$expect_values()
  # app$expect_screenshot()
  # app$wait_for_idle(timeout = 20000)
  # 
  # app$set_inputs(summ_hist_distn = "Sites")
  # app$expect_values()
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_data = "raw")
  # app$expect_values()
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_bins = 15)
  # app$set_inputs(summ_hist_bins = 20)
  # app$expect_values()
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_distn = "People")
  # app$expect_values()
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_data = "pctile")
  # app$expect_values()
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_data = "raw")
  # app$expect_values()
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "Demog.Index.Supp")
  # app$expect_values()
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "pctlowinc")
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "pctlingiso")
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "pctunemployed")
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "pctlths")
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "lowlifex")
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "pctunder5")
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "pctover64")
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "pctmin")
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "pcthisp")
  # app$set_inputs(summ_hist_ind = "pctnhba")
  # app$expect_screenshot()
  # app$set_inputs(summ_hist_ind = "EJ.DISPARITY.o3.supp")
  # app$set_inputs(results_tabs = "Community Report")
})