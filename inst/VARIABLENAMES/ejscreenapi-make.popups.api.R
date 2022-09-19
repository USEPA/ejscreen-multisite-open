make.popups.api <- function(out, linkcolname='pdfurl', linkcolname2=NULL, verbose=FALSE) {
  
  ############################################ #
  ############################################ #

  # SPECIFY VARIABLE NAMES TO USE, SIGNIF DIGITS, etc. ####
  
  ## **currently hard-coded to use EJScreen 2.0 variable names ####

  # temporarily replace the ejscreen and analyze.stuff and ejanalysis package functions and datasets to have this small self contained app not depend on those pkgs,
  # but some this code essentially is copied from those pkgs: 
  # maybe these can be stored in and derived from map_headernames (as csv or as data)
  names.d.api        <- c("VSI.eo", "pctmin", "pctlowinc", "pctlths","pctlingiso", "pctunder5", "pctover64", "pctunemployed")
  names.d.pctile.api <- paste0('pctile.', names.d.api) 
  names.d.nice.api   <- c("Demog.Ind.", "% Low-inc.", "% Minority", "% <High School", "% Linguistic Isol.", "% < age 5", "% > age 64", "% Unemployed")
  
  names.e.api        <- c("pm", "o3", "cancer", "resp", "dpm", "pctpre1960", "traffic.score", "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", "ust")
  names.e.pctile.api <- paste0('pctile.', names.e.api)  
  names.e.nice.api <- c(
    # NOTE THESE ARE A BIT SIMPLER/SHORTER THAN WHAT ejscreen PACKAGE HAS USED
    "PM2.5 level", "Ozone level", "Air toxics cancer risk", 
    "Air toxics respiratory HI", "Diesel PM", 
    "% pre-1960 housing (lead paint)", "Traffic score", 
    "Proximity to NPL sites", "Proximity to RMP facilities", 
    "Proximity to TSDF facilities", 
    "Proximity to discharge to water", "UST score"
  )
  esigfigs.api <- structure(list(
    sigfigs = c(3, 3, 2,   2, 3, 2, 2, 2, 2, 2, 2, 2), 
    evar = names.e.api), row.names = c(NA, 13L), class = "data.frame")
  popupunits.api <- structure(list(
    evar = names.e.api, 
    units = c("ug/m3", "ppb", "lifetime risk per million", "ratio", "ug/m3", "fraction built pre-1960", "daily vehicles/meters distance", "sites/km distance",
              "facilities/km distance", "facilities/km distance", "facilities/km distance", "UST indicator")
    ), row.names = c(NA, 12L), class = "data.frame")
  ############################################ #
  ############################################ #

  # functions to USE THOSE VARIABLE NAMES TO FORMAT NUMBER OF SIGNIFICANT DIGITS ETC. ####
  
  pctileAsText <- function(x) {
    # note: x should be a vector or data.frame of percentiles from 0 to 100, not 0 to 1
    result <-
      sapply(x, function(z) {
        paste(floor(z), '%ile', sep = '')
      })
    result[is.na(x)] <- NA
    return(result)
  }
  signifarray.api <- function(dat, digits = 6) {
    if (!(is.data.frame(dat))) {
      dat <- as.data.frame(dat)
    }
    y <- mapply(FUN = signif, x = dat, digits = digits)
    return(y)
  }
  ejscreensignifarray.api <- function(dat, digits = 'ejscreen') {
    digits <-
      esigfigs.api[match(colnames(dat), esigfigs.api$evar), 'sigfigs']
    return( signifarray.api(dat = dat, digits = digits))
  }
  
  # function to MAKE THE POPUP TEXT FOR JUST THE DEMOGRAPHIC INDICATORS ####
  
  make.popup.d.api <- function(d, pctile, prefix = 'pctile.text.', basenames) {
    if (missing(basenames)) {
      # might add code to handle cases like only one row, matrix not df, etc?
      basenames <- colnames(d)
    }
    
    x <-
      mapply(
        FUN = function(x, y) {
          paste(round(100 * x), '% (', pctileAsText(y), ')', sep = '')
        },
        d,
        pctile
      )
    x <- data.frame(x, stringsAsFactors = FALSE)
    colnames(x) <- paste(prefix, basenames, sep = '') 
    x[is.na(d)] <- NA
    rownames(x) <- rownames(d)
    return(x)
  }
  
  # function to MAKE THE POPUP TEXT FOR JUST THE ENVT INDICATORS ####
  
  make.popup.e.api <- function(e,
                               pctile,
                               prefix = 'pctile.text.',
                               basenames,
                               units,
                               sigfigs) {
    if (missing(basenames)) {
      # might add code to handle cases like only one row, matrix not df, etc?
      basenames <- colnames(e)
    }
    if (missing(sigfigs)) {
      sigfigs <- esigfigs.api$sigfigs[match(basenames, esigfigs.api$evar)]
      sigfigs[is.na(sigfigs)] <- 2
    }
    e <-
      data.frame(ejscreensignifarray.api(e, digits = sigfigs), stringsAsFactors = FALSE)
    
    #   print(e)
    #   print(str(e))
    
    if (missing(units)) {
      units <- popupunits.api$units[match(basenames, popupunits.api$evar)]
      units[is.na(units)] <- ''
    }
    x <-
      mapply(
        FUN = function(x, u, y) {
          paste(x, ' ', u, ' (', pctileAsText(y), ')', sep = '')
        },
        e,
        units,
        pctile
      )
    x <- data.frame(x, stringsAsFactors = FALSE)
    colnames(x) <- paste(prefix, basenames, sep = '') 
    x[is.na(e)] <- NA
    rownames(x) <- rownames(e)
    return(x)
  }

  # USE THE FUNCTIONS TO CREATE POPUPS ####
  
  # **************   ejscreen and other pkgs have been using VSI.eo but this pkg currently used the variable name or partname 'Demog.Index' instead
  names(out) <- gsub('Demog.Index', 'VSI.eo', names(out))
 
  poptext.d <-  make.popup.d.api(d = out[, names.d.api] / 100, pctile = out[, names.d.pctile.api], prefix = '')
  poptext.e <-  make.popup.e.api(e = out[, names.e.api],       pctile = out[, names.e.pctile.api], prefix = '')
  names(poptext.d) <- names.d.nice.api   
  names(poptext.e) <- names.e.nice.api  # ejscreen::names.e.nice  # longer form is   ejscreen::nicenames(names(poptext.e))
  
  linkify <- function(url, text) {
    paste0('<a href=\"', URLencode(url), '\", target=\"_blank\">', text, '</a>')
  }
  if (!(linkcolname %in% names(out))) {linkcolname <- 'EJScreenPDF'} # needed for ejscreenapi_plus() but not sure if for server.R
  if ('id'       %in% names(out)) {pops_id       <- paste0('id: ',       out$id,       '<br>')} else {pops_id       <- ''}
  if ('siteid'   %in% names(out)) {pops_siteid   <- paste0('siteid: ',   out$siteid,   '<br>')} else {pops_siteid   <- ''}
  if ('sitename' %in% names(out)) {pops_sitename <- paste0('sitename: ', out$sitename, '<br>')} else {pops_sitename <- ''}
  
  z <- paste0(
    '<b>',
    pops_id,
    pops_siteid,
    pops_sitename,
    '</b>',
    paste0('Area within ', out$radius.miles, ' miles of site', '<br>'),
    paste0('long, lat: ',  out$lon, ', ', out$lat,             '<br>'),
    '<b>', 'Demographic Indicators: ', '</b>',                 '<br>',
    'Population: ', prettyNum(out$pop, big.mark = ','),        '<br>',
    apply(poptext.d, FUN= function(x) paste0(names(x), ': ', x, collapse = '<br>'), MARGIN = 1), '<br>', 
    '<b>', 'Environmental Indicators: ', '</b>',               '<br>',
    apply(poptext.e, FUN= function(x) paste0(names(x), ': ', x, collapse = '<br>'), MARGIN = 1), '<br>',
    
    # LINK IN POPUP NOT YET WORKING:  and need to switch to popup for uploaded pts when upload is done.
    # toggle pts() vs results_table() as basis for popups as buttons clicked
    # 
    # linkify(out[ , linkcolname], 'EJScreen Report'),           '<br>',  # EJScreenPDF
    # linkify(out[ , linkcolname2], 'EJScreen Map'),
    
    sep = '<br>'
  )
  if (verbose) {printpopup(z)} #  was  cat(gsub('<br>','  <br>\n',z))  #  see in console   
  return(z) # invisible(z) did not seem to work for some reason

  # end
  ############################################################################# #  
  # notes on testing / sample data  ####
  if (FALSE) {
    # sample data ####
    
    testout2 <- structure(list(
      siteid = 1:2, sitename = c("example site 1", "example site 2"), lon = c(-91.132107, -91.09), lat = c(30.494982, 30.45), id = 1:2, 
      
      pctmin = c(96L, 49L), pctlowinc = c(75L, 24L), pctlths = c(23L, 7L), pctlingiso = c(1L, 1L), pctunder5 = c(8L, 5L), pctover64 = c(10L, 19L), pctunemployed = c(18L, 3L), 
      VSI.eo = c(86L, 37L), 
      pctpre1960 = c(0.22, 0.27), dpm = c(0.464, 0.479), cancer = c(51L, 58L), resp = c(0.7, 0.6), traffic.score = c(980L, 830L), proximity.npdes = c(0.41, 1.7), proximity.npl = c(0.091, 0.056), proximity.rmp = c(2, 0.62), proximity.tsdf = c(3.8, 2.1), o3 = c(38, 38.1), pm = c(10.2, 10.3), ust = c(2.9, 2.9), 
      
      state.avg.pctmin = c(41L, 41L), state.avg.pctlowinc = c(39L, 39L), state.avg.pctlths = c(15L, 15L), state.avg.pctlingiso = c(2L, 2L), state.avg.pctunder5 = c(7L, 7L), state.avg.pctover64 = c(15L, 15L), state.avg.unemployed = c(6L, 6L), 
      state.avg.VSI.eo = c(40L, 40L), 
      state.avg.pctpre1960 = c(0.2, 0.2), state.avg.dpm = c(0.298, 0.298), state.avg.cancer = c(41L, 41L), state.avg.resp = c(0.45, 0.45), state.avg.traffic.score = c(560L, 560L), state.avg.proximity.npdes = c(0.42, 0.42), state.avg.proximity.npl = c(0.086, 0.086), state.avg.proximity.rmp = c(0.91, 0.91), state.avg.proximity.tsdf = c(1.4, 1.4), state.avg.o3 = c(37.2, 37.2), state.avg.pm = c(9.22, 9.22), state.avg.ust = c(2L, 2L), 
      
      state.pctile.pctmin = c(93L, 63L), state.pctile.pctlowinc = c(94L, 27L), state.pctile.pctlths = c(79L, 28L), state.pctile.pctlingiso = c(67L, 67L), state.pctile.pctunder5 = c(66L, 41L), state.pctile.pctover64 = c(27L, 74L), state.pctile.unemployed = c(93L, 31L), 
      state.pctile.VSI.eo = c(96L, 52L), 
      state.pctile.pctpre1960 = c(70L, 76L), state.pctile.dpm = 81:82, state.pctile.cancer = c(87L, 92L), state.pctile.resp = c(98L, 96L), state.pctile.traffic.score = c(85L, 83L), state.pctile.proximity.npdes = c(93L, 96L), state.pctile.proximity.npl = c(72L, 53L), state.pctile.proximity.rmp = c(85L, 60L), state.pctile.proximity.tsdf = c(91L, 75L), state.pctile.o3 = c(64L, 67L), state.pctile.pm = c(93L, 98L), 
      
      state.pctile.ust = c(76L, 76L),  # name is ok here.----------------- *** *
      
      state.pctile.EJ.DISPARITY.pctpre1960.eo = c(90L, 36L), state.pctile.EJ.DISPARITY.dpm.eo = c(95L, 60L), state.pctile.EJ.DISPARITY.cancer.eo = c(96L, 56L), state.pctile.EJ.DISPARITY.resp.eo = c(98L, 57L), state.pctile.EJ.DISPARITY.traffic.score.eo = c(95L, 73L), 
      state.pctile.EJ.DISPARITY.proximity.npdes.eo = c(96L, 94L), state.pctile.EJ.DISPARITY.proximity.npl.eo = c(92L, 61L), state.pctile.EJ.DISPARITY.proximity.rmp.eo = c(95L, 69L), state.pctile.EJ.DISPARITY.proximity.tsdf.eo = c(97L, 74L), state.pctile.EJ.DISPARITY.o3.eo = c(94L, 55L), state.pctile.EJ.DISPARITY.pm.eo = c(95L, 56L), 
      
      state.pctile.EJ.DISPARITY.ust.eo = c(91L, 78L), # should be   state.pctile.EJ.DISPARITY.ust.eo ----------------- **************************************
      
      region.avg.pctmin = c(52L, 52L), region.avg.pctlowinc = c(36L, 36L), region.avg.pctlths = c(15L, 15L), region.avg.pctlingiso = c(6L, 6L), region.avg.pctunder5 = c(7L, 7L), region.avg.pctover64 = c(13L, 13L), region.avg.unemployed = c(5L, 5L), 
      region.avg.VSI.eo = c(44L, 44L), 
      region.avg.pctpre1960 = c(0.16, 0.16), region.avg.dpm = c(0.219, 0.219), region.avg.cancer = c(32L, 32L), region.avg.resp = c(0.37, 0.37), region.avg.traffic.score = c(470L, 470L), region.avg.proximity.npdes = c(0.5, 0.5), region.avg.proximity.npl = c(0.08, 0.08), region.avg.proximity.rmp = c(0.83, 0.83), region.avg.proximity.tsdf = c(0.8,  0.8), region.avg.o3 = c(41.1, 41.1), region.avg.pm = c(9.32, 9.32), region.avg.ust = c(2L, 2L), 
      
      region.pctile.pctmin = c(91L, 49L), region.pctile.pctlowinc = c(95L, 34L), region.pctile.pctlths = c(75L, 33L), region.pctile.pctlingiso = c(42L, 42L), region.pctile.pctunder5 = c(62L, 36L), region.pctile.pctover64 = c(41L, 79L), region.pctile.unemployed = c(96L, 33L), 
      region.pctile.VSI.eo = c(97L, 44L), 
      region.pctile.pctpre1960 = c(75L, 79L), region.pctile.dpm = 95:96, region.pctile.cancer = 97:98, region.pctile.resp = c(99L, 99L), region.pctile.traffic.score = c(88L, 86L), region.pctile.proximity.npdes = c(95L, 98L), region.pctile.proximity.npl = c(76L, 62L), region.pctile.proximity.rmp = c(89L, 60L), region.pctile.proximity.tsdf = c(97L, 89L), region.pctile.o3 = 29:30, region.pctile.pm = c(83L, 87L), 
      
      region.pctile.ust = 74:75,   #  # name is ok here -- FIRST COPY OF DUPLICATED COLUMN NAME --------------------- *** *****
      
      region.pctile.EJ.DISPARITY.pctpre1960.eo = c(87L, 22L), region.pctile.EJ.DISPARITY.dpm.eo = c(94L, 53L), region.pctile.EJ.DISPARITY.cancer.eo = c(92L, 49L), region.pctile.EJ.DISPARITY.resp.eo = c(94L, 50L), region.pctile.EJ.DISPARITY.traffic.score.eo = c(92L, 66L), region.pctile.EJ.DISPARITY.proximity.npdes.eo = c(97L, 94L), region.pctile.EJ.DISPARITY.proximity.npl.eo = c(86L, 53L), region.pctile.EJ.DISPARITY.proximity.rmp.eo = c(91L, 60L), region.pctile.EJ.DISPARITY.proximity.tsdf.eo = c(97L, 74L), region.pctile.EJ.DISPARITY.o3.eo = c(83L, 47L), region.pctile.EJ.DISPARITY.pm.eo = c(86L, 48L), 
      
      region.pctile.EJ.DISPARITY.ust.eo = c(85L, 69L),   #  # should be  region.pctile.EJ.DISPARITY.ust.eo --- 2d COPY OF DUPLICATED COLUMN NAME----------------- **************************************
      
      us.avg.pctmin = c(40L, 40L), us.avg.pctlowinc = c(31L, 31L), us.avg.pctlths = c(12L, 12L), us.avg.pctlingiso = c(5L, 5L), us.avg.pctunder5 = c(6L, 6L), us.avg.pctover64 = c(16L, 16L), us.avg.unemployed = c(5L, 5L), 
      us.avg.VSI.eo = c(36L, 36L), 
      us.avg.pctpre1960 = c(0.28, 0.28), us.avg.dpm = c(0.295, 0.295), us.avg.cancer = c(29L, 29L), us.avg.resp = c(0.36, 0.36), us.avg.traffic.score = c(710L, 710L), us.avg.proximity.npdes = c(12L, 12L), us.avg.proximity.npl = c(0.13, 0.13), us.avg.proximity.rmp = c(0.75, 0.75), us.avg.proximity.tsdf = c(2.2, 2.2), us.avg.o3 = c(42.6, 42.6), us.avg.pm = c(8.74, 8.74), us.avg.ust = c(3.9, 3.9), 
      
      pctile.pctmin = c(93L, 65L), pctile.pctlowinc = c(96L, 43L), pctile.pctlths = c(84L, 43L ), pctile.pctlingiso = c(50L, 50L), pctile.pctunder5 = c(71L, 45L), pctile.pctover64 = c(30L, 71L), pctile.pctunemployed = c(96L, 33L), 
      pctile.VSI.eo = c(98L, 60L), 
      pctile.pctpre1960 = c(56L, 60L), pctile.dpm = c(84L, 86L), pctile.cancer = c(99L, 99L), pctile.resp = 99:98, pctile.traffic.score = c(82L, 79L), pctile.proximity.npdes = c(90L, 94L), pctile.proximity.npl = c(63L, 46L), pctile.proximity.rmp = c(90L, 64L), pctile.proximity.tsdf = c(83L,71L), pctile.o3 = 21:22, pctile.pm = 86:87, 
      
      pctile.ust = c(66L, 66L),  # names is ok here -- FIRST COPY OF DUPLICATED COLUMN NAME --------------------- *** *****
      
      pctile.EJ.DISPARITY.pctpre1960.eo = c(86L, 43L), pctile.EJ.DISPARITY.dpm.eo = c(93L, 65L), pctile.EJ.DISPARITY.cancer.eo = c(96L, 64L), pctile.EJ.DISPARITY.resp.eo = c(97L, 65L), pctile.EJ.DISPARITY.traffic.score.eo = c(93L, 74L), pctile.EJ.DISPARITY.proximity.npdes.eo = c(95L, 93L), pctile.EJ.DISPARITY.proximity.npl.eo = c(88L, 65L), pctile.EJ.DISPARITY.proximity.rmp.eo = c(95L, 72L), pctile.EJ.DISPARITY.proximity.tsdf.eo = c(93L, 75L), pctile.EJ.DISPARITY.o3.eo = c(89L, 63L), pctile.EJ.DISPARITY.pm.eo = c(92L, 64L), 
      
      pctile.EJ.DISPARITY.ust.eo = c(86L, 76L),   # should be   pctile.EJ.DISPARITY.ust.eo  -- 2D COPY OF DUPLICATED COLUMN NAME----------------- **************************************
      
      ST = c("LA", "LA"), statename = c("LOUISIANA", "LOUISIANA"), REGION = c(6L, 6L), 
      pop = c(11970L, 6236L), NUM_NPL = c(0L,  0L), NUM_TSDF = c(2L, 0L), 
      statLayerCount = c(18L, 8L), statLayerZeroPopCount = c(0L, 0L), weightLayerCount = c(200L, 173L), timeSeconds = c(1.0818591, 0.7029361), radius.miles = c(1L, 1L), unit = c(9035L, 9035L), statlevel = c("blockgroup", "blockgroup"), inputAreaMiles = c(3.14, 3.14), 
      lon.1 = c(-91.132107, -91.09), lat.1 = c(30.494982, 30.45), 
      pdfurl = c(
        "https://ejscreen.epa.gov/mapper/EJSCREEN_report.aspx?namestr=&geometry={\"spatialReference\":{\"wkid\":4326},\"x\":-91.132107,\"y\":30.494982}&distance=1&unit=9035&areatype=&areaid=&f=report",
        "https://ejscreen.epa.gov/mapper/EJSCREEN_report.aspx?namestr=&geometry={\"spatialReference\":{\"wkid\":4326},\"x\":-91.09,\"y\":30.45}&distance=1&unit=9035&areatype=&areaid=&f=report"
      )),  class = "data.frame", row.names = c(NA, -2L))
    
    # IF NEED SAMPLE DATASET FOR TESTING ####
    #    example: 
    # out <- testout2
    # make.popups.api(out)
  }
  
  ############################################################################# #  
}
