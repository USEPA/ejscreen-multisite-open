
# formulas_d  - a vector of formulas as text strings like "a = 1 + b" see data-raw/datacreate_formulas.R

# formula_varname()  - utility to get the names of variables created by the formulas
# calc_ejam()  which uses  calc_byformula()  -  read the formulas and execute them 

# source_this_codetext()  - allows execution of any code in the text
# source_this_codetext_careful() - allows execution of only functions explicitly allowed
######################################## #


######################################## #
## example showing how it works
#
# formula_varname(formulas_d)
# 
# bgdf <- data.frame(blockgroupstats)
# newdf <- ejscreen::ejscreen.acs.calc(bgdf,
#                                      keep.old = c('bgid', 'pop', 'hisp'),
#                                      keep.new = "all",
#                                      formulas = formulas_d
#                                      )
# round(t(newdf[1001:1002, ]), 3)
# 
# cbind(
#   newdf[11001:11020, c('hisp', 'pop', 'pcthisp')],
#   check = (newdf$hisp[11001:11020] / newdf$pop[11001:11020])
# )
# ## note the 0-100 percentages in blockgroupstats versus the 0-1 calculated percentages
#  rm(bgdf, newdf)
######################################## #
###   example:
# source_this_codetext_careful("(a+b)^c - d", list(a = 1, b = 2, c = 3, d = 4))
# # [1] 23
# formulas_d[1] 
# source_this_codetext_careful(
#   "1 * ifelse(pop == 0, 0, over64 / pop)", 
#   data_list = list(pop = blockgroupstats$pop[10001:10003], over64 = blockgroupstats$over64[10001:10003])
# )
# source_this_codetext_careful("list.files()", list())
# # Error in list.files() : could not find function "list.files"
# 
# source_this_codetext_careful("list.files()", list(),
#    eval_envir = rlang::new_environment(list("list.files" = list.files)))
# 
# # succeeds in listing my files if i explicitly allow it
######################################## #
### example using just 10 block groups from 1 county in Delaware 
#  c1 <- fips2countyname(fips_counties_from_state_abbrev('DE'), includestate = F)[1]
#  bgdf = data.frame(EJAM::blockgroupstats[ST == "DE" & countyname == c1, ])[1:10, ]
# 
#  newdf <- calc_ejam(bgdf, keep.old = "",
#    formulas = c(
#      "my_custom_recalc_demog <- (pctlowinc + pctmin)/2",
#      "mystat2  = 100 * pctlowinc"))
# cbind(Demog.Index = bgdf$Demog.Index, newdf, pctlowinc = bgdf$pctlowinc)
#
# newdf <- calc_ejam(bgdf, formulas = formulas_d)
# newdf
######################################## #




source_this_codetext_careful <- function(text_expression, data_list, eval_envir = NULL) {

  # DRAFT WORK NOT COMPLETED 
  
  # # A safer implementation was in this example, which 
  #  allows only arithmetic functions explicitly enabled:
  # 
  #  # found at https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
  #  ##### eval_text_expression <- function(text_expression, data_list, eval_envir = NULL) {
  
  # argument checks
  stopifnot(is.character(text_expression) && length(text_expression) == 1)
  stopifnot(is.list(data_list))
  stopifnot(length(data_list) == 0 || (!is.null(names(data_list)) && all(names(data_list) != "")))
  stopifnot(all(!(lapply(data_list, typeof) %in% c('closure', 'builtin'))))
  stopifnot(is.null(eval_envir) || is.environment(eval_envir))
  # default environment for convenience
  if (is.null(eval_envir)) {
    arithmetic_funcs <- list("+" = `+`, "-" = `-`, "*" = `*`, "/" = `/`, "^" = `^`, "(" = `(`, "==" = `==`)
    
    ### EXAMPLES OF OTHER FUNCTIONS TO ENABLE ??? 
    arithmetic_funcs <- c(arithmetic_funcs, list("sum" = sum, "min" = min, "max" = max, "sqrt" = sqrt, "ifelse" = ifelse)) 
    
    eval_envir = rlang::new_environment(data = arithmetic_funcs, parent = rlang::empty_env())
  }
  # load data objects into evaluation environment, then evaluate expression
  eval_envir <- list2env(data_list, envir = eval_envir)
  eval(parse(text = text_expression, keep.source = FALSE), eval_envir)
}
######################################## #


calc_byformula <- function(mydf, formulas = NULL, keep = formula_varname(formulas)) {
  
  
  # DRAFT WORK NOT COMPLETED 
  
  
  # keep is useful if some of the formulas are just interim steps
  # creating evanescent variables created only for use in later formulas and not needed after that.

  if (is.null(formulas)) {
      stop("no formulas specified or found, so no calculation done")
    }
  formulas <- trimws(formulas)
  formulas <- formulas[!is.na(formulas)]
  #  cat('\n formulas: ', formulas,'\n\n')
  #  cat('\n keep: ', keep,'\n\n')
  
  attach(mydf)
  on.exit(detach(mydf))
  
    for (thisformula in formulas) {
      
      # *** Need to handle cases where formula tries to use some variable that was not a colname in mydf 
      #   but is in the search path, like in the calling envt.
      
      # May need to more carefully specify environment, as in 
      
      #  as we probably do not want formulas to use variables defined globally or where this function was called from, right? 
      # If not careful about the environment in which things are evaluated, 
      # A problem arises if a formula relies on a variable that is not in mydf but is used by some loaded package,
      # for example if mydf does not contain the field called cancer but the survival package is attached and provides a dataset lazyloaded called cancer,
      #  the formula appears to work, but it uses the cancer from the survival package instead of the intended but missing mydf$cancer
      # So we could check the formula's inputs to see that they are all in mydf, 
      
      #  or are the outputs of earlier formulas that would work!
      
      #    textform <- strsplit(gsub('.*<-', '', parse(text = thisformula)), split=' |[[:punct:]]| ' )[[1]]
      #    textform <- textform[textform != '']
      # that gets the words from the right side of the formula, but only if it is written as ....<-....
      # and it also gets functions like sum, min, max, mean, etc. so it can't really be used to check if each variable is in mydf since some are functions not variables in the formula.
      # and it splits up on _ etc.
      ## Also might consider package called  sourcetools that has a tokenize function to parse text into parts

      
      y <- try(source_this_codetext(thisformula), silent = TRUE)
      # tries to evaluate formula in THIS environment (the one that called the function source_this... )
      suppressWarnings(
        if (class(y) == "try-error") {
          cat('Cannot use formula: '); cat(thisformula, '\n') # print(as.character(parse(text=thisformula)))
        } else {
          cat(paste0('Using ', thisformula, '\n'))
        }
      )
    }
    
    # RETURN ONLY THE ONES SUCCESSFULLY CREATED, OUT OF ALL REQUESTED TO BE KEPT

    keep.from.mydf <- keep[keep %in% names(mydf)]
    keep.other <- keep[!(keep %in% keep.from.mydf)]
    if (length(keep.other) > 0) {keep.other <- keep.other[keep.other %in% ls()] }
    
    # COULD ADD WARNINGS HERE ABOUT VARIABLES USER ASKED TO KEEP THAT DO NOT EXIST
    if (length(keep.from.mydf) > 0) {
    if (length(keep.other) > 0) {
      outdf <- data.frame( mydf[ , keep.from.mydf, drop = FALSE], 
                           mget(keep.other), 
                           stringsAsFactors = FALSE)
    } else {
      outdf <- data.frame( mydf[ , keep.from.mydf, drop = FALSE], stringsAsFactors = FALSE)
    }
    } else {
      if (length(keep.other) > 0) {
        outdf <- data.frame(mget(keep.other), 
                            stringsAsFactors = FALSE)
      } else {
        outdf <- NULL
      }
}
    return(outdf)
}
################################################################ #



calc_ejam <- function(bg,
                      # folder = getwd(),
                      keep.old = c("bgid", "pop"),
                      keep.new = 'all',
                      # formulafile,
                      formulas) {
  
  if (is.null(formulas)) {
    if (exists("formulas_d")) {
      formulas <- formulas_d
    } else {
      warning("no formulas specified or found, so no calculation done")
      return(NULL)
    }
  }
  
  # if (!missing(formulafile) &
  #     !missing(formulas)) {
  #   stop('Cannot specify both formulafile and formulas.')
  # }
  
  # if (missing(formulafile) & missing(formulas)) {
  #   #both missing so use default built in formulas and fieldnames
  #   x <- ejscreen::ejscreenformulas  # lazy loads as data
  #   myformulas <- x$formula
  # }
  
  # if (!missing(formulafile) & missing(formulas)) {
  #   if (!file.exists(file.path(folder, formulafile))) {
  #     stop(paste(
  #       'formulafile not found at',
  #       file.path(folder, formulafile)
  #     ))
  #     # x <- ejscreen::ejscreenformulas  # or could lazy load as data the defaults here
  #   } else {
  #     x <-
  #       read.csv(file = file.path(folder, formulafile),
  #                stringsAsFactors = FALSE)
  #     myformulas <- x$formula
  #   }
  # }
  
  # if (missing(formulafile) & !missing(formulas)) {
  myformulas <- formulas
  ## could add error checking here
  # }
  
  if (keep.old[1] == 'all') {
    keep.old <- names(bg)
  }
  # don't try to keep fields not supplied in bg
  keep.old <- keep.old[keep.old %in% names(bg)]
  if (keep.new[1] == 'all') {
    keep.new <- formula_varname(myformulas) # tries to keep all of those formulas would try to create
  }
  bg <- calc_byformula(bg, formulas = myformulas, keep = c(keep.old, keep.new))
  # if any of these are not successfully created by calc_byformula(), they just won't be returned by that function.
  return(bg)
}
################################################################ #


formula_varname <- function(myforms) {
  return(
    gsub("^([^ <=]*)[ <=].*", "\\1", trimws(myforms))
  )
  
  # formula_varname(c("z=10", "b<- 1", "c <- 34", " h = 1+1", "   q=2+2"))
  # formula_varname(formulas_d)
  # > length(formulas_d)
  # [1] 40
  # > length(formula_varname(formulas_d))
  # [1] 40
  
}
################################################################ #


source_this_codetext <- function(codetext, env = parent.frame()) {
  
  # useful tool because it is hard to remember how to do this:   eval(parse(text = 
  # and that can be useful because we...
  
  # may want to store formulas for indicators as a table of metadata 
  # (rather than formulas being buried in code like doaggregate 
  # or even pulled out into a function focused on just that)
  
  # and/or want to allow user to specify a custom indicator or summary stat via formula they provide as text,
  # so EJAM could then aggregate and report the new stat alongside the built-in indicators.
  
  # see datacreate_formulas.R 
  
  return(
    eval(parse(text = codetext), envir = env)
  )

  ## EJAM::calc_ejam() which uses calc_byformula() 
  ## was based on 
  ## ejscreen::ejscreen.acs.calc() which used analyze.stuff::calc.fields() 
  ##
  # https://rdrr.io/github/ejanalysis/ejscreen/man/ejscreen.acs.calc.html
  # or more generally here
  #  https://rdrr.io/github/ejanalysis/analyze.stuff/man/calc.fields.html
  #  stored at
  #  https://github.com/ejanalysis/analyze.stuff
  #  https://github.com/ejanalysis/analyze.stuff/blob/36afe6b102cb2cef90b87a48dfea9479b1a2447a/R/calc.fields.R
  #  devtools::install_github("ejanalysis/analyze.stuff")
  #  ?analyze.stuff::calc.fields()
  # 
  # example using just 10 block groups from 1 county in Delaware 
  #   c1 <- fips2countyname(fips_counties_from_state_abbrev('DE'), includestate = F)[1]
  #   bgdf = data.frame(EJAM::blockgroupstats[ST == "DE" & countyname == c1, ..names_d])[1:10, ]
  #    newdf <-  ejscreen::ejscreen.acs.calc(bgdf, keep.old = "", keep.new = c("my_custom_stat", "mystat2"), formulas = c(
  #      "my_custom_stat <- (pctlowinc + pctmin)/2", 
  #      "mystat2  = 100 * pctlowinc"))
  # newdf
  
}
################################################################ #
