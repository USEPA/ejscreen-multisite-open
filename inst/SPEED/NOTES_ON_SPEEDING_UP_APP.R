 "  NOTES_ON_SPEEDING_UP_APP

Speed improvements Tasks in order: 


**1 - Identify/ Prioritize slow actions from user point of view.**

Try using the app (basic steps seem sluggish like maps, short report, knitting, etc.). From user's perspective, Why speed is critical: If a site takes longer than 3 seconds to load, 53% of mobile users will abandon the site. If a site takes more than 5 seconds to load, the abandonment rate jumps to 90%.  A one-second delay in page load time can lead to a 7% decrease in conversions, a 16% decrease in customer satisfaction, and an 11% decrease in page views.   • 84% of customers will abandon a website or app if they experience slow load times or other performance issues, sources: https://drive.google.com/file/d/1qngIJToQxE_LExuqgjXXtoV25z2Ri1uQ/view  


**2 - Identify/prioritize slow, key lines of code.**

Do profiling of bottlenecks, before optimizing code - find out what steps are slow.
 Can use EJAM::speedtest(c(10,100), radii=c(1,5)) 
  for benchmarking ejamit() or getblocksnearby() and doaggregate()  or more simply something like:
   system.time({  x1=getblocksnearby(testpoints_1000,1);  save(x1,file = 'x1.rda');rm(x1)})
   system.time({  x3=getblocksnearby(testpoints_1000,3);  save(x3,file = 'x3.rda');rm(x3)})
   system.time({  x6=getblocksnearby(testpoints_1000,6);  save(x6,file = 'x6.rda');rm(x6)})
 
Profile overall, then closer look at key functions. See http://adv-r.had.co.nz/Profiling.html#improve-perf  and consider profvis, microbenchmark, etc. See getblocksnearby() and especially doaggregate() in particular. e.g., the one line of code in doaggregate() that does this: sites2blocks_overall <-  is very slow.  
also see on unit testing: https://www.r-bloggers.com/2023/04/unit-testing-analytics-code/
optimizing percentiles function that looks up what percentile each raw indicator score is in doaggregate() (IT IS SLOW)



**3 - Optimize code and app - see ideas below.**


**4 - Do loadtesting.**

see https://appsilon.com/shinyproxy-vs-posit-connect/ ShinyProxy vs Posit Connect: Benchmark Test for Scaling Shiny Apps (appsilon.com) and optimize further based on that. async methods probably needed when concurrent users. Do load testing and modify code to ensure app is highly responsive under anticipated load. The shinyload package may be useful.



  IDEAS FOR step 3. - HOW TO SPEED UP APP/ CODE
  
Consider strategies such as these (roughly in order of priority or feasibility): 

•	[Again, it is critical to do profiling to confirm what are bottlenecks, first, before trying to optimize anything!  
- Articles and resources on this: 
  - https://appsilon.com/speeding-up-r-shiny/
  - https://appsilon.com/scaling-and-infrastructure-why-is-my-shiny-app-slow/
  - https://www.r-bloggers.com/2023/04/lessons-learned-with-shiny-benchmark-improving-the-performance-of-a-shiny-dashboard/
•	Offer a spinner/ good progress bar where relevant, until speed improves.
•	Have fast basic elements of webpage/tab load asap, then slower ones afterwards.
•	Use reactive programming optimally - have the minimum of things refresh, like other tabs, using isolate() and bindcache functions, etc., use debouncing of sliders, etc.
•	Preload all large datasets early while user is not waiting (maybe using callr to do it in a separate process?). Decide if it is better to load large datasets at package loading time, via .onLoad() in aaa_onLoad_create_quad_tree_index.R, instead of when user does a query that triggers slow lazyloading? Wait will be either when pkg loads or when query is first done, right? These are the big ones: quaddata (168 MB), blockgroupstats  (54 MB), blockpoints (86 MB) and blockwts (31 MB), blockid2fips (20 MB) files
•	Consider how datasets are designed and used in the code, to avoid some joins or use memory more efficiently or avoid loading nonessential columns, etc.
•	Fix where doing the same thing twice - some code seemed duplicative in doaggregate(). 
•	remove some nonessential features/stats if they are bottlenecks. 
•	Get rid of for loops - Use vectorized functions (and not the apply family of functions). getblocksnearby() still has non-vectorized portions, and so does doaggregate() !
•	Precalculate constants or intermediate values if that saves time.
•	cache prior requests in earlier sessions. 
•	cache prior requests in current session. memo-izing?
•	Consider precalculating and being able to quickly offer final results for anticipated common queries! maybe 1 standard distance for most likely FRS sites?
•	Consider precalculating some slow step(s) within doaggregate()? It creates a new index vec and does some math that might be things that could be stored as data on FRS sites, eg.?
•	Find algorithm improvements/ change in approach (as opposed to how the code implements that algorithm)

•	Use data.table properly to avoid code that makes copies of data.tables (e.g., x <- df[asdf]) and instead use setorder, setcolumns, setnames, and `:=`() to change everything by reference 
•	Use data.table properly to do faster select/filter/join in data.table using the best approach (using keys properly,etc). 
•	Use collapse:: with data.table:: (not dplyr/tibble/data.frame), and maybe other pkgs from the fastverse as much as possible, to fix some bottlenecks, espec operations like these:
  o	Use the optimized versions of functions like fmean, fmin, fmax, funique (and maybe even could use fdist?)
  o Grouping/by=   see if faster to do grouping via collapse:: versus via data.table:: 
  o	* cbind(data, someFUN(data[,cols])   replace it with   add_vars(data) <- someFUN(get_vars(data, cols))
  o	cbind(data1, data2, data3, ...) is slow, 
  o	collapse::add_vars(data1, data2, data3, ...) much faster and preserves attributes of data1. 
  o	collapse::add_vars(data) <- someFUN(get_vars(data, cols, pos= ))  efficiently appends data with *computed* columns.
  o	data[cols] <- someFUN(data[cols])  replace with 10x more efficient collapse::get_vars(data, cols) <- someFUN(get_vars(data, cols)) 
  o	* x <- DT[ ]       replace it with  something from collapse:: ?
  o	get_vars(<-) is around 2x faster than `[.data.frame` and 8x faster than `[<-.data.frame` 
  o	From collapse documentation:    https://sebkrantz.github.io/collapse/reference/select_replace_vars.html   
  o	data[sapply(data, is.numeric)] or data[sapply(data, is.numeric)] <- value ... replace with num_vars(data) and num_vars(data) <- value 
  o	dplyr::select() is slow and collapse::fselect is about 100x faster 
  o	weighted.mean() etc., replace with collapse::fmean(x, w= ) and see fmin, fmax, funique, etc. - mostly done. 
  o	   See https://sebkrantz.github.io/Rblog/2021/08/13/fastverse/
  
•	Consider precalculating raw sites2blocks tables at some large distance for many sites, and have getblocksnearby() be able to use that info when appropriate, filtering by the new specified distance, instead of doing the full recalculation of sites2blocks for a new specified distance. 

•	Parallel processing. But consider if it requires each process has a full copy of the large block datasets.

•	Asynchronous coding - async coding is pretty hard to do well but probably essential. Simplest is to use callr pkg, e.g. to knit rmarkdown in background, to create localtree and do data(blockpoints), or maybe run getblocksnearby() and doaggregate() in background, etc. There are multiple ways to add async programming to your Shiny app (see https://drive.google.com/file/d/1qngIJToQxE_LExuqgjXXtoV25z2Ri1uQ/view  ), including (callR can speed it up for a single user, but Promises will just help prevent other users from being slowed down by one user’s computations) 1)• futures and promises, • Unified Parallel and Distributed Processing in R and a promise library for R, 2) • callr to  Call R from R, 3) • coro - Coroutines for R. And see Say goodbye to unnecessary waiting: mastering asynchronous programming in Shiny: Veerle van Leemput, at https://drive.google.com/file/d/1qngIJToQxE_LExuqgjXXtoV25z2Ri1uQ/view for slides or Appsilon youtube channel.



"
