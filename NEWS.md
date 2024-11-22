# EJAM 2.32.0-EJScreen2.32 (November 2024)

-   Added FIPS code functionality to handle over 40,000 Census Places such as cities and towns. Not yet in web app.
-   Warnings if no internet connection, via new offline() internal function
-   latlon_from_address() does better checks if it can do geocoding
-   fips_bgs_in_fips() fixed, now returns NA for a 6-7 digit FIPS (city/CDP) since there are no simple child blockgroups that exactly and completely aggregate to cover a given CDP necessarily, and certainly not by just using the 7 digits of the city FIPS as if they were the first 7 of any BG FIPS.
-   fips_place2placename() fixed, now returns NA not "NA, NA" for invalid 6-7 digit fips, and related functions will likewise.
-   fips_valid() is exported
-   Faster unit testing

# EJAM 2.32.0-EJScreen2.32 (August-October 2024)

-   Deployed updates 10/11/2024
-   Incorporated new datasets to match EJScreen version 2.32 ozone and other data changes/corrections EJScreen version 2.32 released in August 2024.
-   Fixed known issues in web app, R functions, and examples or articles.
-   Improved and updated vignettes (articles) on documentation website (GitHub pages pkgdown site content) and README
-   Improved and updated documentation of R packages in help files.
-   Conducted validation analysis comparing EJAM and EJScreen results for proximity to 2,000 randomly selected FRS sites (3 mile radius).
    - Removed % owner occupied housing (pctownedunits) indicator pending clarifications/ corrections re: EJScreen calculation.
    - Confirmed the displayed demographic indicator values agree for well over 95% of locations, and the environmental indicator values also closely agree at the vast majority of sites. (Exceptions being the two noted above).
    - Updated percentile lookup logic to match the recent changes in EJScreen – some edge cases still in progress.
    - Improved community report scaling and formatting using decimals and sigfigs to better align with EJScreen.
    - Improved logic that identifies which State a site is in.
    - Addressed issues in % pre 1960 lead paint indicator
    - Improved comparison/validation functions
-   Improved file upload read-in validation.
-   Corrected several unit tests and added new ones.
-   Adopted shinytest2 to create shiny-based testing of the app, package, and GitHub merges.
-   Updated [map_headernames], test output files, names_... vectors, etc.

# EJAM 2.3.0-EJScreen2.3 (July 2024)

-   New data and indicators incorporated, to match EJScreen version 2.3 (Environmental indicators `cancer` and `resp` were removed from list of 13 key variables in `names_e`, and `no2` and `drinking` were added). Changes throughout code and datasets to use new data, including metadata in `map_headernames`, variable names stored as `names_e`, `names_e_pctile`, etc., formulas for aggregating across blockgroups for a report via `doaggregate()`, etc.
-   EJAM now is stand-alone in the sense that it now does not require the packages that had been called EJAMejscreenapi and EJAMbatch.summarizer.
-   All essential code from the old EJAMbatch.summarizer package are now in EJAM. `map_headernames` in particular is now in the EJAM package.
-   All essential code from the old EJAMbatch.summarizer package are now in EJAM.
-   `ejamit(quiet=TRUE)` new parameter prints less to RStudio console
-   Recreated all test datasets and files like `testoutput_ejamit_10pts_1miles` and removed `testpoints_conus5` dataset
-   Refreshed all documentation of R package help files and vignettes (articles) website
-   Versioning system now stores version info at top and bottom of `DESCRIPTION` file, used by get_metadata_mapping(), metadata_mapping() etc.
-   `names_d` and similar lists of names now are based on `map_headernames` which is the master list
-   Many new unit tests for various functions

# EJAM 2.2.4 internal beta web app and R package, preparing for internal deployment (June 2024)

### Changed

-   `latlon_from_naics()` children = TRUE is now an option and the default. Note children = FALSE has been default in `regid_from_naics()` and `naics_from_name()` etc. but that may change.
-   `fips_counties_from_state_abbrev()` and `fips_state_from_statename()` etc. are now not case-sensitive

### R package additions and improvements for R users

-   Improved [`ejam2barplot()`](https://usepa.github.io/EJAM/reference/ejam2barplot.html) shows easy plot of a few indicators overall.
-   New [`ejam2barplot_sites()`](https://usepa.github.io/EJAM/reference/ejam2barplot_sites.html) shows plot comparing top sites, for 1 indicator.
-   New [`name2fips()`](https://usepa.github.io/EJAM/reference/name2fips.html) converts State abbreviation, State name, or County name to FIPS code.
-   New `ejam2shapefile()` drafted as a quick way to save results of ejamit() for import to other GIS tools
-   New `ejam2barplot_sitegroups()` shows output of `ejamit_compare_types_of_places()`
-   [`ejamit_compare_types_of_places()`](https://usepa.github.io/EJAM/reference/ejamit_compare_types_of_places.html) and [`ejamit_compare_distances()`](https://usepa.github.io/EJAM/reference/ejamit_compare_distances.html) are flexible now in how sitepoints is defined: they let you specify sitepoints as an interactively selected file (if the sitepoints parameter is not provided) or a filepath or a data.frame object.
-   Additions on how to select radius or radii, including uncertainty considerations in low-density areas, in [Advanced article (vignette)](https://usepa.github.io/EJAM/articles/4_advanced.html).
-   New `is.island()` function
-   `fips_state_from_state_abbrev()` and `fips_from_table()` etc. now can handle Island Territories AS, GU, MP, UM, VI
-   `counties_as_sites()` and `states_as_sites()` revamped
-   `mapfastej_counties()` now accepts a data.frame not just data.table
-   `fips_valid()` handles NA values better and recognizes more problematic fips

### Bug fixes

-   Several functions of the suite xyz_from_naics() and xyz_from_mact() etc. fixed -- had been returning only first matching site.
-   Many new unit tests for fips-related, map-related, and other functions.
-   Fixed bug that caused errors in `ejamit_compare_distances()` and `ejamit_compare_distances_fulloutput()` <https://github.com/USEPA/EJAM/commit/e2f11014e88c0b339f2ae5f60a73d25f769bbccf>
-   Fixed `fips2countyname()` to return NA for inputs that are not countyfips

------------------------------------------------------------------------

# EJAM 2.2.3 internal beta web app and R package (May 2024)

## Significant New Features

### Documentation of all EJAM R functions and datasets via website

[The documentation website](https://usepa.github.io/EJAM/index.html) includes

-   Overview of the R package
-   Reference with documentation of every function and dataset
-   How-to articles (aka vignettes) on installing, getting starting with key functions, and doing analysis in RStudio.

### Accessibility and Usability Review

-   UI is updated for 508 compliance and other standards and guidelines, including contrast and colors
-   Webpage responsiveness and use of space are improved.

### Comparing Distances

Comparing results at various distances is possible in new RStudio functions. This is not available in the web app UI, but may later be built into the web app interface.

-   Examples and background drafted in the [Advanced article (vignette)](https://usepa.github.io/EJAM/articles/4_advanced.html) on how to compare distances, and how to select radius or radii, including uncertainty considerations in low-density areas.
-   New [`ejamit_compare_distances()`](https://usepa.github.io/EJAM/reference/ejamit_compare_distances.html) lets you compare results at several different distances easily. It lets R users quickly run `ejamit()` for each of several distances, and automatically combines the results in convenient tables.
-   A plot shows how the indicators vary with distance from the site(s).
-   `plot_distance_by_pctd()` is a related function and is now improved so it now can handle any indicator that can be aggregated as a weighted mean (not just demographics and not only population weights), is more flexible in input formats, does more error checking, and has documentation. \* New functionality is still being further developed so formats may change.

### Comparing Groups of Locations or Facilities

Comparing whole categories to each other and to reference groups of places is possible in new RStudio functions. This is not available in the web app UI, but may later be built into the web app interface. However, this type of analysis is best suited for work in RStudio because it would typically cover many thousands of sites, or even hundreds of thousands, which might take an hour per 200,000 sites, for example.

-   New [`ejamit_compare_types_of_places()`](https://usepa.github.io/EJAM/reference/ejamit_compare_types_of_places.html) drafted and documented, providing powerful capabilities for comparing subgroups of places within one long list of sites. This allows one to do many types of larger analyses such as the following examples:

    -   comparing places of type A versus type B, based on user-provided areas grouped in user-defined categories (types). For example these could be user-provided polygons defining various kinds of communities, watersheds, zones based on some criteria, etc., once a user has a list of locations they have labelled indicating which places are in which group/ category/ type of place.
    -   compare a specific set of EPA-regulated facilities to one or more relevant comparison groups of sites.

One now can categorize facilities based on any criteria for which one already has data in a table of sites. For example, groups of sites could be defined by a user if they have data on any of the following:

-   type of facility such as industrial sector defined by NAICS/SIC
-   history of compliance/enforcement issues
-   types of emissions controls
-   magnitude of emissions
-   modeled contribution to ambient levels or risks
-   applicable rules to which the sites are subject
-   geographic region
-   urban/rual location
-   parent company
-   etc.

One can also include one or more reference groups within the single list of sites analyzed, to simplify comparison of one or more groups of sites to one or more reference groups of places. This allows one to compare residents within 3 miles of certain facilities, for example, to a random sample of comparably-sized locations (once the user has first generated that list of random locations to use as a reference group appended to the list of sites for analysis). *New functionality is still being further developed so formats may change.*

## Other Changes

#### Save Customized Settings in the Web App

-   Many app settings and defaults now can be adjusted in the "Advanced" page one can enable from the "About" page:

    -   radius shown initially in app by default
    -   default title of analysis
    -   maximum radius allowed
    -   maximum file upload size
    -   which indicators are compared to which thresholds via Advanced tab of shiny app, to count how many of a certain group of indicators exceeds the 90th percentile, or exceeds 2x the State average, etc.
    -   many other settings

-   Bookmarking in the advanced tab will enable a user to set up the app so it always starts off a certain way, with their preferred default radius, title of analysis, indicators, thresholds, etc. This is work in progress.

-   The advanced tab also provides a link to what had been called an "interim batch tool" -- EJAM's ejscreenapi shiny app -- if necessary to obtain a batch of data results directly from EJScreen (exact same numbers as reported by the EJScreen API), via link in advanced tab. This might be dropped or changed in the future.

-   The "About" tab has been improved.

#### R package improvements for R users

-   `run_app()` is updated -- Several app settings and defaults now can be adjusted in RStudio using parameters provided to run_app(). This allows a few parameters to set radius in advance, or to show the advanced tab at startup by default, or do the following for example:
    -   `run_app(default_default_miles=3.1, default_max_miles=31, default_max_mb_upload=100)`
    -   see `?run_app` for more parameters.
-   New `ejam2barplot()` simplifies seeing barplot comparing indicators such as the demographic groups, as ratios to state or national averages
-   New `ejam2ratios()` lets you easily see key ratios of indicators at sites analyzed compared to US/State average, in RStudio.
-   New `latlon_from_anything()` provides more flexibility so user can provide path to a file, not just a table object
-   New `naics_is.valid()` is mostly for internal use
-   New `acs_by_bg()` has been drafted and `acs_bycounty()` is improved
-   `shapefile_from_any()` and other shapefile reading functions are improved
-   `sf::st_layers()` is now used by functions using shapefiles
-   New testdata files (examples of input files to try uploading, and examples of output or intermediate data results) are expanded and improved. See the ["Advanced" article section about what files and objects are installed with the package](https://usepa.github.io/EJAM/articles/4_advanced.html#test-data-available-for-ejam).

```         
         folders available at system.file("testdata", package = "EJAM")
    [1,] "EJAM/testdata/address"                                       
    [2,] "EJAM/testdata/examples_of_output"                            
    [3,] "EJAM/testdata/fips"                                          
    [4,] "EJAM/testdata/latlon"                                        
    [5,] "EJAM/testdata/program_type"                                  
    [6,] "EJAM/testdata/programid"                                     
    [7,] "EJAM/testdata/registryid"                                    
    [8,] "EJAM/testdata/shapes"
```

-   testoutput\_ datasets updated to reflect updated formats and indicators provided by ejamit() and ejscreenit(), and invalid latlon points removed from testpoints_10 and testpoints_1000
-   dataload_from\_ functions improved

#### Bug fixes

-   fixed bug in start analysis button in shiny app
-   fixed issue in shiny app when uploaded points were all invalid
-   fixed histogram label that said Percentile in some cases where it should not
-   `plotblocksnearby()` now no longer fails if given output of testpoints_n(n, weighting = "block")
-   `getblocksnearby()` now correctly shows diagnostics on distances and block counts after the distances are adjusted based on lower limit and capped by radius specified
-   `naics_from_code()` and `sic_from_code()` bug fixes
-   names of language-related indicators mostly fixed
-   other bug fixes

------------------------------------------------------------------------

# EJAM 2.2.2 internal beta web app and R package (3/12/2024)

-   Documentation drafted for pkgdown articles/ vignettes
-   More test data / examples added
-   Shapefile handling improved
-   Bug fixes
-   other updates/changes

# EJAM 2.2.1 internal beta web app and R package (1/31/2024)

-   Major changes
-   Deployed to Posit Connect staging server as a beta test web app

# EJAM 2.2.1 R package (10/18/2023)

-   Major changes

# EJAM 2.2.0 R package (8/03/2023)

-   Updated to match EJScreen version 2.2

# EJAM 2.1.1 R package (9/19/2022)

-   First version on USEPA github
