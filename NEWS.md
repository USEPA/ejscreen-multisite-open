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
-   New internal function ejscreen_vscript() for interactively running and comparing results using parameter `save_ejscreen_output = TRUE` in `ejscreen_vs_ejam()`
-   New internal function `ejscreen_vs_ejam_bysite()` to show comparison site by site, for 1 indicator

### Bug fixes

-   Several functions of the suite xyz_from_naics() and xyz_from_mact() etc. fixed -- had been returning only first matching site.
-   Many new unit tests for fips-related, map-related, and other functions.
-   Fixed bug that caused errors in `ejamit_compare_distances()` and `ejamit_compare_distances_fulloutput()` <https://github.com/USEPA/EJAM/commit/e2f11014e88c0b339f2ae5f60a73d25f769bbccf>
-   Fixed bugs that caused `ejscreen_vs_ejam()` to not report pctownedunits and to have problems with any of x100varnames not being present in ejscreen output.
-   Fixed `fips2countyname()` to return NA for inputs that are not countyfips

---------------------------------------------------------------------------

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

One can also include one or more reference groups within the single list of sites analyzed, to simplify comparison of one or more groups of sites to one or more reference groups of places. This allows one to compare residents within 3 miles of certain facilities, for example, to a random sample of comparably-sized locations (once the user has first generated that list of random locations to use as a reference group appended to the list of sites for analysis). \* New functionality is still being further developed so formats may change.

## Other Changes

#### Save Customized Settings in the Web App

-   Many app settings and defaults now can be adjusted in the "Advanced"" page one can enable from the "About" page:

    -   radius shown initially in app by default
    -   default title of analysis
    -   maximum radius allowed
    -   maximum file upload size
    -   which indicators are compared to which thresholds via Advanced tab of shiny app, to count how many of a certain group of indicators exceeds the 90th percentile, or exceeds 2x the State average, etc.
    -   many other settings

-   Bookmarking in the advanced tab will enable a user to set up the app so it always starts off a certain way, with their preferred default radius, title of analysis, indicators, thresholds, etc. This is work in progress.

-   The advanced tab also provides a link to what had been called an "interim batch tool" -- EJAM's ejscreenapi shiny app -- if necessary to obtain a batch of data results directly from EJScreen (exact same numbers as reported by the EJScreen API), via link in advanced tab. This might be dropped or changed in the future.

-   The "About"" tab has been improved.

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

```{=html}
<!-- -->
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
