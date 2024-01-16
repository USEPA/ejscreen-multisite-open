EJAM Environmental Justice Analysis Multisite tool
================

<!-- README.md is generated from README.Rmd. Please edit Rmd not md  -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

# Technical Details

- Technical documentation for R software users

  - R users vignette that walks through installation and key functions:
    [Vignette](EJAM-vignette.html "EJAM-vignette.html") (must be on VPN)

  - R developers technical documentation of all R functions and data in
    web page format: [EJAM R Functions and Data](EJAM.html "EJAM.html")
    (must be on VPN)

  - GitHub repository of code and data:
    [USEPA/EJAM](https://github.com/USEPA/EJAM#readme "https://github.com/USEPA/EJAM#readme")
    (must be on VPN)

- EJAM Web App Demo internal to EPA: As of 10/2023 the main app is not
  on a production server yet. A very minimal portion of the calculation
  tool is here on a staging server:

  - For a demo of EJScreen-like results (some \#s are slightly
    different), for 100 points in under 5 seconds, and 1,000 points in
    about 10-30 seconds depending on distance **[ejamlite
    demo](https://rstudio-connect.dmap-stage.aws.epa.gov/content/725e3761-3dc1-4012-b07c-23126063da97/ "https://rstudio-connect.dmap-stage.aws.epa.gov/content/725e3761-3dc1-4012-b07c-23126063da97/")**
    (must be on VPN)

  - For a demo of EJScreen results via API, for 100 points in about 60
    seconds, and 1,000 points in about 10 minutes [ejscreenapi
    demo](https://rstudio-connect.dmap-stage.aws.epa.gov/content/725e3761-3dc1-4012-b07c-23126063da97/ "https://rstudio-connect.dmap-stage.aws.epa.gov/content/725e3761-3dc1-4012-b07c-23126063da97/")
    (must be on VPN)

# Non-technical Overview

# Installation

To install the latest version of EJAM, you will first need access to the
GitHub repositories through EPA. Then use the following steps to install
from private repositories:

1.  create GitHub personal access token with ‘repo scope’

- Go to <https://github.com/settings/tokens> and select Tokens (classic)
  on the left-hand side. Then click ‘Generate New Token’ -\> Generate
  new token (classic).
- Give it a name and select all boxes under repo scope. Scroll down and
  click ‘Generate Token’.

2.  set GitHub credentials in Rstudio

- one-time login: from the console, run credentials::set_github_pat().
  Paste in your PAT to the login popup under ‘Token’.
- store credentials long-term: run usethis::edit_r\_environ() to open
  your .Renviron file and and add a line with your PAT in this format:
  GITHUB_PAT = ‘abc’
  - You can specify an extra argument scope = ‘project’ if you only want
    the PAT to work for a particular Rstudio project.

3.  Install the packages using `devtools::install_github()`

``` r
devtools::install_github('USEPA/EJAMejscreenapi')
devtools::install_github('USEPA/EJAMbatch.summarizer')
devtools::install_github('USEPA/EJAM')
```

### EPA’s Environmental Justice Analysis Multi-site tool

EJAM is a user-friendly web app, provided by the US EPA, that can
summarize demographics and environmental conditions for any list of
places in the nation. It provides interactive results and a formatted,
ready-to-share report with written explanations of the results, tables,
and graphics. The report can provide EJ-related information about people
who live in communities near any of the industrial facilities on a list,
for example.

### What is EJAM?

EJAM is a user-friendly web app, provided by the US EPA, that can
summarize demographics and environmental conditions for any list of
places in the nation. It provides interactive results and a formatted,
ready-to-share report with written explanations of the results, tables,
and graphics. The report can provide EJ-related information about people
who live in communities near any of the industrial facilities on a list,
for example.

To use the tool, one first selects the places to be analyzed. This could
be, for example, everyone within 2 miles of any EPA-regulated facility
in a specific NAICS code (industrial sector). The tool runs a fast
“buffer” or proximity analysis at each location, similar to how EJScreen
provides a standard report for a single location, except EJAM does this
for each of a large number of locations very quickly. Then, most
importantly, EJAM provides a complete overview report, to summarize
environmental conditions and demographics across all the populations and
all of the locations. The results can be explored interactively or
downloaded as a written report with text, tables, and graphics.

An EJAM report can quickly and easily show which demographic groups live
near the selected facilities. It also provides new insights into which
environmental stressors may affect certain demographic subgroups
disproportionately, to varying degrees, near a regulated sector overall
and at individual sites. This allows EJ analysis to move beyond looking
at a small number of indicators for a few demographic groups, at one
site in a single permitting decision, to a more complete picture of
conditions near a whole set of facilities that is the focus of a risk
analysis or a new regulation being considered, for example.

### EJAM features and benefits

EJAM provides a ready-to-use summary report, plus more flexibility,
accuracy, and speed than other tools have. The web-based app quickly
provides results on the fly – The circular buffering module was
optimized to be extremely fast (allowing realtime exploratory work in an
app), while still using the block-population calculation EJScreen uses,
making it more consistent with EJScreen and more accurate than other
approaches.

EJAM also lets one pick locations through several different approaches,
such as specifying points by industry or latitude/longitude, or using
shapefiles with polygons rather than just analyzing residents within a
fixed distance.

The default demographic indicators are [EJScreen’s basic demographic
indicators](https://www.epa.gov/ejscreen/overview-demographic-indicators-ejscreen "https://www.epa.gov/ejscreen/overview-demographic-indicators-ejscreen"),
with the addition of race/ethnicity subgroups.

The data and software are shared as reusable, well-documented functions
or modules in an R package, to allow software developers or analysts to
take advantage of these resources in running their own analyses or
building or supplementing their own tools, websites, or mobile apps.

### Updates

EJAM data updates are meant to match EJScreen’s scheduled updates and
main version numbers, so EJAM 2.2 uses the same basic data as EJScreen
2.2. The 2.2 version of EJScreen (released July 2023) had up-to-date
demographic data (e.g., Census 2020 block weights and ACS 2017-2021
block group demographics).

### Speed

The power of this tool enables faster and more accurate results than
other tools generally have been able to provide. It also should be much
more cost-effective as a public tool than the standard buffering
solution would be. This tool lets any user very quickly see an analysis
of a very large number of places (which EJScreen was not designed to
offer), and immediately get a ready-to-use report that provides
perspective on an entire industrial sector or set of places.

### Accuracy

Compared to other approaches, EJAM’s high-resolution buffering provides
more accurate information about which populations live inside a buffer,
which is important in rural areas where a single blockgroup can cover a
very large area. For circular buffers, EJAM 2.2 uses the locations of
internal points of Census 2020 blocks, not areal apportionment of block
groups, to estimate where residents live within each block group. This
essentially assumes people are evenly spread out within each block, not
each block group, and treats the block population as if they were all
located at the block’s internal point. There are several million blocks
in the US. The only more accurate approaches are to use areal
apportionment of blocks (not block groups), but that is very slow, or to
use a 30x30 meter grid based on dasymetric estimates of where people
live at even higher resolution than a block, but that requires large
amounts of storage and computer time. Any analysis ideally should
closely replicate EJScreen’s results for a single location, such as
total population count within 1 mile, to avoid public confusion and
inconsistency.

EJAM also takes note of which residences are near which sites, to avoid
double-counting people in the summary statistics but still allow a user
to view results for one site at a time. This is something other tools
and analyses often cannot provide - when they aggregate across sites
they typically do not retain the statistics on individual sites, and
rarely if ever keep track of which communities are near multiple
facilities. Keeping track of this would also allow an analyst to explore
how many people are near multiple sites, or ask which sites in
communities that already have multiple sites nearby.

EJAM was designed so that it can provide a continuous distribution of
distances, as distributed across blocks or people for one or all of the
nearby facilities. This enables exploration of the complete picture of
proximities, rather than using an arbitrary single distance defining
near versus far. The distribution can be sliced later for the summary
statistics at any distance, and can be summarized as a distribution of
distances within each demographic group.

### Future Plans

EJAM will also be able to analyze other demographic indicators, to
include user-selected EPA-hosted layers, or even user-provided data. In
other words, an analysis would be able to include other layers on risks
or concentrations (at block group resolution), or user-provided scores
for each block group.

The tool uses a default, standard report, but will allow flexibility
beyond that. EJAM will provide a standard report (text, graphics, and
maps) to print, download, share, and use, but that a user could further
edit offline (e.g., in Word). Users also can download individual
graphics and data files (for individual sites and summary statistics).

The results can be viewed interactively, where one can adjust certain
aspects of the analysis and outputs (and that could possibly flow into
the summary report as well) such as preferred graphics/ tables,
indicator thresholds, reference groups, reference areas, metrics, etc.),
to make the outputs fit a given user’s needs.

To provide further flexibility and help avoid duplication of effort, an
EJAM API would provide access to services such as fast buffering,
summarization, or data.

EJAM is designed with growth in mind, assuming that the specific
indicators of interest will expand or change over time. It is even
possible that multiple resolutions of data will need to be analyzed,
such as block, block group, and tract data for different indicators. A
subsequent refinement might even use a high-resolution raster grid of
population estimates rather than the Census Block counts currently used
for buffering and weighting block group scores for partially included
block groups.

## Disclaimer for App

This software/application has been approved for release by the U.S.
Environmental Protection Agency (USEPA). Although the software has been
subjected to rigorous review, the USEPA reserves the right to update the
software as needed pursuant to further analysis and review. No warranty,
expressed or implied, is made by the USEPA or the U.S. Government as to
the functionality of the software and related material nor shall the
fact of release constitute any such warranty. Furthermore, the software
is released on condition that neither the USEPA nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.

## Disclaimer for GitHub Repository Content

The United States Environmental Protection Agency (EPA) GitHub project
code is provided on an “as is” basis and the user assumes responsibility
for its use. EPA has relinquished control of the information and no
longer has responsibility to protect the integrity, confidentiality, or
availability of the information. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by EPA. The EPA seal and logo
shall not be used in any manner to imply endorsement of any commercial
product or activity by EPA or the United States Government.
