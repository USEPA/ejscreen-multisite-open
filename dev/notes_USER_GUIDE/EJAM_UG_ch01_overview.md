# Chapter 1 (Introduction): EJAM background, uses, and support resources.

## What is EJAM?

EJAM is the Environmental Justice Analysis Multisite (EJAM) tool provided by the United States Environmental Protection Agency (US EPA). EJAM is a user-friendly web app that can summarize demographics and environmental conditions for any list of places in the nation. It provides interactive results and a formatted, ready-to-share report with written explanations of the results, tables, and graphics. The report can provide EJ-related information about people who live in communities near any of the industrial facilities on a list, for example.

To use the tool, one first selects the places to be analyzed. This could be, for example, everyone within 2 miles of any EPA-regulated facility in a specific NAICS code (industrial sector). The tool runs a fast "buffer" or proximity analysis at each location, similar to how EJScreen provides a standard report for a single location, except EJAM does this for each of a large number of locations very quickly. Then, most importantly, EJAM provides a complete overview report, to summarize environmental conditions and demographics across all the populations and all of the locations. The results can be explored interactively or downloaded as a written report with text, tables, and graphics.

An EJAM report can quickly and easily show which demographic groups live near the selected facilities. It also provides new insights into which environmental stressors may affect certain demographic subgroups disproportionately, to varying degrees, near a regulated sector overall and at individual sites. This allows EJ analysis to move beyond looking at a small number of indicators for a few demographic groups, at one site in a single permitting decision, to a more complete picture of conditions near a whole set of facilities that is the focus of a risk analysis or a new regulation being considered, for example.

## Support Resources / For More Information

\-\-\-\-\-\-\-\-\-\-\-\--TBD

## Features and Benefits

EJAM provides a ready-to-use summary report, plus more flexibility, accuracy, and speed than was previously available. The web-based app quickly provides a written report plus interactive tables and graphics. Default indicators will include those in EJScreen plus a few others (e.g., demog. subgroups), but user-selected and user-provided indicators can also be analyzed. The circular buffering module was optimized to be extremely fast (allowing realtime exploratory work in an app), while still using the block-population calculation EJScreen uses, making it more consistent with EJScreen and more accurate than some other approaches.

Building on existing tools such as EJScreen and other environmental and demographic mapping tools, EJAM provides new levels of flexibility and power:

### Ease of use and standardization by default, but flexibility for power users

The tool uses default indicators but is flexible enough for work with other environmental and demographic indicators. The default environmental indicators are [EJScreen's 12 environmental indicators](https://www.epa.gov/ejscreen/overview-environmental-indicators-ejscreen), but an analysis can also include other user-selected EPA-hosted data layers on risks or concentrations (at block group resolution), or user-provided scores for each block group.

The default demographic indicators are be [EJScreen's basic demographic indicators](https://www.epa.gov/ejscreen/overview-demographic-indicators-ejscreen), with the addition of the 8 race/ethnicity subgroups in ACS5 Table B03002, and % poor as derived from Table C17002. EJAM can also analyze other demographic indicators, to include user-selected EPA-hosted layers, or user-provided data. See [Table B03002](https://data.census.gov/cedsci/table?hidePreview=true&tid=ACSDT5Y2020.B03002) and [Table C17002](https://data.census.gov/cedsci/table?hidePreview=true&tid=ACSDT5Y2020.C17002)

When using the tool, one can use various approaches to defining the areas to be analyzed, using shapefiles rather than just circular buffers.

The tool also uses a default, standard report, but allows flexibility beyond that. EJAM provides a standard report (text, graphics, and maps) to print, download, and use, but that a user could further edit offline (e.g., in Word). Users also can download individual graphics and data files (for individual sites and summary statistics).

The results also can be viewed interactively, where one can adjust certain aspects of the analysis and outputs (and that could possibly flow into the summary report as well) such as preferred graphics/ tables, indicator thresholds, reference groups, reference areas, metrics, etc.), to make the outputs fit a given user's needs.

### Speed and Accuracy

The power of this tool enables faster and more accurate results than other tools have been able to provide. It also should be very useful as a public tool because it offers the ability to analyze information within circular buffers that typically requires commercial software and expertise. This tool lets any user very quickly see an analysis of a very large number of places (which EJScreen cannot offer the public), and immediately get a ready-to-use report that provides perspective on an entire industrial sector or set of places.

EJAM uses up-to-date demographic data (e.g., Census 2020 block weights, and ACS 5-year block group demographics within several months of their release each year (such as ACS 2016-2020 released in December 2021 and used by EJScreen and EJAM in 2022), but likely stay on the same schedule as EJScreen's updates. Compared to other approaches, EJAM's high-resolution buffering (probably using internal points of Census blocks, not areal apportionment of block groups) provides more accurate information about which populations live inside a buffer, which is important in rural areas where a single blockgroup can cover a very large area. It also attempts to closely replicate EJScreen's results for a single location, to avoid public confusion and inconsistency.

EJAM also takes note of which residences are near which sites, to avoid double-counting people in the summary statistics but still allow a user to view results for one site at a time. This is something other tools and analyses often cannot provide - when they aggregate across sites they typically do not retain the statistics on individual sites, and rarely if ever keep track of which communities are near multiple facilities. Keeping track of this would also allow an analyst to explore how many people are near multiple sites, or ask which sites in communities that already have multiple sites nearby.

### Flexibility

EJAM also is designed so that it will be able to provide a continuous distribution of distances, as distributed across blocks or people for one or all of the nearby facilities. This would enable exploration of the complete picture of proximities, rather than using an arbitrary single distance defining near versus far. The distribution could be sliced later for the summary statistics at any distance, or could be summarized as a distribution of distances within each demographic group.

It is also designed with growth in mind, assuming that the specific indicators of interest will expand or change over time. It is even possible that multiple resolutions of data will need to be analyzed, such as block, block group, and tract data for different indicators. A subsequent refinement might even use a high-resolution raster grid of population estimates rather than the Census Block counts currently used for buffering and weighting block group scores for partially included block groups.

Open source software components will be shared as reusable well-documented modular tools, to allow developers or others to take advantage of these resources in running their own analyses or building or supplementing their own tools, websites, or mobile apps.

To provide further flexibility and help avoid duplication of effort, EJAM's API would provide access to services such as fast buffering, summarization, or data.
