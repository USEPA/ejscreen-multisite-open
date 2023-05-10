
# browseURL("https://walker-data.com/tidycensus/articles/basic-usage.html")
# browseURL("https://walker-data.com/umich-workshop-2023/acs-2021/#43")

library(ggplot2) # makes ggplot2 graphic interactive
library(ggiraph) # can customize interactivity- super powerfule pkg for interactive maps, plots, etc.

library(tidycensus)
library(tidyverse)
library(ggplot2)

# see the slides and tutorials. some of the notes below dont work but get the general idea

# NAMES OF TABLES AND THEIR VARIABLES 
vars <- load_variables(2021, "acs5")
View(vars)

# ENTIRE TABLE NOT JUST 1 VARIABLE
raceinfo = income_table <- get_acs(
  geography = "county", 
  table = "B30002",
  year = 2021
)

# WIDE DATA
age_sex_table_wide <- get_acs(
  geography = "state", 
  table = "B01001", 
  year = 2021,
  survey = "acs1",
  output = "wide"
)

# BLOCK GROUPS, AND NAMED VARIABLES
mytable <- get_acs(
  geography = "block group",
  state = "DE",
  variables = c(pct_a = "DP02_0062P",
                percent_bach = "DP02_0065P",
                percent_grad = "DP02_0066P"),
  year = 2021
)

highest_incomes <- median_income %>%
  separate(NAME, into = c("county", "state"), sep = ", ") %>%
  group_by(state) %>%
  filter(estimate == max(estimate))

# dplyr::arrange() sorts data based on values in one or more columns, 
# filter() to query data based on column values
arrange(median_income, desc(estimate))
income_states_dc <- filter(median_income, !str_detect(NAME, "Puerto Rico"))

# group_by() and 
# summarize() in dplyr to implement the split-apply-combine method of data analysis
highest_incomes <- median_income %>%
  separate(NAME, into = c("county", "state"), sep = ", ") %>%
  group_by(state) %>%
  filter(estimate == max(estimate))

# example
md_rent <- get_acs(
  geography = "county",
  variables = "B25031_001",
  state = "MD",
  year = 2021
)
# reorder()
md_plot <- ggplot(md_rent, aes(x = estimate, 
                               y = reorder(NAME, estimate))) +
  geom_point(color = "darkred", size = 2)
md_plot <- md_plot + 
  scale_x_continuous(labels = label_dollar()) +
  scale_y_discrete(labels = function(x) str_remove(x, " County, Maryland|, Maryland"))
