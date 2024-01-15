



# https://pins.rstudio.com/articles/using-board-url.html


?pins::write_board_manifest()
# https://pins.rstudio.com/




library(pins)

# github_raw <- function(x) paste0("https://raw.githubusercontent.com/", x)
# github_raw <- function(x) paste0("https://github.com/ejanalysis/ejscreen/blob/master/inst/extdata/", x)

github_raw <- function(x) paste0("https://github.com/ejanalysis/ejscreen/raw/master/inst/extdata/", x)

# "https://github.com/ejanalysis/ejscreen/raw/master/inst/extdata/mact_table.arrow"


## Specify the URL to define the board, with a named vector of URLs to specific pins:
b1 <- board_url(c(
  mact_table = github_raw("mact_table.arrow")
  # etc.
))

# confirm file is there
b1 %>% pins::pin_search("mact_table")

# This will download save locally and then read from local... but isn't there a way to directly read into memory?
#
# download and save locally in a temp folder and get the path, then read it from temp folder
mypath <- b1 %>% pin_download("mact_table")
x <- as.data.frame(arrow::read_ipc_file(mypath)) # otherwise it is retrieved as a tibble


# download and interactively prompt user for where to save the file locally
b1 %>% pin_browse("mact_table", local = TRUE)


# download and save locally in a temp folder and open that folder in Windows File Explorer to see it
b1 %>% pin_browse("mact_table", local = TRUE)


# download and read into memory? must have declared file type
b1 %>% pin_read("mact_table")

