
library(pins)
library(EJAM)

## create board linked to posit connect
board <- board_connect(versioned = TRUE)

## other options for creating boards: 
## board_local() - link to local file folder
## board_folder() - link to dropbox or network drive
## board_s3() - link to S3 bucket, such as EPA Data commons
## board_url() - build board from data URLs, allow read-only access to datasets

## read in datasets to be added
load('data/blockgroupstats.rda')
load('data/bgid2fips.rda')
load('data/blockid2fips.rda')
load('data/blockwts.rda')
load('data/quaddata.rda')

## add datasets to board in RDS format
pin_write(board = board, x = blockgroupstats, name = 'blockgroupstats_rds', type='rds')
pin_write(board = board, x = bgid2fips, name = 'bgid2fips_rds', type='rds')
pin_write(board = board, x = blockid2fips, name = 'bgid2fips_rds', type='rds')
pin_write(board = board, x = blockwts, name = 'blockwts_rds', type='rds')
pin_write(board = board, x = quaddata, name = 'quaddata_rds', type='rds')

## add datasets to board in arrow format
pin_write(board = board, x = blockgroupstats, name = 'blockgroupstats_arrow', type='arrow')
pin_write(board = board, x = bgid2fips, name = 'bgid2fips_arrow', type='arrow')
pin_write(board = board, x = blockid2fips, name = 'bgid2fips_arrow', type='arrow')
pin_write(board = board, x = blockwts, name = 'blockwts_arrow', type='arrow')
pin_write(board = board, x = quaddata, name = 'quaddata_arrow', type='arrow')

## print board or dataset metadata
board
pin_meta(board = board, name = 'blockgroupstats_rds')

## list all datasets in a board
pin_list(board = board)

## show versions of a dataset
pin_versions(board = board, name = 'blockgroupstats_rds')

## read datasets back in from the board
system.time(bgstats <- board %>% pin_read('blockgroupstats_rds'))
system.time(bgstats <- board %>% pin_read('blockgroupstats_arrow'))


## example of reading data from a pins board into a shiny app
## see more at https://pins.rstudio.com/articles/posit-connect.html
if (FALSE) {
  library(shiny)
  ui <- fluidPage(
    tableOutput("table")
  )
  
  server <- function(input, output, session) {
    #board <- board_local()
    data <- pin_reactive_read(board, "blockgroupstats_arrow", interval = 1000)
    output$table <- renderTable(data()[1:100,1:10])
  }
  shinyApp(ui, server)
}
