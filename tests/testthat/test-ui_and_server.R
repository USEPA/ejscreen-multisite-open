# # attempting to be able to test a module...
# mypackage_server <- EJAM::mod_ejscreenapi_server
# server <- function(id) {
#   moduleServer(id, mypackage_server)
# }
# test_that("The module receives its input", {
#   shiny::testServer(server, {
#     session$setInputs(some_input = 100)
#     expect_equal(output$some_output, 50)
#   })
# })

source(system.file("global.R", package = "EJAM"))
source(system.file("global.R", package = "EJAMejscreenapi"))


test_that("app ui", {
  ui <- EJAM:::app_ui()
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(EJAM:::app_ui)
  for (i in c("request")) {
    expect_true(i %in% names(fmls))
  }
})

test_that("app server", {
  server <- EJAM:::app_server
  expect_type(server, "closure")
  # Check that formals have not been removed
  fmls <- formals(EJAM:::app_server)
  for (i in c("input", "output", "session")) {
    expect_true(i %in% names(fmls))
  }
})

test_that(
  "EJAM:::app_sys works",
  {
    expect_true(
      EJAM:::app_sys("golem-config.yml") != ""   #  source/EJAM/inst/golem-config.yml = installed/EJAM/golem-config.yml
    )
  }
)

test_that(
  "golem-config works",
  {
    config_file <- EJAM:::app_sys("golem-config.yml") #  source/EJAM/inst/golem-config.yml = installed/EJAM/golem-config.yml
    skip_if(config_file == "")

    expect_true(
      EJAM:::get_golem_config(
        "app_prod",
        config = "production",
        file = config_file
      )
    )
    expect_false(
      EJAM:::get_golem_config(
        "app_prod",
        config = "dev",
        file = config_file
      )
    )
  }
)

# Configure this test to fit your need.
# testServer() function makes it possible to test code in server functions and modules, without needing to run the full Shiny application
testServer(EJAM:::app_server, {

  # Set and test an input
  session$setInputs(bt_rad_buff = 1)
  expect_equal(input$bt_rad_buff, 1)
 
  
  # Example of tests you can do on the server:
  # - Checking reactiveValues
  # expect_equal(r$lg, 'EN')
  # - Checking output
  # expect_equal(output$txt, "Text")
})

# Configure this test to fit your need
test_that(
  "app launches",
  {
    
    golem::expect_running(sleep = 5)
  }
)
