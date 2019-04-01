## use this script to run the data explorer locally

# Change this line for where these files reside on your computer
setwd("F:/_Howard/git/data_explorer")

# run the scripts
source("global.R")
source("server.R")
source("ui.R")

# call the Shiny App
shinyApp(ui=ui,server=server)
