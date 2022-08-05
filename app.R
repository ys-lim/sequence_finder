
source("~/CE_sequence_finder/finder.R")

ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel(h1("Cryptic exon sequence finder")),
  mainPanel(width = 12,
            finder_ui("finder")
  )
)

server <- function(input,output,session) {
  callModule(finder_server, "finder")
}

shinyApp(ui, server)
