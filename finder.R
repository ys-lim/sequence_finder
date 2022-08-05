# Inputs:
# 1. Intron sequence that CE resides in
# 2. Start and end coordinates of given intron
# 3. Start and end coordinates of cryptic exon

# Output:
# 1. Sequence of cryptic exon
# 2. Length of cryptic exon

get_sequence_forward <- function(intron_seq, intron_start, intron_end, ce_start, ce_end) {
  #intron_length <- nchar(intron_seq)
  intron_seq <- paste(intron_seq, collapse = "\n")
  ce_start_scaled <- as.numeric(ce_start) - as.numeric(intron_start) + 1
  ce_end_scaled <- as.numeric(ce_end) - as.numeric(intron_start) + 1
  substr(intron_seq, ce_start_scaled, ce_end_scaled)
}

get_ce_length <- function(ce_start, ce_end) {
  as.numeric(ce_end) - as.numeric(ce_start) + 1
}

get_sequence_reverse <- function(intron_seq, intron_start, intron_end, ce_start, ce_end) {
  #intron_length <- nchar(intron_seq)
  intron_seq <- paste(intron_seq, collapse = "\n")
  ce_start_scaled <- as.numeric(ce_start) - as.numeric(intron_end) + 1
  ce_end_scaled <- as.numeric(ce_end) - as.numeric(intron_end) + 1
  reversed_intron_seq <- intToUtf8(rev(utf8ToInt(intron_seq)))
  intToUtf8(rev(utf8ToInt(substr(reversed_intron_seq, ce_start_scaled, ce_end_scaled))))
}


# =========================================================================================

finder_ui <- function(id) {
  ns <- NS(id)
  
  div(
    h2("Choose strand:"),
    fluidRow(
      column(2,
             checkboxInput(ns("strand_for"), "Forward", value = TRUE, width = NULL)),
      column(2,
             checkboxInput(ns("strand_rev"), "Reverse", value = FALSE, width = NULL))
    ),
    
    h2("Input intron sequence:"),
    fluidRow(
      column(12,
             textAreaInput(ns("intron_seq"), "Intron sequence", "ACTGACTGACTGACTG"))
    ),
    fluidRow(
      column(2,
             textInput(ns("intron_start"), "Intron start", "10001")),
      column(2,
             textInput(ns("intron_end"), "Intron end", "10016")),
      column(2,
             textInput(ns("ce_start"), "Cryptic exon start", "10005")),
      column(2,
             textInput(ns("ce_end"), "Cryptic exon end", "10009"))
    ),
    actionButton(ns("submit"),"Submit"),
    
    h2("Output cryptic exon sequence:"),
    textOutput(ns("ce_sequence")),
    textOutput(ns("ce_length"))
    
  )
}

finder_server <- function(input, output, session){
  intron_seq <- eventReactive(input$submit,{
    input$intron_seq
  })
  
  intron_start <- eventReactive(input$submit,{
    input$intron_start
  })
  
  intron_end <- eventReactive(input$submit,{
    input$intron_end
  })
  
  ce_start <- eventReactive(input$submit,{
    input$ce_start
  })
  
  ce_end <- eventReactive(input$submit,{
    input$ce_end
  })
  
  strand_for <- eventReactive(input$submit,{
    input$strand_for
  })
  
  strand_rev <- eventReactive(input$submit,{
    input$strand_rev
  })
  
  output$ce_sequence <- renderText({
    if (strand_for() && !strand_rev()) {
      get_sequence_forward(intron_seq(),
                           intron_start(),
                           intron_end(),
                           ce_start(),
                           ce_end())
    } else if(!strand_for() && strand_rev()) {
      get_sequence_reverse(intron_seq(),
                           intron_start(),
                           intron_end(),
                           ce_start(),
                           ce_end())
    }
    
    
    })
  
  output$ce_length <- renderText({
    paste("Length of cryptic exon: ", get_ce_length(ce_start(),ce_end()), "bp")

  })
}

