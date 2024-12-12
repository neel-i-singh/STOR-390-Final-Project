library(shiny)
library(shinythemes)

snps_data <- data.frame(
  SNP = c("rs1625579", "rs17662626", "rs3800316", "rs17693963", "rs2021722", 
          "rs707938", "rs9268856", "rs10503253", "rs7004633", "rs7914558", 
          "rs11191580", "rs12966547"),
  OR = c(1.12, 1.2, 1.13, 1.19, 1.15, 1.12, 1.12, 1.11, 1.1, 1.1, 1.15, 1.09)
)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Schizophrenia Risk Assessment Tool"),
  p(HTML("This is a clinical decision support tool designed to aid in a psychiatrist or psychologist's assessment of a patient's schizophrenia risk.<br> <br>
  The use of this tool requires the user to acknowledge the following warnings:<br>
  1. The proprietary nature of this schizophrenia risk predictor tool prevents disclosure of specific information relating to how genetic factors are weighed or how risk scores are determined.<br>
  2. This schizophrenia risk predictor tool is based on group data and is designed to identify patterns associated with groups at higher genetic risk—not to definitively determine the risk for any specific individual.<br>
  3. Some genetic studies have raised questions about whether predictive tools based on genetic data might inadvertently reflect biases present in the datasets used to develop them, potentially influencing classifications for certain demographic groups.<br>
  4. This tool compares genetic profiles to data from European populations. However, cross-validation specific to all ethnic populations has not yet been conducted. Predictive tools must be regularly evaluated and updated to ensure accuracy and fairness across diverse populations.<br>
  5. This schizophrenia risk predictor tool is designed for use by licensed psychiatrists and psychologists only."
  )),
  
  sidebarLayout(
    sidebarPanel(
      h4("Input Your Genotype"),
      p("Enter the number of risk alleles (0, 1, or 2) for each SNP:"),
      
      numericInput("rs1625579", "rs1625579:", 0, min = 0, max = 2, step = 1),
      numericInput("rs17662626", "rs17662626:", 0, min = 0, max = 2, step = 1),
      numericInput("rs3800316", "rs3800316:", 0, min = 0, max = 2, step = 1),
      numericInput("rs17693963", "rs17693963:", 0, min = 0, max = 2, step = 1),
      numericInput("rs2021722", "rs2021722:", 0, min = 0, max = 2, step = 1),
      numericInput("rs707938", "rs707938:", 0, min = 0, max = 2, step = 1),
      numericInput("rs9268856", "rs9268856:", 0, min = 0, max = 2, step = 1),
      numericInput("rs10503253", "rs10503253:", 0, min = 0, max = 2, step = 1),
      numericInput("rs7004633", "rs7004633:", 0, min = 0, max = 2, step = 1),
      numericInput("rs7914558", "rs7914558:", 0, min = 0, max = 2, step = 1),
      numericInput("rs11191580", "rs11191580:", 0, min = 0, max = 2, step = 1),
      numericInput("rs12966547", "rs12966547:", 0, min = 0, max = 2, step = 1),
      
      actionButton("calculate", "Calculate Odds")
    ),
    
    mainPanel(
      h4("Results:"),
      textOutput("odds_text")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$calculate, {
    genotypes <- c(
      input$rs1625579,
      input$rs17662626,
      input$rs3800316,
      input$rs17693963,
      input$rs2021722,
      input$rs707938,
      input$rs9268856,
      input$rs10503253,
      input$rs7004633,
      input$rs7914558,
      input$rs11191580,
      input$rs12966547
    )
    
    log_odds <- sum(log(snps_data$OR) * genotypes)
    odds <- exp(log_odds)
    
    output$odds_text <- renderText({
      paste("Compared to the general popluation, the patient's estimated schizophrenia odds are:", round(odds, 4))
    })
  })
}

shinyApp(ui = ui, server = server)