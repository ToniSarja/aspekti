library(shiny)
library(ggplot2)
library(lme4)
library(sjPlot)
ui <- fluidPage(
  fileInput("upload", NULL, accept = c(".csv", ".tsv")),
  selectInput("var1", "Muuttuja 1", choices = c("not selected")),
  selectInput("var2", "Muuttuja 2", choices = c("not selected")),
  selectInput("var3", "Muuttuja 3", choices = c("not selected")),
  selectInput("ranvar1", "Muuttuja 3", choices = c("not selected")),
  selectInput("ranvar2", "Muuttuja 3", choices = c("not selected")),
  numericInput("n", "Rows", value = 5, min = 1, step = 1),
  tableOutput("head"),
  verbatimTextOutput("seka"),
  plotOutput("kuva"),
)


server <- function(input, output, session) {
  
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
           

    )
  })
  
  
  
  f <- reactive({
    as.formula(paste(input$var1,"~",input$var2,"+",input$var3,"+(1|",input$ranvar1,")+(1|",input$ranvar2,")"))
    })
  
  f2 <- reactive({
    req(input$upload)
    as.formula(paste(input$var1,"~",input$var2,"+",input$var3))
  })
  
  
  observeEvent(data(),{
    choices <- c("not selected",names(data()))
    updateSelectInput(inputId = "var1", choices = choices)
    updateSelectInput(inputId = "var2", choices = choices)
    updateSelectInput(inputId = "var3", choices = choices)
    updateSelectInput(inputId = "ranvar1", choices = choices)
    updateSelectInput(inputId = "ranvar2", choices = choices)
    })
  

  malli <- reactive({
    log <- glmer(f(), family = binomial, data = data())
  })
  

  summa <- reactive({
    summary(malli())
  })
  

  esjee <- reactive({
    plot_model(malli())
  })
  

  output$seka <- renderPrint(summa())
  
  output$kuva <- renderPlot(esjee())

  output$head <- renderTable({
    head(data(), input$n)
  })
  
}
  
  
shinyApp(ui = ui, server = server)
