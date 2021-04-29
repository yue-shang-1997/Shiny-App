Shiny App Project  


home <- read_csv("home.csv")
home%>%
  filter(STYLE == 1) -> style1
home%>%
  filter(STYLE == 2) -> style2
home%>%
  filter(STYLE == 3) -> style3

ui <- fluidPage(
  titlePanel("Housing Information"),
  textInput("name", "Nickname"),
  textOutput("greeting"),
  selectInput("var", "Selection in style 1", choices = names(style1)),
  plotOutput("plot1"),
  selectInput("var", "Selection in style 2", choices = names(style2)),
  plotOutput("plot2"),
  selectInput("var", "Selection in style3", choices = names(style3)),
  plotOutput("plot3"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "home",
                  label = "Choose a home style:",
                  choices = c("style1", "style2", "style3")),
      numericInput(inputId = "num",
                   label = "How many houses' detail do you want to see in your style:",
                   value = 10)
      
    ),
    
    mainPanel(
      textOutput("text"),
      verbatimTextOutput("summary"),
      textOutput("text2"),
      verbatimTextOutput("summary2"),
      textOutput("text3"),
      verbatimTextOutput("summary3"),
      textOutput("text4"),
      verbatimTextOutput("summary4"),
      textOutput("text5"),
      tableOutput("view")
      
    )
  )
)


server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$home,
           "style1" = style1,
           "style2" = style2,
           "style3" = style3)
  })
  output$greeting <- renderText({
    paste("Hello ", input$name, "!","Please choose your dream house.")
  })
  output$text <- renderText({
    "Price:"
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset$SALES_PRICE)
  })
  output$text2 <- renderText({
    "Area:"
  })
  output$summary2 <- renderPrint({
    dataset <- datasetInput()
    summary(dataset$FINISHED_AREA)
  })
  output$text3 <- renderText({
    "Year:"
  })
  output$summary3 <- renderPrint({
    dataset <- datasetInput()
    summary(dataset$YEAR_BUILT)
  })
  output$text4 <- renderText({
    "LOT_Size:"
  })
  output$summary4 <- renderPrint({
    dataset <- datasetInput()
    summary(dataset$LOT_SIZE)
  })
  output$text5 <- renderText({
    "Housing Information"
  })
  output$view <- renderTable({
    head(datasetInput(), n = input$num)
  })
  output$plot1 <- renderPlot({
    ggplot(style1, aes(x = .data[[input$var]])) +
      geom_histogram(fill="lightblue",color="black")+
      ggtitle("Visualization of Style 1")
  })
  output$plot2 <- renderPlot({
    ggplot(style2, aes(x = .data[[input$var]])) +
      geom_histogram(fill="lightblue",color="black")+
      ggtitle("Visualization of Style 2")
  })
  output$plot3 <- renderPlot({
    ggplot(style3, aes(x = .data[[input$var]])) +
      geom_histogram(fill="lightblue",color="black")+
      ggtitle("Visualization of Style 3")
  })
}

shinyApp(ui, server)