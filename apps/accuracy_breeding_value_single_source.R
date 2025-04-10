# Define UI
ui <- fluidPage(
 titlePanel("Accuracy Breeding Value Estimation"),
 withMathJax(),
 # section below allows in-line LaTeX via $ in mathjax. Replace less-than-sign with <
 # and grater-than-sign with >
 tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
 sidebarLayout(
  sidebarPanel(
   sliderInput("a1", "Additive Genetic Relationship between the
Breeding Candidate and Individuals with Phenotypic Records:", min = 0, max = 1, step = 0.01, value = 0.5),
   sliderInput("a2", "Additive Genetic
Relationship between Individuals with Observations:", min = 0, max = 1, step = 0.01, value = 0.5),
   sliderInput("h2", "Trait Heritability:", min = 0, max = 1, step = 0.01, value = 0.5),
   textInput("n", "Number of Phenotypic Records:", value = "10,20,30")
  ),
  mainPanel(
   h3("Introduction"),
   withMathJax(helpText("This app helps illustrate the
concept of accuracy of breeding value estimation and how it depends on various factors.
The app allows the user to adjust the additive genetic relationship between the
breeding candidate and individuals with phenotypic records $a'$ , the heritability
of the trait $h^2$, the number of phenotypic records $n$, and the additive genetic
relationship between individuals with observations $a''$.
The app uses these inputs to calculate the regression coefficient $b_{a|y}$ and accuracy of breeding
value estimates $\\hat{a}$ based on the provided formulas. By adjusting the inputs, the user can explore how changes
in these factors affect the accuracy.
")),
   h4("Accuracy Breeding Value Estimation Formula's:"),
   withMathJax(
    helpText("$$r_{\\hat{a},a} = a'\\frac{a'nh^2}{1+(n-1)a''h^2} $$")),
   h4("Plot of Accuracy of Breeding Value Estimate:"),
   plotOutput("plot")
  )
 )
)

# Define server
server <- function(input, output) {


 # Calculate regression coefficient
 regression_coefficient <- reactive({
  # Convert input$n to a numeric vector
  n <- as.numeric(strsplit(input$n, split = ",")[[1]])
  #numerator <- input$a1 * input$n * input$h2
  #denominator <- 1 + (input$n - 1) * input$a2 * input$h2
  numerator <- input$a1 * n * input$h2
  denominator <- 1 + (n - 1) * input$a2 * input$h2
  numerator / denominator
 })

 # Calculate accuracy of breeding value estimate
 accuracy_breeding_value_estimate <- reactive({
  regression_coefficient() * input$a1
 })

 # Generate accuracy of breeding value estimate output
 output$accuracy_breeding_value_estimate <- renderPrint({
  round(accuracy_breeding_value_estimate(),3)
 })
 
 # Generate accuracy of breeding value estimate output
 output$plot <- renderPlot({
  n <- as.numeric(strsplit(input$n, split = ",")[[1]])
  plot(y=accuracy_breeding_value_estimate(),n, type="l", lwd=2, col=2, lty=2,
       xlab="Number of Phenotypic Records", ylab="Accuracy", frame.plot=FALSE)
 })
 
}

# Run the app
shinyApp(ui, server)
