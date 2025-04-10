# Define UI
ui <- fluidPage(
 titlePanel("Breeding Value Estimation"),
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
   numericInput("n", "Number of Phenotypic Records:", value = 10),
   numericInput("y", "Average of Phenotypic Values for Individuals with Observations:", value = 30),
   numericInput("mean", "Mean Phenotypic Value in population:", value = 40)
  ),
  mainPanel(
   h3("Introduction"),
   withMathJax(helpText("This app helps illustrate the
concept of breeding value estimation and how it depends on various factors.
The app allows the user to adjust the additive genetic relationship between the
breeding candidate and individuals with phenotypic records $a'$ , the heritability
of the trait $h^2$, the number of phenotypic records $n$, the additive genetic
relationship between individuals with observations $a''$, the average of phenotypic values
for the individuals with records $\\bar{y}$, and the population mean $\\mu$.
The app uses these inputs to calculate the regression coefficient $b_{a|y}$ and breeding
value estimate $\\hat{a}$ based on the provided formulas. By adjusting the inputs, the user can explore how changes
in these factors affect the breeding value estimate.
")),
   h4("Breeding Value Estimation Formula's:"),
   withMathJax(
    helpText("$$\\hat{a} = b_{a|y} (\\bar{y} - \\mu)$$"),
    helpText("$$r_{\\hat{a},a} = a'b_{a|y} $$"),
    helpText("$$b_{a|y} = \\frac{a'nh^2}{1+(n-1)a''h^2}$$")),
   h4("Regression Coefficient:"),
   verbatimTextOutput("regression_coefficient"),
   h4("Breeding Value Estimate:"),
   verbatimTextOutput("breeding_value_estimate"),
   h4("Accuracy of Breeding Value Estimate:"),
   verbatimTextOutput("accuracy_breeding_value_estimate")
  )
 )
)

# Define server
server <- function(input, output) {

 # Calculate regression coefficient
 regression_coefficient <- reactive({
  numerator <- input$a1 * input$n * input$h2
  denominator <- 1 + (input$n - 1) * input$a2 * input$h2
  numerator / denominator
 })

 # Generate regression coefficient output
 output$regression_coefficient <- renderPrint({
  round(regression_coefficient(),3)
 })

 # Calculate breeding value estimate
 breeding_value_estimate <- reactive({
  regression_coefficient() * (input$y - input$mean)
 })

 # Calculate accuracy of breeding value estimate
 accuracy_breeding_value_estimate <- reactive({
  regression_coefficient() * input$a1
 })

  # Generate breeding value estimate output
 output$breeding_value_estimate <- renderPrint({
  round(breeding_value_estimate(),3)
 })

 # Generate accuracy of breeding value estimate output
 output$accuracy_breeding_value_estimate <- renderPrint({
  round(accuracy_breeding_value_estimate(),3)
 })

}

# Run the app
shinyApp(ui, server)
