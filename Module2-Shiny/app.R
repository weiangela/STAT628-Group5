library(tidyverse)
library(ggplot2)
library(corrplot)
library(factoextra)
library(rpart)
library(rpart.plot)
library(haven)
library(shiny)
library(bslib)

bodyFat <-
  read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/BodyFat.csv")

ui <- fluidPage(# Application title
  titlePanel("Bodyfat Estimation"),
  
  # Sidebar with a slider input for age
  sidebarLayout(
    sidebarPanel(
      h2("Where did you land amongst study participants?"),
      p("If you are an adult male, please enter in your body dimensions in!"),
      strong(
        "Abdomen circumference in cm",
        tooltip(
          bsicons::bs_icon("question-circle"),
          "Wrap a measuring tape tightly around your abdomen around where your belly button is.",
          placement = "right"
        )
      ),
      sliderInput(
        "ab",
        "",
        min = 50,
        max = floor(max(bodyFat$ABDOMEN)),
        value = 90
      ),
      strong(
        "Wrist circumference in cm",
        tooltip(
          bsicons::bs_icon("question-circle"),
          "Wrap a measuring tape tightly around your wrist.",
          placement = "right"
        )
      ),
      sliderInput(
        "p2",
        "",
        min = 15,
        max = 22,
        value = 19,
        step = 0.5
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p(
        "Within the study performed in 1970, 252 adult males had their body dimensions measured to correlate values with body fat percentage."
      ),
      plotOutput("abPlot"),
      plotOutput("p2Plot")
    )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$abPlot <- renderPlot({
    # generate bins based on input$ab from ui.R
    ab_grp <-
      cut(input$ab, seq(min(bodyFat$ABDOMEN), max(bodyFat$ABDOMEN) + 5, by = 5), labels = FALSE)
    
    data_df <- bodyFat %>%
      select(ABDOMEN) %>%
      mutate(
        grp = cut(
          ABDOMEN,
          seq(min(ABDOMEN), max(ABDOMEN) + 5, by = 5),
          labels = FALSE,
          include.lowest = TRUE,
          right = TRUE
        ),
        ab_select = (grp == ab_grp)
      )
    
    # draw the histogram with the specified number of bins
    ggplot(data_df, aes(x = ABDOMEN, fill = ab_select)) +
      geom_histogram(binwidth = 5, boundary = 0, title = "Abdomen") +
      theme(legend.position = "none")
    
  })
  
  output$p2Plot <- renderPlot({
    # generate bins based on input$p2 from ui.R
    p2_grp <- cut(input$p2, seq(min(bodyFat$WRIST), max(bodyFat$WRIST) + 1, by = 0.5), labels = FALSE)
    
    data_df <- bodyFat %>%
      select(WRIST) %>%
      mutate(
        grp = cut(
          WRIST,
          seq(min(WRIST), max(WRIST) + 1, by = 0.5),
          labels = FALSE,
          include.lowest = TRUE,
          right = TRUE
        ),
        p2_select = (grp == p2_grp)
      )
    
    # draw the histogram with the specified number of bins
    ggplot(data_df, aes(x = WRIST, fill = p2_select)) +
      geom_histogram(binwidth = 0.5, boundary = 0, title = "Wrist Circumference") +
      theme(legend.position = "none")
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
