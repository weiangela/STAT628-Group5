library(tidyverse)
library(ggplot2)
library(corrplot)
library(factoextra)
library(rpart)
library(rpart.plot)
library(haven)
library(shiny)
library(bslib)
library(ggpmisc)

bodyFat <-
  read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/BodyFat.csv")
bodyFat <- bodyFat %>% filter(BODYFAT > 3, HEIGHT > 60, ABDOMEN < 130)

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
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p(
        "Within the study performed in 1970, 252 adult males had their body dimensions measured to correlate values with body fat percentage."
      ),
      plotOutput("abPlot"),
      plotOutput("Scatterplot")
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
      theme(legend.position = "none") +
      theme_bw()
    
  })

  output$Scatterplot <- renderPlot({
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
    
    data_df2 <- bodyFat %>%
      select(BODYFAT, ABDOMEN) %>%
      mutate(
      ab_select = ABDOMEN %in% data_df[data_df$ab_select == TRUE,]$ABDOMEN
      )
      
    ggplot(data_df2, aes(x=ABDOMEN, y=BODYFAT)) +
      geom_smooth(method = "lm", level = 0.99, alpha = 0.5, col ="red", formula = y ~ x) +
      stat_poly_eq(formula = y ~ x, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +
      geom_point(aes(col=ab_select)) +
      scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "black")) +
      labs(x = "Abdomen Circumference (cm)",
           y = "Body Fat (%)",
           title = "Scatterplot of Body Fat and ABDOMEN") +
      theme_bw()
    
  })

}

# Run the application
shinyApp(ui = ui, server = server)
