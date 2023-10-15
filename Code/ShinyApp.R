library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(shinyBS)
bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/BodyFat.csv")
# convert all units to metric
bodyFat$WEIGHT <- bodyFat$WEIGHT / 2.20462  # kg
bodyFat$HEIGHT <- bodyFat$HEIGHT * 2.54     # cm

bodyFat <- bodyFat %>% filter(BODYFAT > 3, HEIGHT > 90, WEIGHT < 160, 
                              ADIPOSITY < 45, NECK < 50, ABDOMEN < 140, 
                              HIP < 140, THIGH < 80, KNEE < 47, 
                              ANKLE < 32,BICEPS<42)


ui <- fluidPage( 
  theme = bs_theme(),
  titlePanel("Bodyfat Estimation"),
  
  # Use tabsetPanel to contain your tabs
  tabsetPanel(
    
    # First tab: Study Data
    tabPanel("Study Data",
             sidebarLayout(
               sidebarPanel(
                 h2("Where did you land amongst study participants?"),
                 p("If you are an adult male, please enter in your body dimensions in!"),
                 strong("Abdomen circumference in cm",
                        bsTooltip(
                          bsicons::bs_icon("question-circle"),
                          "Wrap a measuring tape tightly around your abdomen around where your belly button is.",
                          placement = "right"
                        )
                 ),
                 sliderInput("ab","", min = 50, max = floor(max(bodyFat$ABDOMEN)), value = 90),
                 strong(
                   "Wrist circumference in cm",
                   bsTooltip(
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
               mainPanel(
                 p("Within the study performed in 1970, 252 adult males had their body dimensions measured to correlate values with body fat percentage."),
                 fluidRow(
                   column(width = 6, plotOutput("abPlot")),
                   column(width = 6, plotOutput("p2Plot"))
                 ),
                 fluidRow(
                   column(width = 12, plotlyOutput("TDplot"))
                 )
               )
             )
    ),
    
    # Second tab: Bodyfat Calculator
    tabPanel("Bodyfat Calculator", 
             sidebarLayout(
               sidebarPanel(
                 h3("Input your measurements"),
                 numericInput("abInput", "Abdomen (cm)", value = 90),
                 numericInput("wristInput", "Wrist (cm)", value = 19),
                 actionButton("calculate", "Calculate")
               ),
               mainPanel(
                 h3("Estimated Bodyfat:"),
                 verbatimTextOutput("bodyfatOutput")
               )
             )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  data_colors <- reactive({
    data_df <- bodyFat %>%
      select(ABDOMEN, WRIST, BODYFAT) %>%
      mutate(
        # create a group variable for ABDOMEN based on the input slider
        ab_grp = cut(
          ABDOMEN,
          seq(min(ABDOMEN), max(ABDOMEN) + 5, by = 5),
          labels = FALSE,
          include.lowest = TRUE,
          right = TRUE
        ),
        ab_select = (ab_grp == cut(input$ab, seq(min(bodyFat$ABDOMEN), max(bodyFat$ABDOMEN) + 5, by = 5), labels = FALSE)),
        # create a group variable for WRIST based on the input slider
        wr_grp = cut(
          WRIST,
          seq(min(WRIST), max(WRIST) + 0.5, by = 0.5),
          labels = FALSE,
          include.lowest = TRUE,
          right = TRUE
        ),
        wr_select = (wr_grp == cut(input$p2, seq(min(bodyFat$WRIST), max(bodyFat$WRIST) + 0.5, by = 0.5), labels = FALSE))
      )
    colors <- ifelse(data_df$ab_select & data_df$wr_select, "purple", 
                     ifelse(data_df$ab_select, "green", 
                            ifelse(data_df$wr_select, "blue", 
                                   "black"))) 
    list(data_df = data_df, colors = colors)
  })
  
  output$abPlot <- renderPlot({
    # draw the histogram with the specified number of bins
    ggplot(data_colors()$data_df, aes(x = ABDOMEN, fill = ab_select)) +
      geom_histogram(binwidth = 5, boundary = 0, title = "Abdomen") +
      theme(legend.position = "none") +
      theme_bw()
  })
  
  output$p2Plot <- renderPlot({
    # draw the histogram with the specified number of bins
    ggplot(data_colors()$data_df, aes(x = WRIST, fill = wr_select)) +
      geom_histogram(binwidth = 0.5, boundary = 0, title = "Wrist Circumference") +
      theme(legend.position = "none") +
      theme_bw()
  })
  
  output$TDplot <- renderPlotly({
    # create and save the linear model
    model <- lm(BODYFAT ~ ABDOMEN + WRIST, data = bodyFat)
    
    # create a new data frame with all combinations of ADBOMEN and WRIST
    bodyFat_grid <- tidyr::crossing(ABDOMEN = seq(min(bodyFat$ABDOMEN), max(bodyFat$ABDOMEN), length.out = 100),
                                    WRIST = seq(min(bodyFat$WRIST), max(bodyFat$WRIST), length.out = 100))
    
    # add the predicted values of BODYFAT from the model
    bodyFat_grid$BODYFAT <- predict(model, newdata = bodyFat_grid)
    
    # plot the 3D scatter plot with the plane
    plot_ly(data = data_colors()$data_df) %>%
      add_trace(x = ~ABDOMEN, y = ~WRIST, z = ~BODYFAT, type = "scatter3d", mode = "markers",
                marker = list(size = 2 , color = "black")) %>%
      layout(scene = list(xaxis = list(title = "Abdomen (cm)"),
                          yaxis = list(title = "Wrist (cm)"),
                          zaxis = list(title = "Body Fat (%)")),
             title=paste0("<b>Body Fat Estimation Based on Abdomen and Wrist Circumference</b><br><sup>", 
                          paste0("The predicted Body Fat is ", round(predict(model, newdata=data.frame(ABDOMEN=input$ab, WRIST=input$p2)), 2), "%"), 
                          "</sup>")) %>%
      add_trace(type = "mesh3d", data = bodyFat_grid,
                x = ~ABDOMEN, y = ~WRIST, z = ~BODYFAT,
                opacity = 0.5, showscale = FALSE) %>%
      add_trace(x = c(input$ab), y = c(input$p2), 
                z = c(predict(model, newdata=data.frame(ABDOMEN=input$ab, WRIST=input$p2))), 
                type="scatter3d", mode="markers", marker=list(color="red", size=5))
  })
  observeEvent(input$calculate, {
    abdomen <- as.numeric(input$abInput)
    wrist <- as.numeric(input$wristInput)
    estimated_bodyfat <- 0.7218 * abdomen - 2.079 * wrist - 9.8304
    output$bodyfatOutput <- renderText({
      paste(round(estimated_bodyfat, 2), "%")
    })
  })
  
}

shinyApp(ui=ui, server=server)
