library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(shinyBS)
library(tidyverse)

bf <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/Data/BodyFat.csv")

# convert all units to metric
bf$WEIGHT <- bf$WEIGHT / 2.20462  # kg
bf$HEIGHT <- bf$HEIGHT * 2.54     # cm

bodyFat <- bf %>% filter(BODYFAT > 3, HEIGHT > 90, WEIGHT < 160, 
                         ADIPOSITY < 45, NECK < 50, ABDOMEN < 140, 
                         HIP < 140, THIGH < 80, KNEE < 47, 
                         ANKLE < 32, BICEPS < 42)

ab_plot_min <- floor(min(bodyFat$ABDOMEN) / 5) * 5
ab_plot_max <- ceiling(max(bodyFat$ABDOMEN) / 5) * 5
ab_bins <- seq(ab_plot_min, ab_plot_max, by = 5)
wrist_plot_min <- floor(min(bodyFat$WRIST) / 0.5) * 0.5
wrist_plot_max <- ceiling(max(bodyFat$WRIST) / 0.5) * 0.5
wrist_bins <- seq(wrist_plot_min, wrist_plot_max, by = 0.5)

ab_hist_plot <- function(input_ab) {
  # generate bins based on input$ab from ui.R
  ab_df <- data.frame(ABDOMEN = bodyFat$ABDOMEN)
  input_ab_grp <- cut(input_ab, breaks = ab_bins, labels = FALSE)
  ab_df$grp <- cut(ab_df$ABDOMEN, ab_bins, labels = FALSE, include.lowest = TRUE, right = TRUE)
  ab_df$ab_select <- factor(ab_df$grp == input_ab_grp, labels = c("Study Participants", "You!"))
  
  if (input_ab_grp %in% ab_df$grp) {
    # draw the histogram with the specified number of bins
    ggplot(ab_df, aes(x = ABDOMEN, fill = ab_select)) +
      geom_histogram(breaks = ab_bins, boundary = 0) +
      labs(title = "Abdomen", fill = "") + 
      xlab("Abdomen Circumference [cm]") + 
      ylab("Count") +
      theme_minimal() + 
      scale_fill_manual(values = c("grey", "red"))
  } else {
    ggplot(ab_df, aes(x = ABDOMEN)) +
      geom_histogram(breaks = ab_bins, boundary = 0, fill = "grey") +
      labs(title = "Abdomen", fill = "") + 
      xlab("Abdomen Circumference [cm]") + 
      ylab("Count") +
      theme_minimal()
  }
}

wrist_hist_plot <- function(input_wrist) {
  # generate bins based on input$wrist from ui.R
  wrist_df <- data.frame(WRIST = bodyFat$WRIST)
  
  if ((input_wrist > wrist_plot_min) & (input_wrist < wrist_plot_max)) {
    wrist_grp <- cut(input_wrist, breaks = wrist_bins, labels = FALSE)
    wrist_df$grp <- cut(wrist_df$WRIST, wrist_bins, labels = FALSE, include.lowest = TRUE, right = TRUE)
    wrist_df$wrist_select <- factor(wrist_df$grp == wrist_grp, labels = c("Study Participants", "You!"))
    
    # draw the histogram with the specified number of bins
    ggplot(wrist_df, aes(x = WRIST, fill = wrist_select)) +
      geom_histogram(breaks = wrist_bins, boundary = 0) +
      labs(title = "Wrist", fill = "") + 
      xlab("Wrist Circumference [cm]") + 
      ylab("Count") +
      theme_minimal() + 
      scale_fill_manual(values = c("grey", "red"))
  } else {
    ggplot(wrist_df, aes(x = WRIST)) +
      geom_histogram(breaks = wrist_bins, boundary = 0, fill = "grey") +
      labs(title = "Wrist", fill = "") + 
      xlab("Wrist Circumference [cm]") + 
      ylab("Count") +
      theme_minimal()
  }
}

ui <- fluidPage(
  titlePanel("Bodyfat Estimation"),
  
  # Sidebar with a slider input for age
  sidebarLayout(
    sidebarPanel(
      h2("Where did you land amongst study participants?"),
      p("If you are an adult male, please enter in your body dimensions in!"),
      sliderInput("ab","Abdomen circumference in cm", 
                  min = floor(min(bodyFat$ABDOMEN)) + 1, max = ceiling(max(bodyFat$ABDOMEN)), value = 90),
      sliderInput("wrist","Wrist circumference in cm",
                  min = floor(min(bodyFat$WRIST)) + 1, max = ceiling(max(bodyFat$WRIST)),value = 19,step = 0.5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p("Within the study performed in 1970, 252 adult males had their body dimensions measured to correlate values with body fat percentage."),
      p("Below, you can see two histograms of the abdominal circumference and wrist circumference of people from those studies. If you change the sliders on the left side of the screen, you can also see where you land among the study participants!"),
      fluidRow(column(width = 12, plotOutput("abPlot"))),
      fluidRow(column(width = 12, plotOutput("wristPlot"))),
      p("From the study information, the team was able to put together a linear regression algorithm to predict body fat percentage based on abdominal and wrist circumference."),
      fluidRow(column(width = 12, plotlyOutput("TDplot"))),
      p("The equation that we settled on was [BODYFAT] = 0.7218*[ABDOMEN] - 2.079*[WRIST] - 9.8304."),
      p(paste0("What this equation means is that for every one cm increase in abdomen circumference ",
          "(holding wrist circumference constant), body fat percentage increases by 0.7218%. ",
          "For wrist circumference, if we hold abdominal circumference constant, for every 1 ",
          "cm increase in wrist circumference, body fat percentage decreases by 2.079%.")),
      p("Have any questions about the page or the model? Please reach out to our contact at aawei@wisc.edu!")
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
        ab_grp = cut(ABDOMEN, ab_bins, labels = FALSE, include.lowest = TRUE, right = TRUE),
        ab_select = (ab_grp == cut(input$ab, ab_bins, labels = FALSE)),
        # create a group variable for WRIST based on the input slider
        wr_grp = cut(WRIST, wrist_bins, labels = FALSE, include.lowest = TRUE, right = TRUE),
        wr_select = (wr_grp == cut(input$wrist, wrist_bins, labels = FALSE))
      )
    colors <- ifelse(data_df$ab_select & data_df$wr_select, "purple", 
                     ifelse(data_df$ab_select, "green", 
                            ifelse(data_df$wr_select, "blue", 
                                   "black"))) 
    list(data_df = data_df, colors = colors)
  })
  
  output$abPlot <- renderPlot({ab_hist_plot(input$ab)})
  output$wristPlot <- renderPlot({wrist_hist_plot(input$wrist)})
  
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
             title=paste0("<b>Body Fat Estimation</b><br><sup>", 
                          paste0("The predicted Body Fat is ", round(predict(model, newdata=data.frame(ABDOMEN=input$ab, WRIST=input$wrist)), 2), "%"), 
                          "</sup>")) %>%
      add_trace(type = "mesh3d", data = bodyFat_grid,
                x = ~ABDOMEN, y = ~WRIST, z = ~BODYFAT,
                opacity = 0.5, showscale = FALSE) %>%
      add_trace(x = c(input$ab), y = c(input$wrist), 
                z = c(predict(model, newdata=data.frame(ABDOMEN=input$ab, WRIST=input$wrist))), 
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

# Run the application
shinyApp(ui = ui, server = server)
