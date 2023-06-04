library(shiny)
library(tidyverse)
library(ggplot2)
library(usmap)
library(plotly)

df <- read.csv("MergedDataframe.csv")
outliers_df <- df %>%
  filter(CS_to_EV_Ratio_Percent > median(CS_to_EV_Ratio_Percent) +
           IQR(CS_to_EV_Ratio_Percent) / 2 |
           CS_to_EV_Ratio_Percent < median(CS_to_EV_Ratio_Percent) -
           IQR(CS_to_EV_Ratio_Percent) / 2)
no_outliers_df <- df[!df$X %in% outliers_df$X, ]
county_df <- summarize(
  group_by(df, County, fips, state),
  EV_Pop = sum(Total_Electric_Vehicles),
  CS_Pop = sum(Total_Charging_Stations),
  CS_EV_Per = sum(Total_Charging_Stations) / sum(Total_Electric_Vehicles) * 100
)

zoom <- tabPanel(
  title = "Zooming Out",
  titlePanel(h1("Zooming Out on the State of Washington",
                align = "center",
                style = "color:blue; background-color:lightblue"
  )),
  br(),
  h3(strong("About this Area Plot"), align = "center"),
  p("In this map we zoomed out on all the zip codes and grouped them into significant counties in
        the Washington state. We then made a density map analyzing each county and determined each counties'
        charging station to electric vehicle percentage. The higher the percentage,
        the darker the color for the county. For this map, we had to use another library to
        plot the map instead of ggplot, and we ended up using usmaps library for this map of Washington."),
  br(),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectInput(
        inputId = "density_color",
        label = "Choose a color",
        choices = c("Blue", "Red", "Yellow", "Dark Green", "Black")
      ),
      h3("Results"),
      p("Unequal access to charging stations is evident in some Washington counties, although
          most counties are shown to be similar, others that have close to a zero percentile are
          prone to less accessibility. As electric vehicle adoption grows, addressing these
          disparities is crucial for equitable access and a viable future.")
    ),
    mainPanel(
      plotlyOutput(outputId = "county_plot"),
      p("Hover over counties to reveal Chraging Station to Electric Vehicle percentage (For all graphs)")
    )
  )
)

comparison <- tabPanel(
  title = "Comparison",
  titlePanel(h1("Comparing Percentages",
                align = "center",
                style = "color:green; background-color:#d6f6d5"
  )),
  br(),
  h3(strong("About this Barplot"), align = "center"),
  p("In this bar chart, we analyzed each county's charging station to electric vehicle
        percentage and ordered the counties from highest to lowest percentage. The colored
        vertical line refers to the average percentage of all the counties. In the chart, you
        can see that there are a couple counties (Garfield, Ferry) with a percentage of 0%,
        and a county (Columbia) with a very high percentage."),
  br(),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectInput(
        inputId = "comparison_color",
        label = "Choose a color for the avg line",
        choices = c("Blue", "Red", "Gold", "Green")
      ),
      p("Select to view"),
      checkboxInput("comparison_line", "Average Percent Line",
                    value = TRUE
      ),
      checkboxInput("rect_above_avg", "Portion above average",
                    value = TRUE
      ),
      checkboxInput("rect_below_avg", "Portion below average",
                    value = TRUE
      ),
      checkboxInput("labels", "Charging Station to Electric Vehicle Percentages",
                    value = FALSE
      ),
      checkboxInput("label_line", "Average Line Percentage",
                    value = FALSE
      ),
      textInput("color", "Color for Plot (must be Rstudio or Hex)", value = "white"),
      h2("Results"),
      p("We can come to a conclusion that some counties in Washington have less access to
          charging stations and over half of the counties are below average. As the growth of
          EVs continues, we must recognize these indifference to both provide to consumers of
          EVs and pave a road to a more sustainable future.")
    ),
    mainPanel(
      plotlyOutput(outputId = "comparison_plot"),
    )
  )
)

outliers <- tabPanel(
  title = "Outliers",
  titlePanel(h1("Outliers in our Dataset",
                align = "center",
                style = "color:#ff5349; background-color:#fcd299"
  )),
  br(),
  h3(strong("About this Scatter Plot"), align = "center"),
  p("In this scatter plot, we analyzed the dataset in terms of how many electric vehicles
        were registered to each zip code in the state of Washington, and how many charging
        stations were in each zip code in the state of Washington as well. The red dots refer
        to outliers in our dataset, where the county had a charging station to electric vehicle
        ratio that was outside the interquartile range, while the blue dots refer to zip codes
        that fell within that interquartile range. The orange line shows the linear regression of
        all datapoints, while the purple line only shows the linear regression of the points that
        were not outliers."),
  br(),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      checkboxInput("outlier_points", "Hide all outliers?",
                    value = FALSE
      ),
      checkboxInput("outlier_line", "Show the linear regression line including outliers?",
                    value = TRUE
      ),
      checkboxInput("no_outlier_line", "Show the linear regression line excluding outliers?",
                    value = TRUE
      ),
      sliderInput("slider", "Filter by total charging stations (per county)",
                  min = 0,
                  max = 125,
                  value = c(0, 125)
      ),
      textOutput(outputId = "slide"),
      h2("Results"),
      p("Comparing the two regression lines, there is a different in the lines when outliers are
          included, this leads to the result that shows outlying results tend to shift the balance
          of the graph. Considering this, we should move forward in focusing on providing balance
          into the outliers and creating a fair range within the Washington counties, making it so
          more people are less prone to worry whether they are in a fair range of a charging
          station if they decide to buy an electric vehicle.")
    ),
    mainPanel(
      plotlyOutput(outputId = "outlier_plot")
    )
  )
)
ui <- fluidPage(
  navbarPage(
    "EVs and Charging Stations in Washington",
    tabPanel(
      title = "Introduction",
      tags$style(HTML("h1{background-color: #ccd5d8;color: Black;}")),
      titlePanel(h1("Examining the Population of Electric Vehicles and EV
                 Charging Stations in the State of Washington",
                    align = "center",
                    style = "color:black"
      )),
      p(em("By Carson Ou and Phuc Nguyen"), align = "center"),
      h3(strong("Background Information"), align = "center"),
      p("The focus of our study is mainly to compare and contrast correlations between Electric Vehicles
        (EVs) and Electric Vehicle Charging Stations. The graphs area of focus of the analyzation is on
        counties in Washington, USA. This information is sourced by data.gov and Department of
        Transportation based on data from 2020. We are looking at the data results to analyze
        evidence and incur whether or not there is imbalance or unique correlation in the
        counties of Washington. Further information is provided about each of the graphs given."),
      br(),
      img(
        src = "Seattle_aerial_2,_May_2023.png", width = "750px",
        style = "display: block; margin-left: auto; margin-right: auto;"
      ),
      br(),
      br(),
      HTML("<center><p>
           <b>BUILT BY:</b> Carson Ou and Phuc Nguyen using the power of RStudio and Shiny<br>
           <b>R PACKAGES:</b> tidyverse, shiny, ggplot2, usmaps, plotly<br>
           <b>SOURCES:</b> Electric Vehicle Dataset from
           <a href = 'https://catalog.data.gov/dataset/electric-vehicle-population-data/resource/fa51be35-691f-45d2-9f3e-535877965e69'>
           Data.gov</a>, Charging Station Dataset from
           <a href = 'https://data-usdot.opendata.arcgis.com/datasets/alternative-fueling-stations/explore'>
           US Department of Transportation</a></p></center>"),
      br(),
    ),
    comparison,
    outliers,
    zoom,
  )
)

server <- function(input, output) {
  output$slide <- renderText({
    paste("Selected Range:", input$slider[1], "to", input$slider[2])
  })
  
  output$county_plot <- renderPlotly({
    p <- plot_usmap(
      data = county_df, values = "CS_EV_Per", include = c("WA"),
      color = input$density_color
    ) +
      scale_fill_continuous(low = "white", high = input$density_color, name = "Percentage") +
      labs(title = "Percentage of Charging Stations per Electric Vehicle per County")
    p <- ggplotly(p)
    return(p)
  })
  
  output$outlier_plot <- renderPlotly({
    filt_df <- filter(df, Total_Charging_Stations >= input$slider[1], Total_Charging_Stations <= input$slider[2])
    no_out_filt <- filter(no_outliers_df, Total_Charging_Stations >= input$slider[1], Total_Charging_Stations <= input$slider[2])
    out_filt <- filter(outliers_df, Total_Charging_Stations >= input$slider[1], Total_Charging_Stations <= input$slider[2])
    
    p <- ggplot(filt_df, aes(y = Total_Electric_Vehicles, x = Total_Charging_Stations)) +
      geom_point(data = no_out_filt, color = "blue", alpha = 0.25) +
      geom_point(data = out_filt, color = "red", alpha = 0.25 * !input$outlier_points) +
      geom_line(
        stat = "smooth", method = "lm", se = FALSE, color = "orange",
        alpha = as.integer(input$outlier_line) * !input$outlier_points
      ) +
      labs(
        x = "Total Charging Stations", y = "Total Electric Vehicles",
        title = "Total Electric Vehicles in Comparison to Total Charging Stations per County"
      )
    
    if (input$slider[1] < 28) {
      p <- p + geom_line(
        stat = "smooth", data = no_outliers_df, method = "lm", color = "purple",
        alpha = as.integer(input$no_outlier_line)
      )
    }
    p <- ggplotly(p)
    return(p)
  })
  
  output$comparison_plot <- renderPlotly({
    p <- ggplot(county_df, aes(x = CS_EV_Per, y = reorder(County, CS_EV_Per)), fill = County) +
      geom_bar(stat = "identity") +
      geom_vline(
        xintercept = mean(county_df$CS_EV_Per), color = input$comparison_color,
        alpha = as.integer(input$comparison_line)
      ) +
      labs(
        x = "Charging Station to Electric Vehicle Percentage", y = "County Name",
        title = "Each County's Charging Station to Electric Vehicle Percentage"
      )
    
    if(input$color %in% colors() | grepl("^#?([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", 
                                         input$color, ignore.case = TRUE)) { 
      p <- p + geom_col(fill = input$color, color = "lightblue")
    }
    
    if (input$labels == TRUE) {
      p <- p + geom_text(aes(label = round(CS_EV_Per, 2)), size = 2)
    }
    if (input$label_line) {
      p <- p + annotate("text", x = 4.3, y = 20, label = "3.62")
    }
    if (input$rect_above_avg) {
      p <- p + annotate("rect",
        xmin = 3.62, xmax = 23.1, ymin = "Asotin", ymax = "Columbia", color = "blue",
        fill = "lightblue", alpha = 0.2
      )
    }
    if (input$rect_below_avg) {
      p <- p + annotate("rect",
        xmin = 0, xmax = 3.62, ymin = "Ferry", ymax = "Columbia", color = "red",
        fill = "orange", alpha = 0.2
      )
    }
    p <- ggplotly(p)
    return(p)
  })
}

shinyApp(ui = ui, server = server)