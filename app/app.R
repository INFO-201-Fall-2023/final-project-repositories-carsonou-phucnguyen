library(dplyr)
library(stringr)
library(shiny)
library(tidyverse)
library(ggplot2)
library(maps)
library(mapdata)
library(usmap)
library(plotly)

df <- read.csv("MergedDataframe.csv")
outliers_df <- df %>%
  filter(CS_to_EV_Ratio_Percent > median(CS_to_EV_Ratio_Percent) + 
           IQR(CS_to_EV_Ratio_Percent)/2 |
           CS_to_EV_Ratio_Percent < median(CS_to_EV_Ratio_Percent) - 
           IQR(CS_to_EV_Ratio_Percent)/2)
no_outliers_df <- df[!df$X %in% outliers_df$X,]
county_df <- summarize(
  group_by(df, County, fips, state),
  EV_Pop = sum(Total_Electric_Vehicles),
  CS_Pop = sum(Total_Charging_Stations),
  CS_EV_Per = sum(Total_Charging_Stations) / sum(Total_Electric_Vehicles) * 100
)

ui <- fluidPage(
  tags$style(HTML("h1{background-color: #ccd5d8;color: Black;}")),
  navbarPage("EVs and Charging Stations in Washington",
    tabPanel(title = "Introduction", 
      titlePanel(h1("Examining the Population of Electric Vehicles and EV 
                 Charging Stations in the State of Washington", align = "center")),
      br(),
      h3(strong("Background Information"), align = "center"),
      p("These graphs are mainly to compare and contrast correlations between Electric Vehicles 
        (EVs) and Electric Vehicle Charging Stations. The area of focus of this study is on 
        counties in Washington, USA. This information is sourced by data.gov and Department of 
        Transportation based on data from 2020. We are looking at the data results to analyze 
        evidence and incur whether or not there is imbalance or unique correlation in the 
        counties of Washington. Further information is provided about each of the graphs given."),
      br(),
      img(src = "Seattle_aerial_2,_May_2023.png", width = "750px",
          style="display: block; margin-left: auto; margin-right: auto;")
    ),
    tabPanel(title = "Zooming Out", 
      titlePanel(h1("Zooming Out on the State of Washington", align = "center")),
      br(),
      h3(strong("About the Plot"), align = "center"),
      p("In this map we zoomed out on all the zip codes and grouped them into counties in 
        Washington state. We then made a density map analyzing each county and which county had 
        the highest charging station to electric vehicle percentage. The higher the percentage, 
        the darker the color for the county. For this map, we had to use another library to 
        plot the map instead of ggplot, and we ended up using usmaps library for this map."),
      p("Unequal access to charging stations is evident in some Washington counties, although 
        most counties are shown to be similar, others that have close to a zero percentile are 
        prone to less accessibility. As electric vehicle adoption grows, addressing these 
        disparities is crucial for equitable access and a viable future."),
      br(),
      sidebarLayout(position = 'left',
        sidebarPanel(
          selectInput(
            inputId = "density_color",
            label = "Choose a color",
            choices = c("Blue", "Red", "Dark Green", "Black")
          )
        ),
        mainPanel(
          plotlyOutput(outputId = "county_plot")
        )
      )
    ),
    tabPanel(title = "Outliers",
      titlePanel(h1("Outliers in our Dataset", align = "center")),
      br(),
      h3(strong("About the Plot"), align = "center"),
      p("In this scatter plot, we analyzed the dataset in terms of how many electric vehicles 
        were registered to each zip code in the state of Washington, and how many charging 
        stations were in each zip code in the state of Washington as well. The red dots refer 
        to outliers in our dataset, where the county had a charging station to electric vehicle 
        ratio that was outside the interquartile range, while the blue dots refer to zip codes 
        that fell within that interquartile range. The orange line shows the linear regression of 
        all datapoints, while the purple line only shows the linear regression of the points that 
        were not outliers."),
      p("Comparing the two regression lines, there is a different in the lines when outliers are 
        included, this leads to the result that shows outlying results tend to shift the balance 
        of the graph. Considering this, we should move forward in focusing on providing balance 
        into the outliers and creating a fair range within the Washington counties, making it so 
        more people are less prone to worry whether they are in a fair range of a charging 
        station if they decide to buy an electric vehicle."),
      br(),
      sidebarLayout(position = 'left',
        sidebarPanel(
          checkboxInput("outlier_points", "Hide all outliers?", 
                        value = FALSE),
          checkboxInput("outlier_line", "Show the linear regression line including outliers?", 
                        value = TRUE),
          checkboxInput("no_outlier_line", "Show the linear regression line excluding outliers?", 
                        value = TRUE)
        ),
        mainPanel(
          plotlyOutput(outputId = "outlier_plot")
        )
      )
    ),
    tabPanel(title = "Comparison",
      titlePanel(h1("Comparing Percentages", align = "center")),
      br(),
      h3(strong("About the Plot"), align = "center"),
      p("In this bar chart, we analyzed each county's charging station to electric vehicle 
        percentage and ordered the counties from highest to lowest percentage. The colored 
        vertical line refers to the average percentage of all the counties. In the chart, you 
        can see that there are a couple counties (Garfield, Ferry) with a percentage of 0%, 
        and a county (Columbia) with a very high percentage."),
      p("We can come to a conclusion that some counties in Washington have less access to 
        charging stations and over half of the counties are below average. As the growth of 
        EVs continues, we must recognize these indifference to both provide to consumers of 
        EVs and pave a road to a more sustainable future."),
      br(),
      sidebarLayout(position = 'left',
        sidebarPanel(
          selectInput(
            inputId = "comparison_color",
            label = "Choose a color",
            choices = c("Blue", "Red", "Gold", "Green")
          ),
          checkboxInput("comparison_line", "Show the average percentage line?", 
                        value = TRUE)
        ),
        mainPanel(
          plotlyOutput(outputId = "comparison_plot")
        )
      )
    ),
  )
)

server <- function(input, output) {
  output$county_plot <- renderPlotly({
    p <- plot_usmap(data = county_df, values = "CS_EV_Per", include = c("WA"), 
                    color = input$color) +
      scale_fill_continuous(low = "white", high = input$density_color, name = "Percentage") + 
      labs(title = "Percentage of Charging Stations per Electric Vehicle per County")
    p <- ggplotly(p)
    return(p)
  })
  output$outlier_plot <- renderPlotly({
    p <- ggplot(df, aes(y = Total_Electric_Vehicles, x = Total_Charging_Stations)) +
      geom_point(data = no_outliers_df, color = "blue", alpha = 0.25) + 
      geom_point(data = outliers_df, color = "red", alpha = 0.25 * !input$outlier_points) +
      geom_line(stat = "smooth", method = "lm", se = FALSE, color = "orange", 
                alpha = as.integer(input$outlier_line) * !input$outlier_points) + 
      geom_line(stat = "smooth", data = no_outliers_df, method = "lm", color = "purple",
                alpha = as.integer(input$no_outlier_line)) +
      labs(x = "Total Charging Stations", y = "Total Electric Vehicles", 
           title = "Total Electric Vehicles in Comparison to Total Charging Stations per County")
    p <- ggplotly(p)
    return(p)
  })
  output$comparison_plot <- renderPlotly ({
    p <- ggplot(county_df, aes(x = CS_EV_Per, y = reorder(County, CS_EV_Per))) +
      geom_bar(stat = "identity") +
      geom_vline(xintercept = mean(county_df$CS_EV_Per), color = input$comparison_color,
                 alpha = as.integer(input$comparison_line)) +
      labs(x = "Charging Station to Electric Vehicle Percentage", y = "County Name", 
           title = "Each County's Charging Station to Electric Vehicle Percentage")
    p <- ggplotly(p)
    return(p)
  })
}

shinyApp(ui = ui, server = server)
