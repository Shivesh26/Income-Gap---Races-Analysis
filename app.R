#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Load libraries, load data, transform data
library(tidyverse)
library(gapminder)
library(shiny)

retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_distribution$race <- str_replace_all(income_distribution$race,"White Alone","White")
income_distribution$race <- str_replace_all(income_distribution$race,"Black Alone","Black")
income_distribution$race[income_distribution$race == "Hispanic (Any Race)"] <- "Hispanic"
race_wealth_retirement <- inner_join(income_distribution,retirement,by=c("year","race"))
exp_ret_savings <- race_wealth_retirement %>%
    mutate(ex_life_sav = income_mean*48*.0001)%>%
    mutate(adj_retirement = retirement*.001)
retirement_adj <- retirement %>%
    mutate(adj_retirement = retirement*.001)
target <- c("White", "Black", "Hispanic")
income_dist_up <- filter(income_distribution, race %in% target) %>%
    mutate(income_mean = (income_mean/1000))


# Define UI for application that draws a scatterplot
ui <- fluidPage(
    
    # Application title
    titlePanel("Income wealth gap"),
    theme = shinythemes::shinytheme('superhero'),
    p("Brush the retirement savings timeline to show data in scatterplot."), # Adds text 
    p(),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(12,
               plotOutput(outputId = "dynamic2_plot", height = "700px")
        )
    ),
    
    # Show a plot of the generated distribution
    fluidRow(column(12,{
        plotOutput(outputId = "cor_plot", width = "100%", height = "500px", brush = "plot_brush")
    }
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$dynamic2_plot = renderPlot({
        ggplot(data = semi_join(exp_ret_savings, brushedPoints(exp_ret_savings, input$plot_brush)),
               aes(ex_life_sav, adj_retirement, colour = race)) +
            geom_point(alpha = .8,size = 5) +
            xlab(label = "Expected Retirement savings ($100k)" )+
            ylab(label = "Real Retirement savings ($100k)")+
            theme_minimal()+
            theme(legend.position = "top")}, height = 700)
    
    # Correlation timeline plot
    output$cor_plot <- renderPlot({
        ggplot(retirement_adj, aes(x= year, y = adj_retirement, group = race, color = race)) +
            geom_line() +
            geom_point(data = brushedPoints(exp_ret_savings, input$plot_brush), size = 3) +
            labs(y = "Retirement Savings") +
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
