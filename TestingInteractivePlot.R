library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(patchwork)
library(stringr)
library(maps)

#Import Data
Future50 <- read.csv("/Users/swathib/Desktop/Internship Risers/archive/Future50.csv")

#Future50$Events <- ordered(Future50$Event, levels = c("50 Free", "100 Free", "200 Free", "500 Free", "1000 Free", "1650 Free", "100 Fly", "200 Fly", "100 Back", "200 Back", "100 Breast", "200 Breast", "100 IM", "200 IM", "400 IM", "200 Free Relay", "400 Free Relay", "800 Free Relay", "200 Medlay Relay", "400 Medlay Relay"))
Future50 = separate(data = Future50, col = Location, into = c("City", "State"), sep = ",")
Future50= mutate(Future50, new_bin_Sales = cut(Sales, breaks=4))
Future50$new_bin_Sales

Future50$State[Future50$State==" Wash."]="Washington"
Future50$State[Future50$State==" N.C."]="North Carolina" 
Future50$State[Future50$State==" Calif."]="California"
Future50$State[Future50$State=="  Calif."]="California"
Future50$State[Future50$State==" N.J."]="New Jersey"
Future50$State[Future50$State=="  N.J."]="New Jersey"
Future50$State[Future50$State==" Pa."]="Pennsylvania"
Future50$State[Future50$State==" Ky."]="Kentucky"
Future50$State[Future50$State==" S.C."]="South Carolina"
Future50$State[Future50$State==" Colo."]="Colorado"
Future50$State[Future50$State==" Fla."]="Florida"
Future50$State[Future50$State==" Mo."]="Montana"
Future50$State[Future50$State==" N.Y."]="New York"
Future50$State[Future50$State==" Va."]="Virginia"
Future50$State[Future50$State==" Ga."]="Georgia"
Future50$State[Future50$State==" Ariz."]="Arizona"
Future50$State[Future50$State==" Neb."]="Nebraska"
Future50$State[Future50$State==" Ore."]="Oregon"
Future50$State[Future50$State==" Ark."]="Arkansas"
Future50$State[Future50$State==" Tenn."]="Tennessee"
Future50$State[Future50$State==" Texas"]="Texas"
Future50$State[Future50$State==" Ohio"]="Ohio"
Future50$State[Future50$State==" D.C."]="D.C."

state_level_df <- data.frame(State = tolower(state.name), 
                             long = state.center$x, 
                             lat = state.center$y,
                             stringsAsFactors = FALSE)
Future50$StateCap = Future50$State
Future50$State = tolower(Future50$State)
Future50 = merge(x=Future50,y=state_level_df,by="State",all.x=TRUE)

Future50$lat[Future50$State=="D.C."]=38.9072
Future50$long[Future50$State=="D.C."]=77.0369
us = map_data("state")
# mmss_format <- function(x, ...) {
#   sec <- x%%60
#   min <- x%/%60
#   sec <- base::sprintf("%05.2f", sec)
#   ifelse(min == 0, paste(sec),
#          paste(min, sec, sep = ":"))
# }

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# Define UI
ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("Restaurants", theme = shinytheme("lumen"),
             tabPanel("Future 50", fluid = TRUE, icon = icon("globe-americas"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Desired Restaurant Characteristics"),
                          #shinythemes::themeSelector(),
                          fluidRow(column(3,
                                          
                                          # Select which Gender(s) to plot
                                          checkboxGroupInput(inputId = "Franchising",
                                                             label = "Select Franchise Category(s):",
                                                             choices = c("No" = "No", "Yes" = "Yes"),
                                                             selected = "Yes"),
                                          
                                          # Select which Division(s) to plot
                                          checkboxGroupInput(inputId = "FutureSales",
                                                             label = "Select Range(s):",
                                                             choices = c("(20,27.2]", "(27.2,34.5]", "(34.5,41.8]", "(41.8,49]"),
                                                             selected = "(20,27.2]")
                          ),
                          column(6, offset = 2,
                                 # Select which Region(s) to plot
                                 checkboxGroupInput(inputId = "StateFinder",
                                                    label = "Select State(s):",
                                                    choices = c("Washington","North Carolina", "California", "New Jersey",
                                                                "Pennsylvania","Kentucky","South Carolina", "Colorado",
                                                                "Texas","Ohio","Florida","Montana","New York","Virginia",
                                                                "Georgia","Arizona","Nebraska","Oregon","Arkansas","D.C.",
                                                                "Tennessee"),
                                                                selected = "New York"
                                                                ),
                                                    sliderInput(inputId = "Restaurant_RankFinder",
                                                                label = "Restaurant Rank",
                                                                min = 1,
                                                                max = 50,
                                                                value = c(1,50),
                                                                width = "220px")
                                 )),
                          ),
                        mainPanel(
                          fluidRow(
                            column(3, offset = 9,
                                   radioButtons(inputId = "show_NamesFinder",
                                                label = "Display:",
                                                choices = c("Restaurant Names", "City Names", "Neither"),
                                                selected = "Restaurant Names")
                            )),
                          # hr(),
                          withSpinner(plotOutput(outputId = "scatterplotFinder", click = "click_plotFinder"
                          )),
                          tableOutput("restaurtantstableFinder")
                          )
                      )
             )
  )
)


# Define server
server <- function(input, output, session) {
  
  #Restaurant Finder
  
  
  Future50_finder = reactive({
      #req(input$show_NamesFinder)
      filter(Future50, new_bin_Sales %in% input$FutureSales) %>%
      filter(StateCap %in% input$StateFinder) %>%
      filter(Franchising %in% input$Franchising) %>%
      filter(Rank >= input$Restaurant_RankFinder[1])%>%
      filter(Rank <= input$Restaurant_RankFinder[2])%>%
      group_by(State)
  })
  
  output$scatterplotFinder <- renderPlot({
    input$FutureSales
    input$StateFinder
    input$Franchising
    input$RankFinder
    input$show_NamesFinder
    isolate({
      if (length(Future50_finder()) == 0) {
        ggplot() +
          geom_polygon(data = Future50_finder(), aes(x = long, y = lat, group = State), color = "white", fill = "grey") +
          coord_quickmap() +
          theme_void() +
          ggtitle("No programs fit selected characteristics. \nPlease modify selections.") +
          theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 20))
      } else {
        ggplot() + geom_polygon( data=us, aes(x=long, y=lat, group = group),color="white", fill="grey92" )+
          {if(input$show_NamesFinder == "Restaurant Names") geom_text_repel(data = Future50_finder(), aes(x = long, y = lat, label = as.character(Restaurant)))} +
          {if(input$show_NamesFinder == "City Names") geom_text_repel(data = Future50_finder(), aes(x = long, y = lat, label = as.character(City)))} +
          geom_point(data = Future50_finder(), aes(x = long, y = lat,size = Sales), alpha = 0.5) +
          guides(fill = FALSE) +
          theme_void() +
          labs(color = "City", size = "Sales") +
          theme(axis.text = element_blank(), axis.ticks = element_blank()) +
          theme(plot.title = element_text(hjust=0.5, face = "bold")) +
          theme(plot.background = element_rect(fill = "white"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
          guides(alpha = FALSE) +
          theme(legend.text = element_text(size = 12),
                legend.title = element_text(size = 15)) +
          theme(plot.background = element_rect(
            color = "white"))
        
        }
      })
    })
  
 
  output$restaurtantstableFinder<-renderTable({
    #Future50
    filter(Future50, new_bin_Sales %in% input$FutureSales) %>%
      filter(StateCap %in% input$StateFinder) %>%
      filter(Franchising %in% input$Franchising) %>%
      filter(Rank >= input$Restaurant_RankFinder[1])%>%
      filter(Rank <= input$Restaurant_RankFinder[2])
  })
    
  #session$onSessionEnded(stopApp)
}
# Run the application
shinyApp(ui = ui, server = server)

