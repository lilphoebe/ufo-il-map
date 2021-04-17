
library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(stringr)
library(shinythemes)
library(shinydashboard)
setwd("~/Documents/Codes/ufo/ufoapp")
ufo <- read.csv("clean_il_ufo")

cov2 <- colorFactor(palette = c("#FF0A33", "#FFFFFF", "#DEDEE3","#FFAD62","#6CC678","#F49097","#CF8D4F", "#D5EBEB","#2BA3DE", "#5D5B5D", "#F5E960","#FFC833","#C3B2FB", "#00AEB8","#44DFCD"), 
                    levels = c("red", "white", "colorless", "orange", "green", "pink", "copper", "silver", "blue", "black", "yellow", "amber", "purple", "teal", "turquoise"))

#main body - map, comment panel
#used shinydashboard boxes but fluidpage layout
body <- mainPanel(
  fluidRow(
    box(
      leafletOutput(outputId = "ufomap"),
      width = 11,
      status = "primary"
      ),
    fluidRow(
    box(
      tableOutput("searchcomments"), 
      width = 11, 
      status = "primary",
      style = "overflow-y: scroll; max-height: 210px"
      )
    )
  )
)



ui <- fluidPage(theme = shinytheme("cyborg"),
                #extra stylesheet for google fonts
                tags$link(rel = "stylesheet", type="text/css", href="extrastyles.css"),
                #HTML for google font - @import has issues on computer
                includeHTML("www/fontfix.html"),

    # title box & gif
    titlePanel(title = tags$div(
      tags$div(img(src='UFO_ScoutCraft2.gif')),
      tags$div(
                  h2("UFO Sightings in Illinois")
                   ))),
    # Sidebar
        sidebarPanel(
            checkboxInput("heatmode", label = h4("Heatmap mode"), value = FALSE),
            textInput("searchterms", label = h4("Search comments:")),
            hr(),
            fluidRow(column(5, verbatimTextOutput("searchterms"))),
            dateRangeInput("years", label = h4("Date range:"),
                           start = min(ufo$datetime), 
                           end = max(ufo$datetime),
                           min = min(ufo$datetime), 
                           max = max(ufo$datetime)),
            
            hr(),
            fluidRow(
              tags$div(
                tags$div("data 1999-2014"),
                "dataset courtesy of ",
                tags$a(href="https://www.kaggle.com/NUFORC/ufo-sightings", 
                       "kaggle")),
                tags$div(
                  "read full encounters at ",
                  tags$a(href="http://www.nuforc.org/webreports/ndxevent.html", 
                         "NUFORC")))),
            body)


#server
server <- function(input, output) {
    output$value <- renderPrint({ input$text })
    output$value <- renderPrint({ input$years })
    #applying relevant filters from input
    finish <- function(data) {
        data %>%
            filter(str_detect(str_to_lower(ufo$comments), str_to_lower(input$searchterms))) %>%
            filter(datetime >= input$years[1]) %>%
            filter(datetime <= input$years[2]) %>%
            arrange(datetime)
        
    }
    #initial map output
    output$ufomap <-
      renderLeaflet({leaflet(finish(ufo)) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            addTiles("CartoDB.DarkMatter") %>%
            addCircleMarkers(lng = ~longitude,
                             lat = ~latitude,
                             popup=finish(ufo)$label, weight = 3, radius=4, 
                             color=~cov2(colors), stroke = F, fillOpacity = 0.6)})
    #deals with off/on of heatmap
    observe({
      proxy <- leafletProxy("ufomap", data = finish(ufo))
      if (input$heatmode) {
        proxy %>% clearMarkers()
        proxy %>% 
          addHeatmap(lng=~longitude, 
                     lat=~latitude, 
                     blur =  30, 
                     max = 0.15, 
                     radius = 18)
      }
      else{
        proxy %>% clearHeatmap()
        proxy %>%
          addProviderTiles(providers$CartoDB.DarkMatter) %>%
          addTiles("CartoDB.DarkMatter") %>%
          addCircleMarkers(lng = ~longitude,
                           lat = ~latitude,
                           popup=finish(ufo)$label, weight = 3, radius=4, 
                           color=~cov2(colors), stroke = F, fillOpacity = 0.6)
        
      }
    })
    #dynamically filters comment box to match bounds of map
    in_bounding_box <- function(data, boundsinput) {
        bounds <- boundsinput
        data %>%
            filter(latitude > bounds$south & latitude < bounds$north &
                  longitude < bounds$east & longitude > bounds$west)
    }
    #tables comments after filtering 
    comment_table <- function() {
      m <- data.frame(
        strftime(
          (in_bounding_box(finish(ufo), req(input$ufomap_bounds))$datetime),
          "%d/%m/%Y, %H:%M"
        ),
        in_bounding_box(finish(ufo), req(input$ufomap_bounds))$comments
      )
      colnames(m) <- c("date", "comments")
      return (m)
    }
  #renders the table
    output$searchcomments <- renderTable(
      comment_table()
          )
}


# Run the application 
shinyApp(ui = ui, server = server)
