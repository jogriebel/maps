#Quelle der geojson-Datei: http://www.deutschland-in-daten.de/karten/ (Zitationshinweise beachten)

library(htmltools)
library(leaflet)
library(magrittr)
library(maps)
library(geojsonio)
library(readxl)
library(shiny)
require(sp)

#load data
df_prussia<- read_excel("clean_newnames1891_random.xlsx",1)

#clean data (Entfernen von Kreisen, die keine ID zugewiesen bekommen haben/in der Karte keine haben)
df_prussia<-subset(df_prussia, (!is.na(df_prussia[,1])))

#Nachkommastellen runden
df_prussia$avi_top1<-round(df_prussia$avi_top1, 0)
df_prussia$avi_top5<-round(df_prussia$avi_top5, 0)
df_prussia$is_9500<-round(df_prussia$is_9500, 2)
df_prussia$is_9900<-round(df_prussia$is_9900, 2)

#geojson-Datei (Kartengerüst)
counties_prussia<- geojsonio::geojson_read("german_empire_1891.geojson", what = "sp")

#Verknüpfung Karte (geojson) mit Daten
counties_prussia<-sp::merge(counties_prussia, df_prussia, by="ID", duplicateGeoms=TRUE)


#shiny UI
ui <- fluidPage(
  titlePanel("Income Inequality in Prussia, 1895-1910"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput("myvar", 
                  label = "Select a variable",
                  choices = c(
                    "Top 5% income share" = "is_9500",
                    "Top 1% income share" = "is_9900",
                    "Top 1% average income" = "avi_top1",
                    "Top 5% average income" = "avi_top5"),
                  selected = "is_9500"),
      
      shinyWidgets::sliderTextInput(inputId="myyear",
                                    label= "Year",
                                    choices = c( 1895, 1896, 1897, 1898, 1899,1900,
                                                1901, 1902, 1903, 1904, 1905, 1906, 1907, 1908, 1909, 1910 ),
                                    grid=TRUE),
      checkboxInput("legend", "Show legend", TRUE)
    ),
    
    mainPanel( leafletOutput("map", width = "800px", height = "650px"))#, height = "750px" #width = 7,
  )
)


# SERVER
server <- function(input, output,session) {
  
  # selected year
  selectedYear <- reactive({
    #additional conditions to have counties with missings durchsichtig instead of grey
     subset(counties_prussia, year==input$myyear & is_9500!=is.na(counties_prussia$is_9500)& avi_top5!=is.na(counties_prussia$avi_top5))#subset returns a spatialpolygons dataframe
    
  })
  
  # selected Variable
  selectedVariable <- reactive({switch(input$myvar, 
                                       "is_9500" = selectedYear()$is_9500,
                                       "is_9900" = selectedYear()$is_9900,
                                       "avi_top1"=selectedYear()$avi_top1,
                                       "avi_top5"=selectedYear()$avi_top5
  )
  })
  
  
 
  colorpal<-colorNumeric(palette = "YlGnBu", domain = NULL)
  
  
  
  output$map <- renderLeaflet({
    leaflet(selectedYear()) %>%  #das hier passt nicht: Sorgt daf?r, dass nicht einfach alle Kreise gemapped werden 
      addTiles() %>% 
      setView(lng= 14.62395, lat=51.62355, zoom = 6) %>% #6
      addPolygons(
        fillColor =~colorpal(selectedVariable()),
        color = "#BDBDC3",
        fillOpacity = 0.9,
        weight = 2,
        dashArray = "5",
        highlight = highlightOptions(
          weight = 4,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE)
      )
    
  })
  
  
  
  
  
  
  #reactive labels
  observeEvent(c(input$myyear,input$myvar),{
    
    varlabel<-switch(
      input$myvar,
      "is_9900"=  "Top 1% income share",
      "is_9500"= "Top 5% income share",
      "avi_top1"= "Top 1% average income",
      "avi_top5"= "Top 5% average income"
    )
    
    year_popup <- sprintf(
      "<strong>%s</strong><br/> <strong>%s:</strong> %g ",
      selectedYear()$county_name, varlabel ,selectedYear()[[input$myvar]]
    ) %>% lapply(htmltools::HTML)
    
    
    leafletProxy("map", data = selectedYear()) %>%
      clearShapes() %>%
      addTiles()%>%
      addPolygons(fillColor = ~colorpal(selectedVariable()),
                  label = year_popup,
                  color = "#BDBDC3",
                  fillOpacity = 0.9,
                  weight = 2,
                  dashArray = "5",
                  highlight = highlightOptions(
                    weight = 4,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.9,
                    bringToFront = TRUE)
      )
  })
  
  
  
  
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = selectedYear())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal
      proxy %>% addLegend(position = "bottomleft",
                          pal = pal, values = selectedVariable()
      )
    }
  })
  
} 


shinyApp(ui = ui, server = server)

