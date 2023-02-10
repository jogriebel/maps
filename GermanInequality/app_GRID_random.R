# Quelle der geojson-Datei: https://gdz.bkg.bund.de/index.php/default/wfs-verwaltungsgebiete-1-250-000-stand-01-01-wfs-vg250.html

library(shiny)
library(htmltools)
library(leaflet)
library(magrittr)
library(maps)
library(readxl)
library(geojsonio)
library(sp)

#load data

####Load data from xlsx#################
df_1995<-read_excel("Millionaere_random.xlsx", 1)
df_1993<-read_excel("Millionaere_random.xlsx", 2)
df_1989<-read_excel("Millionaere_random.xlsx", 3)
df_1986<-read_excel("Millionaere_random.xlsx", 4)


##clean data seperately###############
df_1995_c<-subset(df_1995, (!is.na(df_1995[,1])))
df_1993_c<-subset(df_1993, (!is.na(df_1993[,1])))
df_1986_c<-subset(df_1986, (!is.na(df_1986[,1])))
df_1989_c<-subset(df_1989, (!is.na(df_1989[,1])))


### Combine all relevant worksheets to one dataframe df
df<-rbind.data.frame(df_1986_c, df_1989_c, df_1993_c, df_1995_c)

df_c<-subset(df, (!is.na(df[,1])))

#get rid of .0 in years
df_c$year=as.integer(df$year)

#######################GIRD-Data###################################

##Load data from xlsx##
df_names<-c( "01",  "04", "07", "10","13", "14", "15", "16")

for(i in 1:8){
  assign(paste("df_g",df_names[i], sep="" ), read_excel("GRID_random.xlsx", i) )
}


#combine all GRID-Dataframes to one dataframe g_all
g_all<-Reduce(function(x,y) merge(x,y, all=TRUE), list( df_g01, df_g04, df_g07, df_g10, df_g13, df_g14, df_g15, df_g16))
colnames(g_all)[3]<- "raumeinheit" #change columnname "county" to "raumeinheit" like in Millionaires.xlsx

#remove rows that have NA in column ID_3
g_all<-subset(g_all, (!is.na(g_all[,1])))

g_all$year=as.integer(g_all$year)

#combine grid and millionaires dataframes 
#to leave the original data.frames intact, first loop through the names that differ, return a named vector of NAs that are concatenated into a list
# with the data.frame using c. Then, data.frame converts the result into an approppriate data.frame for the rbind
df_combi<-rbind(
  data.frame(c(df_c, sapply(setdiff(names(g_all), names(df_c)), function(x) NA))),
  data.frame(c(g_all, sapply(setdiff(names(df_c), names(g_all)), function(x) NA)))
)

colnames(df_combi)[2]<- "AGS"

#Merging the data with the geospatial information
counties<- geojsonio::geojson_read("germany2021.geojson", what = "sp")

#counties is the SpatialPolygonsDataframe containing both: the actual data and the geojson/shapefile information
counties<-sp::merge(counties, df_combi, by="AGS", duplicateGeoms=TRUE)

#shiny UI
ui <- fluidPage(
  titlePanel("Income and wealth inequality in Germany, 1980-2016"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      selectInput("myvar", 
                  label = "Select a variable",
                  choices = c(
                    "Median Income" = "it_5000",
                    "Top 10% income threshold"="it_9000",
                    "Top 1% income threshold" = "it_9900",
                    "Top 10% income share" = "is_9000",
                    "Top 1% income share" = "is_9900",
                    "Number of wealth taxpayers per 100.000 inhabitants" = "itaxed_per_100thousand",
                    "Netwealth per capita"="netwealth_pc",
                    "Grosswealth per capita" = "grosswealth_pc",
                    "Bus wealth per capita" = "buswealth_pc",
                    "Number of Millionaires per 100.000 inhabitants" = "imio_per_100thousand"),
                  selected = "it_5000"),

      
      shinyWidgets::sliderTextInput(inputId="myyear",
                                    label= "Year",
                                    #choices=c(1986, 1989, 1993, 1995),
                                    choices = c(2001, 2004,2007, 2010, 2013, 2014, 2015, 2016),
                                    grid=TRUE),
      
      checkboxInput("legend", "Show legend", TRUE)
    ),
    
    mainPanel( leafletOutput("map", width = "475px", height = "650px"))#, height = "750px" #width = 7,
    
    
  )
  
)


# SERVER
server <- function(input, output,session) {
  
  # selected year
  selectedYear <- reactive({
    subset(counties, year==input$myyear)#subset returns a spatialpolygons dataframe
  })
  
  # selected Variable
  selectedVariable <- reactive({switch(input$myvar, 
                                       "it_5000"=selectedYear()$it_5000,
                                       "it_9000"= selectedYear()$it_9000,
                                       "it_9900"= selectedYear()$it_9900,
                                       "is_9000" = selectedYear()$is_9000,
                                       "is_9900" = selectedYear()$is_9900,
                                       "buswealth_pc"=selectedYear()$buswealth_pc,
                                       "itaxed_per_100thousand"=selectedYear()$itaxed_per_100thousand,
                                       "netwealth_pc"=selectedYear()$netwealth_pc,
                                       "grosswealth_pc"=selectedYear()$grosswealth_pc,
                                       "imio_per_100thousand"=selectedYear()$imio_per_100thousand
  )
  })
  
  
  
  colorpal<-colorNumeric(palette = "YlGnBu", domain = NULL)
  
  
  
  output$map <- renderLeaflet({
    leaflet(selectedYear()) %>% 
      addTiles() %>% 
      setView(lng= 10.5, lat=51.312801, zoom = 6.25) %>% #6
      addPolygons(
        layerId = ~ID_3,
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
  
  
 
  # #Dynamic UI for different sets of years in the data
  observeEvent(input$myvar, {
    choices<-switch(
      input$myvar,
      "it_5000"= c(2001, 2004, 2007, 2010, 2013, 2014, 2015, 2016),
      "it_9000"= c(2001, 2004, 2007, 2010, 2013, 2014, 2015, 2016),
      "it_9900"= c(2001, 2004, 2007, 2010, 2013, 2014, 2015, 2016),
      "is_9900"= c( 2001, 2004, 2007, 2010, 2013, 2014, 2015, 2016),
      "is_9000"= c( 2001, 2004, 2007, 2010, 2013, 2014, 2015, 2016),
      "buswealth_pc"=c(1986, 1989, 1993, 1995),
      "itaxed_per_100thousand"=c(1986, 1989, 1993, 1995),
      "netwealth_pc"=c(1986, 1989, 1993, 1995),
      "grosswealth_pc"=c(1986, 1989, 1993, 1995),
      "imio_per_100thousand"=c(1986, 1989, 1993, 1995)
 
    )
    
    # choices<- sort(unique(selectedVariable()["year"]))
    shinyWidgets::updateSliderTextInput( session=session,
                                         inputId = "myyear",
                                         choices=choices)
  }, ignoreInit = TRUE)
  
  
  
  
  
  #reactive labels
  observeEvent(c(input$myyear,input$myvar),{
    
    varlabel<-switch(
      input$myvar,
      "it_5000"= "Median Income",
      "it_9000"= "Top 10% income threshold",
      "it_9900"= "Top 1% income threshold",
      "is_9900"=  "Top 1% income share",
      "is_9000"= "Top 10% income share",
      "buswealth_pc"= "Bus wealth per capita",
      "itaxed_per_100thousand"= "Number of wealth taxpayers per 100.000 inhabitants",
      "netwealth_pc"="Netwealth per capita",
      "grosswealth_pc"="Grosswealth per capita",
      "imio_per_100thousand"= "Number of Millionaires per 100.000 inhabitants"
    )

    
    year_popup <- sprintf(
      "<strong>%s</strong><br/> <strong>%s:</strong> %g ",
      selectedYear()$raumeinheit,varlabel ,selectedYear()[[input$myvar]]
    ) %>% lapply(htmltools::HTML)
    
    
    leafletProxy("map", data = selectedYear()) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~colorpal(selectedVariable()),
                  label = year_popup,
                  color = "#BDBDC3",
                  fillOpacity = 0.9,
                  weight = 2,
                  # layerId = ~ID_3,
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


