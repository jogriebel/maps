###Zeittrend einer Variablen###
library(shiny)
library(htmltools)
library(leaflet)
library(magrittr)
library(maps)
library(readxl)
library(shinyWidgets)
library(dslabs)
library(tidyverse)
library(plotly)
library(tidyr)
library(ggplot2)
library(openxlsx)

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



###filter data, sodass nur die Kreise enthalten bleiben, die auch auf der Karte enthalten sind
#####Dieser Abschnitt ist da, um die aktuelle Lister der in der Karte verwendeten Kreise zu erstellen über die Kreise, die in der verwendeten geojson-Datei enthalten sind
#gibt es keine Änderungen, dann die Zeilen einfach auskommentiert lassen
# library(geojsonio)
# counties<- geojsonio::geojson_read("germany2021.geojson", what = "sp")
# dfn<-as.data.frame(counties@data$AGS)
# write.xlsx(dfn, "county_names.xlsx")

#wenn die Namensliste schon exisitiert:
dfn<-read.xlsx("county_names.xlsx")

#subset in beiden Fällen
g_all<-subset(g_all, kennziffer %in% dfn[,1])

#alphabetische Sortierung der Kreisnamen
g_all<-g_all[order(g_all$raumeinheit),]

#partial transpose
g_all<- g_all %>% gather(variable, value, -c(year, kennziffer, raumeinheit))


##add column with nice variable names
g_all<-g_all %>%
  mutate(names = case_when(
    variable == "it_9000" ~ "Top 10% income threshold",
    variable == "it_9900" ~ "Top 1% income threshold",
    variable == "is_9900" ~ "Top 1% income share",
    variable == "is_9000" ~ "Top 10% income share",
    variable == "it_5000" ~"Median Income"
    
  ))



#########Shiny Part###################

##Quelle: https://data.library.virginia.edu/getting-started-with-shiny/

f1 <- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "lightgrey"
)
# myx<-list(
#   title="Year",
#   titlefont="f1",
#   showticklabel=TRUE
# )


ui <- fluidPage(
  
  titlePanel("Trends 2001-2016"),
  sidebarLayout(
    sidebarPanel(
      # inputs
      selectizeInput("countyInput", "County",
                     choices = unique(g_all$raumeinheit),  
                     selected="Berlin", multiple =FALSE), 
      checkboxGroupInput("variableInput", "Variable",
                         choices = c(
                           "Top 10% income threshold"="it_9000",
                           "Top 1% income threshold" = "it_9900",
                           "Top 10% income share" = "is_9000",
                           "Top 1% income share" = "is_9900",
                           "Median Income" = "it_5000"),
                         selected = "it_9900"),
      shinyWidgets::sliderTextInput(inputId="yearInput",
                                    label= "Year",
                                    choices = c(2001, 2004,2007, 2010, 2013, 2014, 2015, 2016),
                                    selected = c(2001, 2016),
                                    grid=TRUE),
      
    ),  
    
    mainPanel(
      plotlyOutput("trendplot"),
      br(), br(),
      
    ) 
  )   
)   

server <- function(input, output) {
  
  d <- reactive({
    g_all %>%
      filter(raumeinheit == input$countyInput,
             variable %in% input$variableInput,
             year >= input$yearInput[1],
             year <= input$yearInput[2])
  }) 
  e<- reactive({
    g_all %>%
      filter(raumeinheit == input$countyInput
            )
  }) 

  
  output$trendplot <- renderPlotly({
    pal<- c("green", "red", "blue", "black", "orange")
    pal<-setNames(pal, c( "it_9000",
                          "it_9900",
                          "is_9000",
                          "is_9900",
                          "it_5000" ))
    
   
    fig <- plot_ly(d(), x = ~year, y = ~value, color= ~variable, name=~names, colors=pal,type = 'scatter', mode = 'lines+markers') %>%
      layout(showlegend  = TRUE)
    fig<-fig%>%layout(hovermode="x unified", xaxis=list(title="Year",
                                                        ticktext=list("2001", "2004","2007", "2010", "2013", "2014", "2015", "2016"), 
                                                        tickvals=list(2001, 2004,2007, 2010, 2013, 2014, 2015, 2016), 
                                                        tickmode="array"),
                      yaxis=list(range=c(0, max(e()$value, na.rm = TRUE)+10000),
                                 title="")
                      
                      )
    
  })
}

shinyApp(ui=ui, server=server)
