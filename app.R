# Setup: Install and/or load packages-------------------------------

library(shiny)
library(shinythemes)
library(scales)
library(plotly)
library(leaflet)
library(rgdal)
library(rgeos)
library(tidyverse)
library(RColorBrewer)
library(wesanderson)
library(Hmisc)
library(viridis)
library(hrbrthemes)



#make sure data is all together for script to run----------------------
getwd()


#Data preprocessing for web app-------------------------------
#read simulated Hub data
asthmaDf <- read.csv('fakeAsthmaData.csv')
#asthmaDf$rateper10k = asthmaDf$rate * 10000

# read shapefile
uhfShapeFile <- readOGR('./uhfShapes/UHF42BaseMap', 'shp')
#uhfShapeFile@data
#uhfShapeFile@proj4string
uhfs <- spTransform(uhfShapeFile, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Add labels to levels of variables so that it's more presentable-------------------
str(asthmaDf$asthma)
levels(asthmaDf$asthma)
asthmaDf$asthma = factor(asthmaDf$asthma,
                         levels = c("Any","MildInt","MildPers", "ModPers", "Other", "SevPers", "Unspec"),
                         labels = c("Any", "Mild Intermittent", "Mild Persistent", "Moderate Persistent", "Other", 
                                    "Severe Persistent", "Unspecified"))
levels(asthmaDf$age)
asthmaDf$age = factor(asthmaDf$age,
                      levels = c("00to04",  "05to11",  "12to17",  "18to64",  "65to100"),
                      labels = c("0 to 4",  "5 to 11",  "12 to 17",  "18 to 64",  "65 to 100"))
levels(asthmaDf$sex)
asthmaDf$sex = factor(asthmaDf$sex,
                      levels = c("F",  "M"),
                      labels = c("Female",  "Male"))

#Subsetter Function --------------------------------------------------

subsetter <- function(anySelect, severitySelect, sexSelect, ageSelect, raceSelect){
  df <- asthmaDf
  df <- filter(df, sex %in% sexSelect, age %in% ageSelect, race %in% raceSelect)
  if(anySelect=='Any'){
    df <- filter(df, asthma=='Any')
  } else{
    df <- filter(df, asthma %in% severitySelect)
  }
  dens <- setNames(aggregate(den ~ UHF, df, sum), c('UHF', 'den'))
  nums <- setNames(aggregate(num ~ UHF, df, sum), c('UHF', 'num'))
  uhfData <- merge(merge(uhfs, nums, by='UHF'), dens, by='UHF')
  uhfData$rate <- uhfData$num / uhfData$den
  return(uhfData)
}

#UI-------------------------------------------------------

ui <- navbarPage(
  #UI: theme--------------------
  theme = shinytheme("superhero"),
  
  "Asthma Project",
  #UI: Map Page----------------------
  tabPanel("Map",
           bootstrapPage(titlePanel("Interactive Simulated Asthma Map with Shiny and
                                    Leaflet")),
           
           sidebarPanel(width=4, 
                        selectInput("measure", "Color by:",
                                    c("Number of patients with asthma"='num',
                                      "Number of total patients"='den',
                                      "Rate of patients with asthma"='rate'), 'rate', width='300px'),
                        
                        checkboxGroupInput("sex", strong("Sex"),
                                           unique(asthmaDf$sex), unique(asthmaDf$sex)),
                        
                        checkboxGroupInput("age", "Age",
                                           unique(asthmaDf$age), unique(asthmaDf$age)),
                        
                        checkboxGroupInput("race", "Race",
                                           unique(asthmaDf$race), unique(asthmaDf$race)),
                        
                        radioButtons('any', 'Asthma Dx',
                                     choices=c('Any', 'by Severity'), selected = 'Any'),
                        
                        conditionalPanel(condition = "input.any == 'by Severity'",
                                         checkboxGroupInput("severity", "Asthma Severity",
                                                            unique(asthmaDf[asthmaDf$asthma!='Any',
                                                                            'asthma']),
                                                            unique(asthmaDf[asthmaDf$asthma!='Any',
                                                                            'asthma'])))),
           mainPanel(width = 8,
                     
                     leafletOutput("mymap", height = 600),
                     
                     plotOutput('plot')),
           
           hr(),
           print("NOTE: Underlying data is simulated. Rates and counts shown here do not
                 represent actual Hub numbers. PCIP's Hub is an ad-hoc query system that
                 aggregates electronic health record data.")
           ),    #UI: Overall Summaries page----------------
  tabPanel("Graphs",
           #include a sidebarpanel to filter by UHF neighborhoods
           bootstrapPage(titlePanel("Summary Descriptives")),
            sidebarPanel( style = "position:fixed;width:inherit;",
                          width = 3,
              selectInput("UHFname", "UHF Neighborhoods" ,levels(asthmaDf$name))
            ),
           mainPanel( width = 9,
             plotlyOutput('barplot'),
             hr(), 
             plotlyOutput('barplot2'))
           )
           
           )



server <- function(input, output, session) {
  
  subDf <- reactive({
    validate(
      need(input$sex, 'Select at least one sex category.'),
      need(input$age, 'Select at least one age category.'),
      need(input$race, 'Select at least one race category.')
    )
    if(input$any=='by Severity'){
      validate(
        need(input$severity, 'Select at least one severity category, or select "Any".')
      )      
    }
    
    subsetter(input$any, input$severity, input$sex, input$age, input$race)
  })
  
  colorPal <- reactive({
    colorNumeric('YlOrRd', domain=subDf()$input$measure, na.color = '#577358')
  })
  
  
  #Fix HEERE
  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/><strong>%s</strong><br/>asthmatic patient count: %d<br/>total patients: %d<br/>rate of asthmatic patients: %f",
      subDf()$UHFNAME, subDf()$UHFCODE, subDf()$num, subDf()$den, subDf()$rate
    ) %>% lapply(htmltools::HTML)
  })
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedShape=NULL)
  
  output$mymap <- renderLeaflet({
    leaflet(subDf()) %>%
      setView(-74, 40.7, zoom=10) %>%
      addProviderTiles(providers$CartoDB.Positron)
    # addProviderTiles("MapBox", options = providerTileOptions(
    #   id = "mapbox.light",
    #   accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
  })
  
  observe({
    
    pal <- colorPal()
    leafletProxy('mymap', data=subDf()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(get(input$measure)),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = '#666',
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        layerId=~UHF)%>%
      addLegend(position="bottomright", pal=pal,values=~get(input$measure), title="Hub Patients", 
                layerId = ~UHF, opacity =1 )
    
  })
  
  # store the click
  observeEvent(input$mymap_shape_click,{
    data_of_click$clickedShape <- input$mymap_shape_click
  })
  
  output$plot=renderPlot({
    selectUHF=data_of_click$clickedShape$id
    #pal2 <- wes_palette("Zissou1", 100, type = "continuous")
    row=subset(subDf(), UHF==selectUHF)
    rowRate <- (row$num/row$den)
    totNum <- sum(subDf()$num, na.rm=TRUE)
    totDen <- sum(subDf()$den, na.rm=TRUE)
    totRate <- totNum/totDen
    # plotData <- data.frame(geography=c(as.character(row$UHFNAME), 'NYC - overall'),
    #                        rate=c(rowRate, totRate))
    plotData <- data.frame(geography=factor(c(as.character(row$UHFNAME),'NYC - overall'),
                                            levels=c(as.character(row$UHFNAME),'NYC - overall')),
                           rate=c(rowRate, totRate),
                           count=c(row$num, totNum))
    plotData$geography = factor(plotData$geography, levels =c( 'NYC - overall', 
                                                               as.character(row$UHFNAME)) )
    ggplot(data=plotData, aes(x=geography, y=rate, label=count)) + 
               geom_bar(stat='identity', aes(fill=geography)) +
               geom_text(aes(label=round(rate, digits=2)), vjust=-0.3, size=4) +
               theme_minimal(base_size = 20) +
               labs(title="UHF Area Asthma Rate vs. NYC Hub Overall Asthma Rate" ) +
               #scale_fill_brewer(palette = "YlOrRd")
               # scale_fill_manual=(values="pal2")
               scale_fill_manual(values = c( "sandybrown", 'maroon'))
    
  })
  
  output$barplot=renderPlotly({
    
    ggplotly(asthmaDf%>%
               filter(name==input$UHFname) %>% 
               group_by(asthma) %>% 
               summarise(num= sum(num), den=sum(den)) %>%
               mutate(rate=num/den) %>%
               ggplot( aes(x=asthma, y=rate, label=num, fill=asthma) ) +
               geom_bar(stat = "identity") +
               #geom_segment( aes(x=asthma ,xend=asthma, y=0, yend=rate), color="grey") +
               #geom_point(size=3, color="#56b3a2") +
              # coord_flip() +
               theme_ipsum() +
               labs(title='Patients with Asthma by Severity Diagnosis Categories') +
               theme(legend.position="none", axis.title.y=element_blank()),tooltip = "y",
                     axis.text.x = element_text(angle = 90, hjust = 1))})# +
              #scale_fill_distiller(palette = "YlOrRd")
    #scale_fill_manual(values = wes.palette("GrandBudapest"))
  
  output$barplot2=renderPlotly({
    
    ggplotly(asthmaDf%>%
               filter(asthma=='Any' & name==input$UHFname) %>%
               group_by(race) %>% 
               summarise(num= sum(num), den=sum(den)) %>%
               mutate(rate=num/den) %>%
               ggplot( aes(x=race, y=rate, label=num, fill=race) ) +
               geom_bar(stat = "identity") +
               #geom_segment( aes(x=race ,xend=race, y=0, yend=rate), color="grey") +
              # geom_point(size=3, color="#69b3a2") +
              # coord_flip() +
               theme_ipsum() +
               labs(title='Patients with Any Asthma by Race') +
               theme(
                 panel.grid.minor.y = element_blank(),
                 panel.grid.major.y = element_blank(),
                 legend.position="none", 
                 axis.title.y=element_blank()), 
                 tooltip = c("y", "label"))}) #+
   # scale_fill_manual(values = wes.palette("GrandBudapest"))
  
  
}

shinyApp(ui, server)
