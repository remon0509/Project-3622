#Stage 4
library(shiny)
library(leaflet)
library(tidyverse)


#Loading Dataframe

df1<-read.csv('data/accidents_2012_to_2014.csv',header=TRUE)
df2<-read.csv('data/accidents_2009_to_2011.csv',header=TRUE)

#Subetting by year
yr2014<-subset(df1,Year==2014)
yr2013<-subset(df1,Year==2013)
yr2012<-subset(df1,Year==2012)
yr2011<-subset(df2,Year==2011)
yr2010<-subset(df2,Year==2010)
yr2009<-subset(df2,Year==2009)


tdata<-rbind(yr2014,yr2013,yr2012,yr2011,yr2010,yr2009)%>%
    mutate(Date1=as.Date(Date,"%d/%m/%Y"))%>%
    transform(Date=as.Date(Date,"%d/%m/%Y"))%>%
    transform(Accident_Severity=cut(Accident_Severity,3,labels=c("Fatal","Serious","Slight")))%>%
    transform(Urban_or_Rural_Area=cut(Urban_or_Rural_Area,2,labels=c("Urban","Rural")))

tdata$Date1<-format(as.Date(tdata$Date1),"%Y-%m")%>%
    paste(rep(c("-01"),nrow(tdata)),sep='')%>%
    as.Date(format="%Y-%m-%d")

##GeoJSON raw data of UK admin regions##
# library(geojson)
# GeoData<-geojsonio::geojson_read("https://raw.githubusercontent.com/martinjc/UK-GeoJSON/master/json/administrative/gb/lad.json", what = "sp")

monthStart<-function(x){
    x<-as.POSIXlt(x)
    x$mday<-1
    as.Date(x)
}

##################################
###########            ###########
###########     UI     ###########
###########            ###########
##################################

ui <- bootstrapPage(
    
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$head(includeCSS("styles2.css")),
    
    leafletOutput("map", width="100%", height="100%"),
    
    
    absolutePanel(
        id = "controls", class = "panel panel-default",
        top = 120, left = 20, width = 250, fixed=TRUE,
        draggable = TRUE, height = "auto",
        
        tabsetPanel(
            
            
            
            tabPanel("1",
                     
                     
                     sliderInput("date1",label="",
                                 min=as.Date("2009-01-01"),max=as.Date("2014-12-01"),
                                 value=as.Date("2014-12-01"),timeFormat="%b %Y"),
                     
                     selectInput("radius",
                                 label="Size by:", choices=list("Number of Casualties"=TRUE,
                                                                "Number of Vehicles"=FALSE),
                                 selected=TRUE),
                     selectInput("colour",
                                 label="Colour by:", choices=list("Accident Severity"=7,
                                                                  "Weather Conditions"=26,
                                                                  "Light Conditions"=25,
                                                                  "Pedestrian Crossing Facilities"=24,
                                                                  "Road Type"=17,
                                                                  "Road Surface Conditions"=27,
                                                                  "Area"=30),
                                 selected=7)

                     # selectInput("year",
                     #             label="Year", choices=list("2014"=2014,"2013"=2013,
                     #                                        "2012"=2012,"2011"=2011,
                     #                                        "2010"=2010,"2009"=2009),selected=2014),
                     
                     # selectInput("dayofweek",
                     #             label="Day of Week", choices=list("Monday"=1,"Tuesday"=2,
                     #                                               "Wednesday"=3,"Thursday"=4,
                     #                                               "Friday"=5,"Saturday"=6,
                     #                                               "Sunday"=7,"All"=0),selected=0),
                     
                     #Date Input
                     # dateInput("date",
                     #           label="Date Input", value="")
            ),
            
            
            tabPanel("2",
                     
                     
                     #More setting checkbox
                     # checkboxGroupInput("condition",
                     #                    label="",
                     #                    choices=list("More Options"='yes')),
                     # #Condtional Panel
                     # conditionalPanel(condition="input.condition=='yes'",
                     
                     #Slider for Number_of_Casualities
                     sliderInput("casualty",
                                 label="Number of Casualities",
                                 min=1, max=93, value=c(1,93)),
                     
                     #Slider for Number_of_Vehicles
                     sliderInput("vehicle",
                                 label="Number of Vehicles",
                                 min=1, max=67, value=c(1,67)),
                     
                     #Slider for Speed_limit
                     sliderInput("speed",
                                 label="Speed Limit (mph)",
                                 min=10, max=70, value=c(10,70),
                                 step=10),
                     
                     #Checkbox group for Pedestrian_Crossing.Physical_Facilities
                     checkboxGroupInput("facility", 
                                        label="Pedestrian Crossing Facilities", 
                                        choices=list("Central refuge"="Central refuge",
                                                     "Footbridge or subway"="Footbridge or subway",
                                                     "No physical crossing within 50 meters"="No physical crossing within 50 meters",
                                                     "non-junction pedestrian crossing"="non-junction pedestrian crossing",
                                                     "Pedestrian phase at traffic signal junction"="Pedestrian phase at traffic signal junction",
                                                     "Zebra crossing"="Zebra crossing"),
                                        selected=c("Central refuge",
                                                   "Footbridge or subway",
                                                   "No physical crossing within 50 meters",
                                                   "non-junction pedestrian crossing",
                                                   "Pedestrian phase at traffic signal junction",
                                                   "Zebra crossing")),
                     
                     #Checkbox group for Light_Conditions
                     checkboxGroupInput("light", 
                                        label="Light Condition", 
                                        choices=list("Darkeness: No street lighting"="Darkeness: No street lighting",
                                                     "Darkness: Street lighting unknown"="Darkness: Street lighting unknown",
                                                     "Darkness: Street lights present and lit"="Darkness: Street lights present and lit",
                                                     "Darkness: Street lights present but unlit"="Darkness: Street lights present but unlit",
                                                     "Daylight: Street light present"="Daylight: Street light present"),
                                        selected=c("Darkeness: No street lighting",
                                                   "Darkness: Street lighting unknown",
                                                   "Darkness: Street lights present and lit",
                                                   "Darkness: Street lights present but unlit",
                                                   "Daylight: Street light present"))
                     
                     # #Checkbox group for Accident_Severity
                     # checkboxGroupInput("severity", 
                     #                    label="Accident Severity", 
                     #                    choices=list("Fatal"=1, "Serious"=2, "Slight"=3),
                     #                    selected=c(1,2,3)),
                     
                     #Slider for Accident_Severity
                     # sliderInput("severity",
                     #             label="Accident Severity",
                     #             min=1, max=3, value=3,step=1),
                     # helpText("Level 1: Fatal,",
                     #          "Level 2: Serious,",
                     #          "Level 3: Slight")
            ),
                     
                     
            tabPanel("3",
                     
                     
                     #Checkbox group for Road Type
                     checkboxGroupInput("roadtype", 
                                        label="Road_Type", 
                                        choices=list("Dual carriageway"="Dual carriageway",
                                                     "One way street"="One way street",
                                                     "Roundabout"="Roundabout",
                                                     "Single carriageway"="Single carriageway",
                                                     "Slip road"="Slip road",
                                                     "Unknown"="Unknown"),
                                        selected=c("Dual carriageway",
                                                   "One way street",
                                                   "Roundabout",
                                                   "Single carriageway",
                                                   "Slip road",
                                                   "Unknown")),
                     
                     
                     
                     #Checkbox group for Road_Surface_Conditions
                     checkboxGroupInput("surface", 
                                        label="Road Surface Conditions", 
                                        choices=list("Dry"="Dry",
                                                     "Flood"="Flood (Over 3cm of water)",
                                                     "Frost/Ice"="Frost/Ice",
                                                     "Snow"="Snow",
                                                     "Wet/Damp"="Wet/Damp"),
                                        selected=c("Dry",
                                                   "Flood (Over 3cm of water)",
                                                   "Frost/Ice",
                                                   "Snow",
                                                   "Wet/Damp")),
                     
                     #Checkbox group for Weather_Conditions
                     checkboxGroupInput("weather", 
                                        label="Weather Conditions", 
                                        choices=list("Fine with high winds"="Fine with high winds",
                                                     "Fine without high winds"="Fine without high winds",
                                                     "Fog or mist"="Fog or mist",
                                                     "Raining with high winds"="Raining with high winds",
                                                     "Raining without high winds"="Raining without high winds",
                                                     "Snowing with high winds"="Snowing with high winds",
                                                     "Snowing without high winds"="Snowing without high winds"),
                                        selected=c("Fine with high winds",
                                                   "Fine without high winds",
                                                   "Fog or mist",
                                                   "Raining with high winds",
                                                   "Raining without high winds",
                                                   "Snowing with high winds",
                                                   "Snowing without high winds")),
                     
                     
                     
                     #Checkbox group for Urban_or_Rural_Area
                     checkboxGroupInput("urban", 
                                        label="Urban/Rural Area", 
                                        choices=list("Urban"="Urban",
                                                     "Rural"="Rural"),
                                        selected=c("Urban",
                                                   "Rural"))
            )))
    
    
    
    
    
)



##################################
###########            ###########
###########   Server   ###########
###########            ###########
##################################


server <- function(input, output) {
    
    sliderMonth<-reactiveValues()
    observe({
        full.date<-as.POSIXct(input$date1, tz="GMT")
        sliderMonth$Month<-as.character(monthStart(full.date))
    })
    
    dataFilter<-reactive({
        rtdata<-filter(tdata,Date1==sliderMonth$Month)%>%
            #Number_of_Casualties
            filter(Number_of_Casualties>=input$casualty[1] & Number_of_Casualties<=input$casualty[2])%>%
            #Number_of_Vehicles
            filter(Number_of_Vehicles>=input$vehicle[1] & Number_of_Vehicles<=input$vehicle[2])%>%
            #Speed_limit
            filter(Speed_limit>=input$speed[1] & Speed_limit<=input$speed[2])%>%
            #Accident_Severity
            # filter(Accident_Severity==input$severity)%>%
            #Road_Type
            filter(Road_Type%in%input$roadtype)%>%
            #Light_Conditions
            filter(Light_Conditions%in%input$light)%>%
            #Road_Surface_Conditions
            filter(Road_Surface_Conditions%in%input$surface)%>%
            #Weather_Conditions
            filter(Weather_Conditions%in%input$weather)%>%
            #Pedestrian_Crossing.Physical_Facilities
            filter(Pedestrian_Crossing.Physical_Facilities%in%input$facility)%>%
            #Urban_or_Rural_Area
            filter(Urban_or_Rural_Area%in%input$urban)
    })
    
    
    radius<-reactiveValues()
    observe({
        ifelse(input$radius==TRUE,
               radius$gamma<-dataFilter()$Number_of_Casualties,
               radius$gamma<-dataFilter()$Number_of_Vehicles)
        
    })
    
    circlecol<-reactiveValues()
    observe({
        #Accident_Severity
        if(input$colour==7){
            circlecol$alpha<-c("red","orange","yellow")
            circlecol$beta<-c("Fatal","Serious","Slight")
        }
        #Weather_Conditions
        else if(input$colour==26){
            circlecol$alpha<-c("red","orange","yellow",
                               "green","blue","purple",
                               "black")
            circlecol$beta<-c("Fine with high winds",
                              "Fine without high winds",
                              "Fog or mist",
                              "Raining with high winds",
                              "Raining without high winds",
                              "Snowing with high winds",
                              "Snowing without high winds")
        }
        #Light_Conditions
        else if(input$colour==25){
            circlecol$alpha<-c("red","yellow","green",
                               "blue","purple")
            circlecol$beta<-c("Darkeness: No street lighting",
                              "Darkness: Street lighting unknown",
                              "Darkness: Street lights present and lit",
                              "Darkness: Street lights present but unlit",
                              "Daylight: Street light present")
        }
        #Pedestrian_Crossing.Physical_Facilities
        else if(input$colour==24){
            circlecol$alpha<-c("red","yellow","orange",
                               "navy","purple","black")
            circlecol$beta<-c("Central refuge",
                              "Footbridge or subway",
                              "No physical crossing within 50 meters",
                              "non-junction pedestrian crossing",
                              "Pedestrian phase at traffic signal junction",
                              "Zebra crossing")
        }#Road_Type
        else if(input$colour==17){
            circlecol$alpha<-c("yellow","navy","green",
                               "purple","red","black")
            circlecol$beta<-c("Dual carriageway",
                              "One way street",
                              "Roundabout",
                              "Single carriageway",
                              "Slip road",
                              "Unknown")
        }
        #Road_Surface_Conditions
        else if(input$colour==27){
            circlecol$alpha<-c("yellow","black","green",
                               "navy","purple")
            circlecol$beta<-c("Dry",
                              "Flood (Over 3cm of water)",
                              "Frost/Ice",
                              "Snow",
                              "Wet/Damp")
        }
        #Urban_or_Rural_Area
        else if(input$colour==30){
            circlecol$alpha<-c("navy","red")
            circlecol$beta<-c("Urban","Rural")
        }
        
    })
    
    
    
    
    
    
    
    
    output$map<-renderLeaflet({
        
        
        
        leaflet(tdata)%>%
            addProviderTiles("CartoDB.Positron")%>%
            setView(-3,54.3,zoom=5.5)
        
        
        # addCircleMarkers(stroke=FALSE,
        #                  fillOpacity = 0.15,
        #                  #color =~pal(Accident_Severity),
        #                  radius=0.03) 
        
    })
    
    observe({
        
        pal<-colorFactor(palette=circlecol$alpha, domain=circlecol$beta)
        
        
        
        leafletProxy("map",data=dataFilter())%>%
            clearMarkers()%>%
            clearMarkerClusters()%>%
            clearControls()%>%
            # addCircleMarkers(stroke=FALSE,
            #                  fillOpacity = 0.15,
            #                          color =~pal(Accident_Severity),
            #                          radius=0.8)
            addCircleMarkers(stroke=FALSE, 
                             fillOpacity=0.4,
                             color =~pal(dataFilter()[,as.numeric(input$colour)]),
                             radius=(log(radius$gamma)+1)*2)%>%
            # color=~pal(rtdata$Accident_Severity),
            # clusterOptions=markerClusterOptions())
            addLegend("bottomright",
                      pal=pal,
                      values=~dataFilter()[,as.numeric(input$colour)],
                      opacity=0.6,
                      title="")
    })
    
}

shinyApp(ui = ui, server = server)

