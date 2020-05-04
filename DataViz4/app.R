#Stage 4
library(shiny)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(plotly)
library(forecast)
library(treemapify)

#Loading Dataframe

df1<-read.csv('data/accidents_2012_to_2014.csv',header=TRUE)
df2<-read.csv('data/accidents_2009_to_2011.csv',header=TRUE)
city<-read.csv('data/ukCity.csv',header=TRUE)



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
    transform(Urban_or_Rural_Area=cut(Urban_or_Rural_Area,2,labels=c("Urban","Rural")))%>%
    mutate(Hour=floor(as.numeric(gsub(":","",Time))/100))%>%
    mutate(Hour_Range=cut(Hour+1,breaks=c(0,3,6,9,12,15,18,21,24),
                          labels=c("Midnight to 2:59 a.m.",
                                   "3 a.m. to 5:59 a.m.",
                                   "6 a.m. to 8:59 a.m.",
                                   "9 a.m. to 11:59 a.m.",
                                   "Noon to 2:59 p.m.",
                                   "3 p.m. to 5:59 p.m.",
                                   "6 p.m. to 8:59 p.m.",
                                   "9 p.m. to 11:59 p.m."),
                          include.lowest=TRUE))

tdata$Date1<-format(as.Date(tdata$Date1),"%Y-%m")%>%
    paste(rep(c("-01"),nrow(tdata)),sep='')%>%
    as.Date(format="%Y-%m-%d")


tmp<-tdata[,c("Hour_Range","Day_of_Week","Accident_Severity")]
tmp$Day_of_Week<-cut(tmp$Day_of_Week,breaks=c(0,1,2,3,4,5,6,7),
                     labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

tmp1<-filter(tmp,Accident_Severity==1)
tmp1<-aggregate(x=tmp1$Accident_Severity,
                by=tmp1[c("Hour_Range","Day_of_Week")],FUN=sum)
tmp2<-filter(tmp,Accident_Severity==2)
tmp2<-aggregate(x=(tmp2$Accident_Severity)/2,
                by=tmp2[c("Hour_Range","Day_of_Week")],FUN=sum)
tmp3<-filter(tmp,Accident_Severity==3)
tmp3<-aggregate(x=(tmp3$Accident_Severity)/3,
                by=tmp3[c("Hour_Range","Day_of_Week")],FUN=sum)

colnames(tmp1)<-c("Hour_Range" ,"Day_of_Week","Fatal")
colnames(tmp2)<-c("Hour_Range" ,"Day_of_Week","Serious")
colnames(tmp3)<-c("Hour_Range" ,"Day_of_Week","Slight")

tmpX<-merge(tmp1,tmp2)
tmpX<-merge(tmpX,tmp3)%>%
    mutate(FStoS_Ratio=(Fatal+Serious)/Slight)%>%
    mutate(Fatal_Rate=Fatal/(Fatal+Serious+Slight))

########
########

tmpfun<-function(num_col){
    tmp<-tdata[,c(num_col,7)]
    tmp1<-filter(tmp,Accident_Severity==1)
    tmp1<-aggregate(x=tmp1$Accident_Severity,
                    by=tmp1[c(names(tdata)[num_col])],FUN=sum)
    tmp2<-filter(tmp,Accident_Severity==2)
    tmp2<-aggregate(x=(tmp2$Accident_Severity)/2,
                    by=tmp2[c(names(tdata)[num_col])],FUN=sum)
    tmp3<-filter(tmp,Accident_Severity==3)
    tmp3<-aggregate(x=(tmp3$Accident_Severity)/3,
                    by=tmp3[c(names(tdata)[num_col])],FUN=sum)
    
    colnames(tmp1)<-c(names(tdata)[num_col],"Fatal")
    colnames(tmp2)<-c(names(tdata)[num_col],"Serious")
    colnames(tmp3)<-c(names(tdata)[num_col],"Slight")
    
    tmpX<-merge(tmp1,tmp2)
    tmpX<-merge(tmpX,tmp3)%>%
        mutate(FStoS_Ratio=(Fatal+Serious)/Slight)%>%
        mutate(Fatal_Rate=Fatal/(Fatal+Serious+Slight))
    
    return(tmpX)
}


tmpASRT<-tmpfun(17)
tmpASPCF<-tmpfun(24)
tmpASLC<-tmpfun(25)
tmpASWC<-tmpfun(26)
tmpASRSC<-tmpfun(27)
tmpASUR<-tmpfun(30)
noplot<-tmpfun(7)

######
tdata<-transform(tdata,Accident_Severity=cut(Accident_Severity,3,labels=c("Fatal","Serious","Slight")))
######


#hist 1
tmphist1<-tdata[,c("Day_of_Week","Hour")]

hist1<-ggplot(data=tmphist1,aes(x=Day_of_Week))+ 
    geom_histogram(binwidth=1,alpha=0.3,color="red",fill="red")+
    scale_x_continuous(name=c("Day of Week"),
                       breaks=c(1,2,3,4,5,6,7),
                       labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
    theme(axis.text.x=element_text(angle=45, hjust=1,size=8))

hist2<-ggplot(data=tmphist1,aes(x=Hour))+ 
    geom_histogram(binwidth=1,alpha=0.3,color="blue",fill="blue")+
    scale_x_continuous(name=c("Time"),
                       breaks=seq(0:23)-1,
                       labels=c("12 a.m.","1 a.m.","2 a.m.","3 a.m.","4 a.m.","5 a.m.",
                                "6 a.m.","7 a.m.","8 a.m.","9 a.m.","10 a.m.","11 a.m.",
                                "12 p.m.","1 p.m.","2 p.m.","3 p.m.","4 p.m.","5 p.m.",
                                "6 p.m.","7 p.m.","8 p.m.","9 p.m.","10 p.m.","11 p.m."))+
    theme(axis.text.x=element_text(angle=45, hjust=1,size=8))


#Holt Winter Mul Seasonal Forecast
uk<-tdata[,c(33,34)]%>%
    mutate(count=rep(1,nrow(tdata)))
uk<-aggregate(x=uk$count,
              by=uk[c("Year","Date1")],FUN=sum)
colnames(uk)<-c("Year","Date","Count")


ts.uk<-ts(data=uk[,"Count"],frequency=12,start=c(2009,1))

fit<-hw(y=ts.uk,seasonal="mul",h=12,initial="simple")
r2<-1-fit$model$SSE/sum((fit$x-mean(fit$x))^2)

fit_for<-forecast(fit, h=12)
dates<-seq(uk$Date[72], by=uk$Date[72]-uk$Date[71], len=12)




icons<-awesomeIcons(
    icon='pin',
    iconColor="red",
    library="ion",
    markerColor="white"
)

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
        top=5, right=10, width=550, fixed=TRUE,
        draggable=TRUE, height="auto",
        
        tabsetPanel(
            tabPanel("Plot",
                     plotOutput("tree", height="300px", width="100%"),
                     plotlyOutput("bar", height="400px", width="100%")
                     ),
            
            tabPanel("Forecast",
                     plotlyOutput("forecast", height='400px', width="100%")
                     ),
            
            tabPanel("More",
                     tabsetPanel(
                         tabPanel("Total",
                             h5(textOutput("from"), align = "center"),
                             plotlyOutput("hist1", height='350px', width="100%"),
                             plotlyOutput("hist2", height='350px', width="100%")
                             ),
                         tabPanel("More",
                             plotlyOutput("hist3", height='350px', width="100%"),
                             plotlyOutput("hist4", height='350px', width="100%")
                             )
                     ) 
                   ))
            ),
    

    
    
    absolutePanel(
        id = "controls", class = "panel panel-default",
        top = 100, left = 20, width = 380, fixed=TRUE,
        draggable = TRUE, height = "auto",
        
        tabsetPanel(
            
            
            
            tabPanel("Main",
                     h3(textOutput("title"), align = "left"),
                     
                     sliderInput("date1",label="",
                                 min=as.Date("2009-01-01"),max=as.Date("2014-12-01"),
                                 value=as.Date("2014-12-01"),timeFormat="%b %Y",
                                 animate=animationOptions(interval=50,loop=TRUE)),
                     
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
                                 selected=17),
                     h4(textOutput("num_accidents"), align = "left")

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
            
            
            tabPanel("More",
                     
                     
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
                     
                     
            tabPanel("More",
                     
                     
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
            addAwesomeMarkers(data=city,icon=icons,label=as.character(city$City))%>%
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
    
    output$title<-renderText({
        paste0( "UK Traffic Accidents")
    })
    
    output$num_accidents<-renderText({
        paste0( "Number of Accidents: ",prettyNum(length(dataFilter()$Longitude), big.mark=","))
    })
    
    
    output$tree<-renderPlot(
        ggplot(tmpX, aes(area=FStoS_Ratio, fill=Hour_Range,label=Day_of_Week,
                         subgroup=Hour_Range))+
            geom_treemap()+
            geom_treemap_subgroup_border()+
            geom_treemap_subgroup_text(place="bottom", grow=T, alpha=1,colour="white", min.size = 0)+
            geom_treemap_text(colour="black", place="topleft", reflow=T)+
            ggtitle("Fatal and Serious to Slight Accidents Ratio by Time of the Day")
    )
    

    index<-reactiveValues()
    
    observe({
        if(input$colour==17){
            index$df<-tmpASRT
        }
        else if(input$colour==24){
            index$df<-tmpASPCF
        }
        else if(input$colour==25){
            index$df<-tmpASLC
        }
        else if(input$colour==26){
            index$df<-tmpASWC
        }
        else if(input$colour==27){
            index$df<-tmpASRSC
        }
        else if(input$colour==30){
            index$df<-tmpASUR
        }
        else if(input$colour==7){
            index$df<-noplot
        }
    })
    
    output$bar<-renderPlotly({
        plot_ly(data=index$df, x=index$df[,1], y=~Fatal,type='bar', name='Fatal')%>%
            add_trace(y = ~Serious, name = 'Serious')%>%
            add_trace(y = ~Slight, name = 'Slight')%>%
            layout(yaxis=list(title='Count'), barmode='group')
    })
    
    output$from<-renderText({
        paste0( "From 2009 to 2016")
    })
    
    output$hist1<-renderPlotly({
        ggplotly(hist1)
    })
    
    output$hist2<-renderPlotly({
        ggplotly(hist2)
    })
    
    
    hist<-reactiveValues()
    
    observe({
        tmphist2<-dataFilter()[,c("Day_of_Week","Hour")]
        
        hist$plot2_1<-ggplot(data=tmphist2,aes(x=Day_of_Week))+ 
            geom_histogram(binwidth=1,alpha=0.3,color="purple",fill="purple")+
            scale_x_continuous(name=c("Day of Week"),
                               breaks=c(1,2,3,4,5,6,7),
                               labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
            theme(axis.text.x=element_text(angle=45, hjust=1,size=8))
        
        hist$plot2_2<-ggplot(data=tmphist2,aes(x=Hour))+ 
            geom_histogram(binwidth=1,alpha=0.3,color="orange",fill="orange")+
            scale_x_continuous(name=c("Time"),
                               breaks=seq(0:23)-1,
                               labels=c("12 a.m.","1 a.m.","2 a.m.","3 a.m.","4 a.m.","5 a.m.",
                                        "6 a.m.","7 a.m.","8 a.m.","9 a.m.","10 a.m.","11 a.m.",
                                        "12 p.m.","1 p.m.","2 p.m.","3 p.m.","4 p.m.","5 p.m.",
                                        "6 p.m.","7 p.m.","8 p.m.","9 p.m.","10 p.m.","11 p.m."))+
            theme(axis.text.x=element_text(angle=45, hjust=1,size=8))
    })
    
    
    output$hist3<-renderPlotly({
        ggplotly(hist$plot2_1)
    })
    
    output$hist4<-renderPlotly({
        ggplotly(hist$plot2_2)
    })
    
    
    
    output$forecast<-renderPlotly({
        plot_ly() %>%
            add_lines(x=uk$Date, y=uk$Count,
                      color=I("black"),
                      name="observed",
                      marker=list(mode="lines"))%>%
            add_lines(x=dates, y=fit_for$mean, color = I("Blue"), name = "prediction") %>%
            add_ribbons(x = dates,
                        ymin = fit_for$lower[, 2],
                        ymax = fit_for$upper[, 2],
                        color = I("gray95"),
                        name = "95% confidence") %>%
            add_ribbons(p,
                        x = dates,
                        ymin = fit_for$lower[, 1],
                        ymax = fit_for$upper[, 1],
                        color = I("gray80"), name = "80% confidence")
    })
    
}

shinyApp(ui = ui, server = server)

