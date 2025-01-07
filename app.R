library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(data.table)
library(dplyr)
library(lubridate)
library(leaflet)
library(ggplot2)
library(viridis)
library(rhandsontable)

customers <- read.csv("/home/bort/veo/Data/Customers.csv")
rides <- read.csv("/home/bort/veo/Data/Rides.csv")
vehicles <- read.csv("/home/bort/veo/Data/Vehicles.csv")
dict <- read.csv("/home/bort/veo/Data/Data Dictionary.csv")
tech_actions <- read.csv("/home/bort/veo/Data/Technician Actions.csv") %>%
    mutate(time_elapsed=as.numeric(difftime(as.POSIXct(technician_action_completed_at_local_time),as.POSIXct(technician_action_created_at_local_time),units="mins")),
           completed_date=as.Date(as.POSIXct(technician_action_completed_at_local_time)))
wo_orders <- read.csv("/home/bort/veo/Data/Work Orders.csv")

rounder <- 0.002

ui <- dashboardPage(
    dashboardHeader(title="Veo Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            id="tabs",
            menuItem("Map",tabName="map",icon=icon("map")),
            menuItem("Tech Stats",tabName="score",icon=icon("list-ol")),
            menuItem("Ride Stats",tabName="stats",icon=icon("chart-simple")),
            hr(),
            conditionalPanel(
                condition="input.tabs=='map'",
                sliderInput("hours",label="Hours of Day",min=0,max=24,value=c(0,24),step=1),
                selectInput("variable",label="Variable",choices=c("Mean Daily Revenue","Mean Ride Distance","Mean Daily Battery Swaps","Total Ride Distance","Total Revenue","Total Ride Count")),
                checkboxInput("log",label="Log Scale",value=FALSE)
            ),
            conditionalPanel(
                condition="input.tabs=='stats'",
                selectInput("timeVar",label="Variable",choices=c("hour","day of week","day of year","ride distance","ride charge"))
            ),
            #sliderInput("pixelOpacity",label="Pixel Opacity",min=1,max=6,step=1,value=5),
            sliderInput("pixelSize",label="Pixel Size (m°)",min=1,max=10,step=1,value=2)
        )
    ),
    dashboardBody(
        tags$style(
            "#ridesMap {height: calc(85vh) !important;}",
            "#techPlot {height: calc(66vh) !important;}"
        ),
        tabItems(
            tabItem(
                tabName="map",
                leafletOutput("ridesMap") %>% withSpinner()
            ),
            tabItem(
                tabName="score",
                fluidRow(
                    box(
                        width=9,
                        title="Action Counts",
                        solidHeader=TRUE,
                        status="primary",
                        plotOutput("techPlot") %>% withSpinner()
                    ),
                    box(
                        width=9,
                        title="Data",
                        solidHeader=TRUE,
                        status="primary",
                        rHandsontableOutput("techTable")
                    )
                )
            ),
            tabItem(
                tabName="stats",
                fluidRow(
                    box(
                        title="Ride Stats",
                        solidHeader=TRUE,
                        status="primary",
                        width=6,
                        plotOutput("timePlot") %>% withSpinner()
                    ),
                    box(
                        title="Revenue vs Ride Length",
                        solidHeader=TRUE,
                        collapsible=TRUE,
                        collapsed=FALSE,
                        status="primary",
                        width=6,
                        plotOutput("pricePlot") %>% withSpinner()
                    )#,box(verbatimTextOutput("debug"))
                )
            )
        )
    )
)


server <- function(input, output, session) {
    
    rd <- reactive({
        input$pixelSize/1000
    })
    
    rides2 <- reactive({
        rides2 <- rides %>%
            filter(ride_distance_in_miles<=100) %>%
            filter(between(hour(ride_started_at_local_time),min(input$hours),max(input$hours))) %>%
            mutate(start_lon=round(ride_start_longitude/rd())*rd(),
                   start_lat=round(ride_start_latitude/rd())*rd()) %>%
            group_by(start_lon,start_lat) %>%
            summarise(`Total Ride Count`=as.numeric(n()),
                      `Total Ride Distance`=sum(ride_distance_in_miles),
                      `Total Revenue`=sum(ride_charge_in_dollars),
                      `Mean Daily Revenue`=`Total Revenue`/(as.numeric(difftime(max(as.Date(as.POSIXct(ride_started_at_local_time))),min(as.Date(as.POSIXct(ride_started_at_local_time))),units="days"))+1),
                      `Mean Ride Distance`=mean(ride_distance_in_miles)) %>%
            filter(`Total Ride Count`>=3) %>%
            as.data.table() %>%
            melt(id.vars=c('start_lat','start_lon')) %>%
            mutate(variable=as.character(variable))
        
        act2 <- tech_actions %>%
            filter(technician_action_type=="Battery Swap") %>%
            mutate(completion_date=as.Date(as.POSIXct(technician_action_completed_at_local_time)), # not used, for time series
                   start_lon=round(technician_pickup_longitude/rd())*rd(),
                   start_lat=round(technician_pickup_latitude/rd())*rd(),
                   stop_lon=round(technician_dropoff_longitude/rd())*rd(),
                   stop_lat=round(technician_dropoff_latitude/rd())*rd()) %>%
            group_by(completion_date,start_lon,start_lat) %>%
            summarise(totalSwaps=n()) %>%
            group_by(start_lon,start_lat) %>%
            summarise(value=mean(totalSwaps)) %>%
            mutate(variable="Mean Daily Battery Swaps")
        
        return(bind_rows(rides2,act2))
    })
    
    pixels <- reactive({
        rides2() %>%
            rename("lat"=start_lat,"lon"=start_lon) %>%
            filter(variable=="Mean Daily Revenue") %>%
            arrange(desc(value)) %>%
            mutate(pixelID=row_number(),
                   variable=NULL)
    })
    
    techScores <- reactive({
        tech_actions %>%
            filter(technician_action_type=="Rebalance") %>%
            mutate(completion_date=as.Date(as.POSIXct(technician_action_completed_at_local_time)), # not used, for time series
                   start_lon=round(technician_pickup_longitude/rd())*rd(),
                   start_lat=round(technician_pickup_latitude/rd())*rd(),
                   stop_lon=round(technician_dropoff_longitude/rd())*rd(),
                   stop_lat=round(technician_dropoff_latitude/rd())*rd()) %>%
            select(c("start_lon","start_lat","stop_lon","stop_lat","technician_id")) %>%
            left_join(pixels(),by=c("start_lat"="lat","start_lon"="lon")) %>%
            left_join(pixels(),by=c("stop_lat"="lat","stop_lon"="lon")) %>%
            mutate(valueDiff=value.y-value.x) %>%
            group_by(technician_id) %>%
            summarise(`Mean Score`=mean(valueDiff,na.rm=TRUE),
                      `Total Score`=sum(valueDiff,na.rm=TRUE),
                      `Total Questionable Rebalances`=sum(valueDiff<=0,na.rm=TRUE)) %>%
            as.data.frame()
    })
    
    techPlotData <- reactive({
        actions <- tech_actions %>%
            group_by(technician_action_type,technician_id,completed_date) %>%
            summarise(n=n()) %>%
            group_by(technician_action_type,technician_id) %>%
            summarise(nPerDay=mean(n),
                      n=sum(n)) %>%
            filter(n>=10) %>%
            arrange(technician_action_type,desc(nPerDay)) %>%
            as.data.frame() %>%
            mutate(variable=paste0("Mean ",as.character(technician_action_type),"s per Day"),
                   value=nPerDay,
                   nPerDay=NULL,n=NULL,technician_action_type=NULL)
        
        scores <- techScores() %>%
            melt(id.vars=c("technician_id")) %>%
            mutate(variable=as.character(variable))
        
        bind_rows(actions,scores) %>%
            mutate(technician_id=as.character(technician_id))
    })

    output$ridesMap <- renderLeaflet({
        rides2 <- rides2() %>%
            filter(variable==input$variable) 
        
        if(input$log) {
            rides2 <- rides2 %>%
                mutate(fill=colorNumeric(palette = "plasma", domain = range(log(value)))(log(value)))
        } else {
            rides2 <- rides2 %>%
                mutate(fill=colorNumeric(palette = "plasma", domain = range(value))(value))
        }
        
        rides2 %>%
            left_join(pixels()%>%select(-c("value")),by=c("start_lat"="lat","start_lon"="lon")) %>%
            mutate(label=paste0("Pixel ",pixelID,", value:",round(value,1))) %>%
            leaflet() %>%
            addScaleBar() %>%
            setView(lng=mean(rides$ride_start_longitude),lat=mean(rides$ride_start_latitude),zoom=14) %>%
            addProviderTiles('Esri.WorldImagery') %>%
            addProviderTiles('CartoDB.PositronOnlyLabels') %>%
            addRectangles(
                lat1=~start_lat-rd()/2,
                lat2=~start_lat+rd()/2,
                lng1=~start_lon-rd()/2,
                lng2=~start_lon+rd()/2,
                fillColor=~fill,
                fillOpacity=1/2,#input$pixelOpacity/10,
                color='black',
                weight = 0.5,
                label=~label
            )
    })
    
    output$techPlot <- renderPlot({
         techPlotData() %>%
            ggplot(aes(x=technician_id,y=value,fill=technician_id)) +
            geom_bar(stat="identity",color="black") +
            facet_wrap(vars(variable),scales="free_y") +
            guides(fill=FALSE) +
            labs(y="Value",
                 x="Technician ID") +
            theme(
                axis.title = element_text(size = 16),  # Axis titles
                axis.text.y = element_text(size = 14),
                axis.text.x = element_text(size = 14, angle=90),
                plot.title = element_text(size = 20, face = "bold"),
                strip.text = element_text(size = 16, face = "bold")
            )
    })
    
    output$pricePlot <- renderPlot({
        rides %>%
            filter(ride_distance_in_miles<=100) %>%
            ggplot(aes(x=ride_distance_in_miles,y=ride_charge_in_dollars)) +
            geom_point(alpha=1/4,size=1) +
            geom_smooth(method="lm")
    })
    
    output$timePlot <- renderPlot({
        rides %>%
            filter(ride_distance_in_miles<=100) %>%
            rename('ride distance'=ride_distance_in_miles,
                   'ride charge'=ride_charge_in_dollars) %>%
            mutate(hour=hour(as.POSIXct(ride_started_at_local_time)),
                   `day of week`=wday(as.POSIXct(ride_started_at_local_time)),
                   `day of year`=as.Date(as.POSIXct(ride_started_at_local_time))) %>%
            ggplot(aes_string(x=paste0("`",input$timeVar,"`"))) +
            geom_histogram(fill="purple",color="black",binwidth=1)
    })
    
    output$techTable <- renderRHandsontable({
        techPlotData() %>%
            dcast(technician_id~variable,value.var="value") %>%
            rhandsontable(rowHeaders=FALSE) %>% 
            hot_cols(columnSorting=TRUE)
    })
    
    output$debug <- renderPrint({
        #str(rides2())
    })
    
}

shinyApp(ui = ui, server = server)
