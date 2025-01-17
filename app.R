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
library(RColorBrewer)

customers <- read.csv("www/Customers.csv")
rides <- read.csv("www/Rides.csv")
vehicles <- read.csv("www/Vehicles.csv")
dict <- read.csv("www/Data Dictionary.csv")
tech_actions <- read.csv("www/Technician Actions.csv") %>%
    mutate(#time_elapsed=as.numeric(difftime(as.POSIXct(technician_action_completed_at_local_time),as.POSIXct(technician_action_created_at_local_time),units="mins")),
           completed_date=as.Date(as.POSIXct(technician_action_completed_at_local_time)))
wo_orders <- read.csv("www/Work Orders.csv")

actionVars <- c(
    'Mean Score','Total Score','Total Questionable Rebalances',
    'Mean Daily Battery Swaps','Mean Daily Maintenances','Mean Daily Rebalances',
    'Total Battery Swaps','Total Maintenances','Total Rebalances'
)

rounder <- 0.002

ui <- dashboardPage(
    dashboardHeader(title="Veo Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            id="tabs",
            menuItem("Map",tabName="map",icon=icon("map")),
            menuItem("Tech Stats",tabName="techStats",icon=icon("list-ol")),
            menuItem("Ride Stats",tabName="rideStats",icon=icon("chart-simple")),
            hr(),
            conditionalPanel(
                condition="input.tabs=='map'",
                sliderInput("hours",label="Hours of Day",min=0,max=24,value=c(0,24),step=1),
                selectInput("variable",label="Variable",choices=c("Mean Daily Revenue","Mean Ride Distance","Mean Daily Battery Swaps","Total Ride Distance","Total Revenue","Total Ride Count")),
                checkboxInput("log",label="Log Scale",value=FALSE)
            ),
            conditionalPanel(
                condition="input.tabs=='rideStats'",
                selectInput("rideVar",label="Variable",choices=c("hour","day of week","day of year","ride distance","ride charge")),
            ),
            conditionalPanel(
                condition="input.tabs=='techStats'",
                checkboxInput("removeSmall",label="Exclude Low Contributors",value=TRUE)
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
                tabName="techStats",
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
                tabName="rideStats",
                fluidRow(
                    box(
                        title="Ride Stats",
                        solidHeader=TRUE,
                        status="primary",
                        width=6,
                        plotOutput("ridePlot") %>% withSpinner()
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
        # rides2 <- rides %>%
        #     filter(ride_distance_in_miles<=100) %>%
        #     filter(between(hour(ride_started_at_local_time),min(input$hours),max(input$hours))) %>%
        #     mutate(start_lon=round(ride_start_longitude/rd())*rd(),
        #            start_lat=round(ride_start_latitude/rd())*rd(),
        #            date=as.Date(ride_started_at_local_time)) %>%
        #     group_by(start_lon,start_lat) %>%
        #     summarise(`Total Ride Count`=as.numeric(n()),
        #               `Total Ride Distance`=sum(ride_distance_in_miles),
        #               `Total Revenue`=sum(ride_charge_in_dollars),
        #               `Mean Daily Revenue`=`Total Revenue`/(as.numeric(difftime(max(as.Date(as.POSIXct(ride_started_at_local_time))),min(as.Date(as.POSIXct(ride_started_at_local_time))),units="days"))+1),
        #               `Mean Ride Distance`=mean(ride_distance_in_miles)) %>%
        #     filter(`Total Ride Count`>=3) %>%
        #     as.data.table() %>%
        #     melt(id.vars=c('start_lat','start_lon')) %>%
        #     mutate(variable=as.character(variable))
        
        rides2 <- rides %>%
            filter(ride_distance_in_miles<=100) %>%
            filter(between(hour(ride_started_at_local_time),min(input$hours),max(input$hours))) %>%
            mutate(start_lon=round(ride_start_longitude/rd())*rd(),
                   start_lat=round(ride_start_latitude/rd())*rd(),
                   date=as.Date(ride_started_at_local_time)) %>%
            group_by(start_lon,start_lat,date) %>%
            summarise(`Total Ride Count`=as.numeric(n()),
                      `Mean Ride Distance`=mean(ride_distance_in_miles),
                      `Total Ride Distance`=sum(ride_distance_in_miles),
                      `Total Revenue`=sum(ride_charge_in_dollars)) %>%
            group_by(start_lon,start_lat) %>%
            summarise(`Mean Daily Revenue`=mean(`Total Revenue`),
                      `Mean Ride Distance`=mean(`Mean Ride Distance`),
                      `Total Ride Count`=sum(`Total Ride Count`),
                      `Total Ride Distance`=sum(`Total Ride Distance`),
                      `Total Revenue`=sum(`Total Revenue`)) %>%
            #filter(`Total Ride Count`>=3) %>%
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
        # rides %>%
        #     mutate(start_lon=round(ride_start_longitude/rd())*rd(),
        #            start_lat=round(ride_start_latitude/rd())*rd()) %>%
        #     rename("lat"=start_lat,"lon"=start_lon) %>%
        #     select(c("lat","lon")) %>%
        #     unique() %>%
        #     mutate(pixelID=row_number())
    })
    
    techScores <- reactive({
        tech_actions %>%
            filter(technician_action_type=="Rebalance") %>%
            mutate(#completion_date=as.Date(as.POSIXct(technician_action_completed_at_local_time)), # not used, for time series
                   start_lon=round(technician_pickup_longitude/rd())*rd(),
                   start_lat=round(technician_pickup_latitude/rd())*rd(),
                   stop_lon=round(technician_dropoff_longitude/rd())*rd(),
                   stop_lat=round(technician_dropoff_latitude/rd())*rd()) %>%
            select(c("start_lon","start_lat","stop_lon","stop_lat","technician_id")) %>%
            left_join(pixels(),by=c("start_lat"="lat","start_lon"="lon")) %>%
            left_join(pixels(),by=c("stop_lat"="lat","stop_lon"="lon")) %>%
            mutate(valueDiff=value.y-value.x) %>%
            group_by(technician_id) %>%
            #filter(n()>=3) %>%
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
            summarise(`Mean Daily`=mean(n),
                      Total=sum(n),
                      daysWorked=n()) %>%
            #filter(daysWorked>=3) %>%
            mutate(daysWorked=NULL) %>%
            as.data.table() %>%
            melt(id.vars=c("technician_id","technician_action_type")) %>%
            mutate(variable=paste0(variable," ",technician_action_type,"s"),
                   technician_action_type=NULL)
        
        scores <- as.data.table(techScores()) %>%
            melt(id.vars=c("technician_id")) %>%
            mutate(variable=as.character(variable))
        
        data <- bind_rows(scores,actions) %>%
            mutate(technician_id=as.character(technician_id),
                   variable=factor(variable,levels=actionVars)) %>%
            arrange(technician_id,variable)
        
        if(input$removeSmall) {
            data <- data %>%
                group_by(technician_id) %>%
                filter(min(value[variable=="Total Rebalances"])>=15) %>%
                ungroup()
        }
        
        return(data)
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
            #leafletOptions(zoomSnap = 0.5) %>%
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
            geom_bar(stat="identity",color="black",width=3/4) +
            facet_wrap(vars(variable),scales="free_y") +
            guides(fill=FALSE) +
            labs(y="Value",
                 x="Technician ID") +
            theme(
                axis.title = element_text(size = 16),  # Axis titles
                axis.text.y = element_text(size = 12),
                axis.text.x = element_text(size = 12, angle=90),
                plot.title = element_text(size = 16, face = "bold"),
                strip.text = element_text(size = 14, face = "bold")
            )
    })
    
    output$pricePlot <- renderPlot({
        rides %>%
            left_join(vehicles,by="vehicle_id") %>%
            filter(ride_distance_in_miles<=100) %>%
            ggplot(aes(x=ride_distance_in_miles,y=ride_charge_in_dollars,color=vehicle_type)) +
            geom_point(alpha=1/4,size=1) +
            geom_smooth(method="lm")
    })
    
    output$ridePlot <- renderPlot({
        plot <- rides %>%
            left_join(vehicles,by="vehicle_id") %>%
            filter(ride_distance_in_miles<=100) %>%
            rename('ride distance'=ride_distance_in_miles,
                   'ride charge'=ride_charge_in_dollars) %>%
            mutate(hour=hour(as.POSIXct(ride_started_at_local_time)),
                   `day of week`=wday(as.POSIXct(ride_started_at_local_time)),
                   `day of year`=as.Date(as.POSIXct(ride_started_at_local_time))) %>%
            ggplot(aes_string(x=paste0("`",input$rideVar,"`"))) +
            geom_histogram(aes(fill=vehicle_type),color="black",binwidth=1)
        
        # if(grepl("ride",input$rideVar)) {
        #     plot <- plot + scale_x_log10()
        # }
        
        return(plot)
    })
    
    output$techTable <- renderRHandsontable({
        techPlotData() %>%
            mutate(variable=gsub(" ","<br>",variable)) %>%
            as.data.table() %>%
            dcast(technician_id~variable,value.var="value") %>%
            rhandsontable(rowHeaders=FALSE,escape=FALSE) %>% 
            hot_cols(columnSorting=TRUE)
    })
    
    output$debug <- renderPrint({
    })
    
}

shinyApp(ui = ui, server = server)
