#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Written by: ajayaprakash@karenclarkandco.com

# Loading in the libraries
library(shiny)
library(vroom)
library(dplyr)
library(ggplot2)
library(sf)

# Loading source file of required functions
source("appFuncLib.R")

# Arranging the UI objects for user
ui <- fluidPage(
  tabsetPanel(
    tabPanel("ELT Explorer",
             fileInput("upload", "Upload the ELT in RiskInsight Output Format", accept = c(".csv", ".tsv")),
             fluidRow(
               column(12,
                      dataTableOutput("ELT"))
             ),
             fluidRow(
               column(4,
                      tableOutput("EP")),
               column(8,
                      plotOutput("plot", width = "400px"))
             ),
             fluidRow(
               column(4,
                      numericInput("year", "Return Period", value = 100, min = 10, step = 1),
                      numericInput("n", "Rows", value = 5, min = 1, step = 1)),
               column(8,
                      tableOutput("EventSample"))
             )
             ),
    tabPanel("Event Plotter",
             fileInput("shpfile", "upload all files associated with the shapefile file",
                       multiple = TRUE,
                       accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
             fluidRow(
               column(12,
                      textInput("fp_name", "Footprint Filename:"),
                      textInput("fp_pathname", "Footprint Path:", value = "//kcc-mdstore01/Public/EQ_Models/US_EQ_v2.1_with_FFE_MPM/SHAK/STOC"))
               ),
             fluidRow(
               column(4,
               numericInput("lon_lower_lim", "Longitude Limit (West)", value = -123, min = -180, max = 180),
               numericInput("lon_upper_lim", "Longitude Limit (East)", value = -75, min = -180, max = 180),
               numericInput("lat_upper_lim", "Latitude Limit (North)", value = 50, min = -90, max = 90),
               numericInput("lat_lower_lim", "Latitude Limit (South)", value = 25, min = -90, max = 90)),
               column(8,
                      plotOutput("EQ_footprint", width = "500px"))
             )
             )
  )
)

# Back end calculations performed here
server <- function(input, output, session) {
  
  options(scipen = 999)
  
  # Reading the ELT file as a .csv or .tsv
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  # Output the ELT
  output$ELT <- renderDataTable(data(), options = list(pageLength = 5))
  
  # Calculating the EP curve
  EP_curve <- reactive({
    RP_values <- c(1000,500,250,100,50,20,10)
    ELT_mut <- data() %>%
      arrange(desc(Loss_GU)) %>%
      mutate(cumul = cumsum(EventRate),
             RP = 1/cumul)
    EP1 <- as_tibble(approx(ELT_mut$RP, ELT_mut$Loss_GU, xout = RP_values)) %>%
      rename(Return.Period = x,
             Loss = y)
  })
  
  EP_long <- reactive({
    ELT_mut <- data() %>%
      arrange(desc(Loss_GU)) %>%
      mutate(cumul = cumsum(EventRate),
             RP = 1/cumul)
    EP2 <- tibble(Return.Period = ELT_mut$RP, 
                     Loss = ELT_mut$Loss_GU,
                     TIV = ELT_mut$TIV,
                     EventName = ELT_mut$EventName) 
  })
  
  # Calculating the AAL
  AAL <- reactive({
    data() %>%
      mutate(AAL_event = EventRate*Loss_GU) %>%
      summarise(AAL = sum(AAL_event))%>%
      pull()
  })
  
  # Tabulating EP Curve
  EP_table <- reactive({
    EP_curve() %>%
      mutate(GU_Loss = prettyNum(Loss, big.mark=",",scientific=FALSE),
             `Return Period` = as.character(Return.Period)) %>%
      dplyr::select(`Return Period`, GU_Loss) %>%
      add_row(`Return Period` = "AAL",
              GU_Loss = prettyNum(AAL(), big.mark=",",scientific=FALSE))
  })
  
  # Plotting the EP curve
  EP_plot <- reactive({
    ggplot(EP_long()) +
      geom_point(aes(x = Return.Period, y = Loss/1000000), size = 3) +
      geom_path(aes(x = Return.Period, y = Loss/1000000)) +
      theme_bw() +
      scale_y_continuous("USD (millions)") +
      scale_x_continuous("Return Period")
  })
  
  # Sampling a few (number chosen by user) events around the Loss Return Period chosen by user
  Event_sample <- reactive({
    year <- input$year
    loss <- EP_curve()$Loss[EP_curve()$Return.Period == year]
    ELT_neighborhood <- EP_long() %>%
      filter(Loss > loss*0.95 & Loss < loss*1.05) %>%
      sample_n(size = input$n) %>%
      mutate(TIV = prettyNum(TIV, big.mark=",",scientific=FALSE),
             Loss = prettyNum(Loss, big.mark=",",scientific=FALSE))
  })
  
  # Outputting the EP table and Plot
  output$EP <- renderTable(EP_table())
  
  output$plot <- renderPlot(EP_plot())
  
  # Outputting Sampled Events
  output$EventSample <- renderTable(Event_sample())
  
  # Plotting Events
  shapefile <- reactive({
    req(input$shpfile)
    # shpdf is a data.frame with the name, size, type and datapath
    # of the uploaded files
    shpdf <- input$shpfile
    
    # The files are uploaded with names
    # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # (path/names are in column datapath)
    # We need to rename the files with the actual names:
    # fe_2007_39_county.dbf, etc.
    # (these are in column name)
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    
    map <- st_read(paste(tempdirname,
                               shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                               sep = "/"))
  })
  
  # Reading EQ Footprint
  EQ_footprint <- reactive({
    EQ_footprint <- read_EQfootprint(input$fp_name, input$fp_pathname, old = TRUE)
  })
  
  # Plotting EQ footprint
  longitude_limit <- reactive(c(input$lon_lower_lim, input$lon_upper_lim))
  latitude_limit <- reactive(c(input$lat_lower_lim, input$lat_upper_lim))
  
  EQ_footprint_plot <- reactive({
    plot_EQfootprint(EQ_footprint(), lon_limit = longitude_limit(), lat_limit = latitude_limit())
  })
  
  output$EQ_footprint <- renderPlot(EQ_footprint_plot())

}

# Run the application 
shinyApp(ui = ui, server = server)
