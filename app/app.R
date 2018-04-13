#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Interactive link view 
library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("Map",
        div(class="outer",
           tags$head(
             includeCSS("styles.css")
           ),
           conditionalPanel(condition="input.analysis.indexOf('SP') > -1",
            conditionalPanel(condition="input.sectors_SP.indexOf('Legal') > -1",                
              leafletOutput("SPPlot_legal", width="100%",height="1000px")
            ),
            conditionalPanel(condition="input.sectors_SP.indexOf('Bank') > -1", 
              leafletOutput("SPPlot_bank", width="100%",height="1000px")
            ),
            conditionalPanel(condition="input.sectors_SP.indexOf('Consultancy') > -1", 
                             leafletOutput("SPPlot_con", width="100%",height="1000px")
            ),
            conditionalPanel(condition="input.sectors_SP.indexOf('Accountancy') > -1", 
                             leafletOutput("SPPlot_acct", width="100%",height="1000px")
            ),
            conditionalPanel(condition="input.sectors_SP.indexOf('Architectural') > -1", 
                             leafletOutput("SPPlot_arch", width="100%",height="1000px")
            )
           ),
           conditionalPanel(condition="input.analysis.indexOf('Kernel') > -1",
                            leafletOutput("distPlot", width="100%",height="1000px")
           ),
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                       width = 330, height = "auto",
                       
                       h2("REMGIS"),
                fileInput("file1", "Choose CSV File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
                ), 
                selectInput(inputId="analysis",
                            label="Analysis:",
                            choices=c("Spatial Point Analysis" = "SP",
                                      "Kernel Density Analysis" = "Kernel"),
                            selected = "SP"),
             conditionalPanel(condition="input.analysis.indexOf('Kernel') > -1",
                selectInput(inputId="sector",
                            label="Sector:",
                            choices=c("Legal" = "Legal",
                                      "Banking" = "Bank",
                                      "Consultancy" = "Consultancy",
                                      "Accounting" = "Accountancy",
                                      "Architectural" = "Architectural"),
                            selected = "Legal"),
                sliderInput("distance",
                           "Distance value:",
                           min = 100,
                           max = 1000,
                           value = 280)
              ),
             conditionalPanel(condition="input.analysis.indexOf('SP') > -1",
                              selectInput(inputId="sectors_SP",
                                          label="Sector:",
                                          choices=c("Legal" = "Legal",
                                                    "Banking" = "Bank",
                                                    "Consultancy" = "Consultancy",
                                                    "Accounting" = "Accountancy",
                                                    "Architectural" = "Architectural"),
                                          selected = "Legal")
             )
           ) 
        ) 
      ),
      tabPanel("Kernel Density Map Comparison",
               div(class="outer",
                   tags$head(
                     includeCSS("styles.css")
                   )
               ),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             h2("REMGIS"),  
                             checkboxGroupInput("industries", "Show",
                                                choices = c(
                                                  "Legal" = "Legal",
                                                  "Banking" = "Bank",
                                                  "Consultancy" = "Consultancy",
                                                  "Accounting" = "Accountancy",
                                                  "Architectural" = "Architectural"
                                                )
                             ),
                             sliderInput("distance1",
                                         "Distance value:",
                                         min = 100,
                                         max = 1000,
                                         value = 280)
               ),
               fluidRow(
                 conditionalPanel(condition="input.industries.indexOf('Legal') > -1",
                 column(width = 6, class = "well",
                        h4("Legal kernel Density"),
                        leafletOutput("plotLegal")
                 )), 
                 conditionalPanel(condition="input.industries.indexOf('Bank') > -1",
                 column(width = 6, class="well",
                        h4("Banks Kernel Density"),
                        leafletOutput("plotBank")
                 )),  
                 conditionalPanel(condition="input.industries.indexOf('Consultancy') > -1",
                 column(width = 6, class="well",
                        h4("Consultancy Kernal Density"),
                        leafletOutput("plotConsultancy")
                 )),    
                 conditionalPanel(condition="input.industries.indexOf('Accountancy') > -1",
                 column(width = 6, class="well",
                        h4("Accountancy Kernel Density"),
                        leafletOutput("plotAccountancy")
                 ))
               ,
                 conditionalPanel(condition="input.industries.indexOf('Architectural') > -1",
                 column(width = 6, class="well",
                        h4("Architectural Kernal Density"),
                        leafletOutput("plotArchitectural")
                 ))      
               )
      ),
      tabPanel("Location Quotient",
               div(class="outer",
                   tags$head(
                     includeCSS("styles.css")
                   )),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             h2("REMGIS")
                             ,checkboxGroupInput("lq_industry", "Show",
                                                 choices = c(
                                                   "Legal" = "Legal",
                                                   "Banking" = "Bank",
                                                   "Consultancy" = "Consultancy",
                                                   "Accounting" = "Accountancy",
                                                   "Architectural" = "Architectural"
                                                 )
                             )
               ),fluidRow(
                 column(width = 12, class="well",
                        uiOutput("analysis")
                 ),
                 conditionalPanel(condition="input.lq_industry.indexOf('Legal') > -1",
                                  column(width = 12, class = "well",
                                         h4("Legal Firms Location Quotient Analysis"),
                                         uiOutput("lqLegal")
                                  )),
                 conditionalPanel(condition="input.lq_industry.indexOf('Bank') > -1",
                                  column(width = 12, class = "well",
                                         h4("Banking Firms Location Quotient Analysis"),
                                         uiOutput("lqBank")
                                  )),
                 conditionalPanel(condition="input.lq_industry.indexOf('Consultancy') > -1",
                                  column(width = 12, class = "well",
                                         h4("Consultancy Firms Location Quotient Analysis"),
                                         uiOutput("lqConsultancy")
                                  )),
                 conditionalPanel(condition="input.lq_industry.indexOf('Accountancy') > -1",
                                  column(width = 12, class = "well",
                                         h4("Accountancy Firms Location Quotient Analysis"),
                                         uiOutput("lqAccountancy")
                                  )),
                 conditionalPanel(condition="input.lq_industry.indexOf('Architectural') > -1",
                                  column(width = 12, class = "well",
                                         h4("Architectural Firms Location Quotient Analysis"),
                                         uiOutput("lqArchitectural")
                                  )),
                 column(width = 12, class="well",
                        plotOutput("LQPlot")
                 ),
                 column(width = 12, class="well",
                        plotOutput("LQPlotJurong")
                 )
               )
      ),
      tabPanel("Quadrat Analysis",
               div(class="outer",
                   tags$head(
                     includeCSS("styles.css")
                   )),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             h2("REMGIS"),
                             checkboxGroupInput("quadrat_industry", "Show",
                                                  choices = c(
                                                    "Legal" = "Legal",
                                                    "Banking" = "Bank",
                                                    "Consultancy" = "Consultancy",
                                                    "Accounting" = "Accountancy",
                                                    "Architectural" = "Architectural"
                                                  )
                             ),
                             sliderInput("row_quadrat",
                                         "Rows:",
                                         min = 1,
                                         max = 10,
                                         value = 5,
                                         step = 1),
                             sliderInput("col_quadrat",
                                         "Columns:",
                                         min = 1,
                                         max = 10,
                                         value = 5, 
                                         step =1)
                             
               ),
               fluidRow(
                 conditionalPanel(condition="input.quadrat_industry.indexOf('Legal') > -1",
                                  column(width = 6, class = "well",
                                         plotOutput("plotLegalQuadrat"),uiOutput("pvalueLegal")
                                  )), 
                 conditionalPanel(condition="input.quadrat_industry.indexOf('Bank') > -1",
                                  column(width = 6, class="well",
                                         plotOutput("plotBankQuadrat"),uiOutput("pvalueBank")
                                  )),  
                 conditionalPanel(condition="input.quadrat_industry.indexOf('Consultancy') > -1",
                                  column(width = 6, class="well",
                                         plotOutput("plotConsultancyQuadrat"),uiOutput("pvalueConsultancy")
                                  )),    
                 conditionalPanel(condition="input.quadrat_industry.indexOf('Accountancy') > -1",
                                  column(width = 6, class="well",
                                         plotOutput("plotAccountancyQuadrat"),uiOutput("pvalueAccountancy")
                                  ))
                 ,
                 conditionalPanel(condition="input.quadrat_industry.indexOf('Architectural') > -1",
                                  column(width = 6, class="well",
                                         plotOutput("plotArchitecturalQuadrat"),uiOutput("pvalueArchitectural")
                                  ))      
               ),
               column(width = 12, class = "well",
                  plotOutput("quadratAll"), uiOutput("pvalueAll")
               )
               
      )         
    )   
  )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
   
  # packages = c('leaflet.minicharts', 'manipulateWidget', 'ggmap', 'ggplot2', 'tmap', 'sf', 'rgdal', 'maptools', 'spatstat', 'amap', 'REAT', 'dplyr', 'gridExtra')
  # for(p in packages){
  #   if(!require(p, character.only = T)){
  #     install.packages(p)
  #   }
  #   library(p,character.only = T)
  # }
  # 
  library(dplyr)
  library(spatstat)
  library(rgdal)
  library(amap)
  library(REAT)
  library(maptools)
  library(ggmap)
  library(ggplot2)
  library(tmap)
  library(sf)
  library(shiny)
  library(leaflet)
  library(leaflet.minicharts)
  library(manipulateWidget)

  #observe and update the slider input from comparison panel to KDE map panel
  observe({
    c_num <- input$distance1
    updateNumericInput(session, "distance", value = c_num)
    updateSliderInput(session, "distance",
                      value = c_num)
  })
  
  #observe and update the slider input from KDE Map panel to Comparison panel
  observe({
    c_numCompare <- input$distance
    updateNumericInput(session, "distance1", value = c_numCompare)
    updateSliderInput(session, "distance1",
                      value = c_numCompare)
  })
  
  #upload file reuse
  getFile <- reactive({
    req(input$file1)
    
    if (is.null(input$file1))
      return(NULL)
    
    firms <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    return(firms)
  })
  
  getDataUploadedOrNot <- reactive({
    if(is.null(input$file1)) return(NULL)
    return(input$file1)
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(getDataUploadedOrNot()))
  })

  outputOptions(output, "fileUploaded", suspendWhenHidden=FALSE)
  
  getInputSector <- reactive({
      sector <- input$sector
      return(sector)
  })
  
  getSpatialAdjusted <- reactive({
    firms <- getFile()
    firms <- subset(firms,type==getInputSector())
    
    firms_shp <- st_as_sf(firms, coords = c("lon", "lat"), crs = 4326)
    firms_shp<-st_transform(firms_shp, 3414)
    firms_sp1 = as(firms_shp, 'Spatial')
    firms_sp2 = as(firms_sp1, 'SpatialPoints')
    return(firms_sp2)
  })
  
  #Get reactive ppp object for plotting
  test <- reactive({
    firms_sp2 <- getSpatialAdjusted()
    
    subzones <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_cr <- subzones[subzones$REGION_C == "CR",]
    cr_sp = as(subzone_cr, 'SpatialPolygons')
    
    cr_owin <- as.owin.SpatialPolygons(cr_sp)
    
    point.ppp <- ppp(coordinates(firms_sp2)[,1], coordinates(firms_sp2)[,2], window = cr_owin)
    
    return(point.ppp)
  })
  
  #Get ppp object for interactive map
  getDensity <- reactive({
    kde_sp_100 <- density(test(), input$distance)
    return(kde_sp_100)
  })
  
  #Plot KDE leaflet in interactive map view 
  output$distPlot <- renderLeaflet({
    
    library(tmap)
    library(tmaptools)
    library(raster)
    kde_sp_100 <- getDensity()
    
    r<-raster(kde_sp_100)
    crs(r) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r, colors = "Reds", opacity = 0.4)#%>%

  })
  
  
  output$SPPlot_legal <- renderLeaflet({
    firms1 <- getFile()
    firms_legal <- subset(firms1,type=="Legal")
    firms_legal$vectorName <- iconv(enc2utf8(firms_legal$vectorName),sub="byte")
    firms_legal$vectorAddress <- iconv(enc2utf8(firms_legal$vectorAddress),sub="byte")
    firms_legal$type <- iconv(enc2utf8(firms_legal$type),sub="byte")
    legal_coy <- st_as_sf(firms_legal, coords = c("lon", "lat"), crs = 4326)
    l1<-st_transform(legal_coy, 3414)
    shape_legal <- tm_shape(l1) + tm_dots(col = "red", size=0.02, title = "Firm Type", popup.vars=c("Industry"="type", "Firm Name"="vectorName", "Address"="vectorAddress")) 
    ?tm_dots 
    tmap_leaflet(shape_legal)
  })
  
  output$SPPlot_bank <- renderLeaflet({
    firms2 <- getFile()
    firms_bank <- subset(firms2,type=="Bank")
    firms_bank$vectorName <- iconv(enc2utf8(firms_bank$vectorName),sub="byte")
    firms_bank$vectorAddress <- iconv(enc2utf8(firms_bank$vectorAddress),sub="byte")
    firms_bank$type <- iconv(enc2utf8(firms_bank$type),sub="byte")
    bank_coy <- st_as_sf(firms_bank, coords = c("lon", "lat"), crs = 4326)
    l2<-st_transform(bank_coy, 3414)
    shape_bank <- tm_shape(l2) + tm_dots(col = "blue", size=0.02, title = "Firm Type", popup.vars=c("Industry"="type", "Firm Name"="vectorName", "Address"="vectorAddress")) 
    ?tm_dots 
    tmap_leaflet(shape_bank)
  })
  
  output$SPPlot_con <- renderLeaflet({
    firms3 <- getFile()
    firms_con <- subset(firms3,type=="Consultancy")
    firms_con$vectorName <- iconv(enc2utf8(firms_con$vectorName),sub="byte")
    firms_con$vectorAddress <- iconv(enc2utf8(firms_con$vectorAddress),sub="byte")
    firms_con$type <- iconv(enc2utf8(firms_con$type),sub="byte")
    con_coy <- st_as_sf(firms_con, coords = c("lon", "lat"), crs = 4326)
    l3<-st_transform(con_coy, 3414)
    shape_con <- tm_shape(l3) + tm_dots(col = "darkgreen", size=0.02, title = "Firm Type", popup.vars=c("Industry"="type", "Firm Name"="vectorName", "Address"="vectorAddress")) 
    ?tm_dots 
    tmap_leaflet(shape_con)
  })
  
  output$SPPlot_acct <- renderLeaflet({
    firms <- getFile()
    firms_act <- subset(firms,type=="Accountancy")
    firms_act$vectorName <- iconv(enc2utf8(firms_act$vectorName),sub="byte")
    firms_act$vectorAddress <- iconv(enc2utf8(firms_act$vectorAddress),sub="byte")
    firms_act$type <- iconv(enc2utf8(firms_act$type),sub="byte")
    ac_coy <- st_as_sf(firms_act, coords = c("lon", "lat"), crs = 4326)
    l4<-st_transform(ac_coy, 3414)
    shape_a <- tm_shape(l4) + tm_dots(col = "darkorange", size=0.02, title = "Firm Type", popup.vars=c("Industry"="type", "Firm Name"="vectorName", "Address"="vectorAddress")) 
    ?tm_dots 
    tmap_leaflet(shape_a)
  })
  
  output$SPPlot_arch <- renderLeaflet({
    firms5 <- getFile()
    firms_arch <- subset(firms5,type=="Architectural")
    firms_arch$vectorName <- iconv(enc2utf8(firms_arch$vectorName),sub="byte")
    firms_arch$vectorAddress <- iconv(enc2utf8(firms_arch$vectorAddress),sub="byte")
    firms_arch$type <- iconv(enc2utf8(firms_arch$type),sub="byte")
    arch_coy <- st_as_sf(firms_arch, coords = c("lon", "lat"), crs = 4326)
    l5<-st_transform(arch_coy, 3414)
    shape_arch <- tm_shape(l5) + tm_dots(col = "darkviolet", size=0.02, title = "Firm Type", popup.vars=c("Industry"="type", "Firm Name"="vectorName", "Address"="vectorAddress")) 
    ?tm_dots 
    tmap_leaflet(shape_arch)
  })
  
  #Plot KDE for comparison panel
  output$plotLegal <- renderLeaflet({
    firms <- getFile()
    firms <- subset(firms,type=="Legal")
    
    firms_shp <- st_as_sf(firms, coords = c("lon", "lat"), crs = 4326)
    firms_shp<-st_transform(firms_shp, 3414)
    firms_sp1 = as(firms_shp, 'Spatial')
    firms_sp2 = as(firms_sp1, 'SpatialPoints')
    
    subzones <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_cr <- subzones[subzones$REGION_C == "CR",]
    cr_sp = as(subzone_cr, 'SpatialPolygons')
    
    cr_owin <- as.owin.SpatialPolygons(cr_sp)
    
    point.ppp <- ppp(coordinates(firms_sp2)[,1], coordinates(firms_sp2)[,2], window = cr_owin)
    
    Legal <- density(point.ppp, input$distance)
    
    r1<-raster(Legal)
    crs(r1) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r1, colors = "Reds", opacity = 0.4)%>%
      syncWith("maps")
  })
  
  #Plot KDE Leaflet for banks in comparison view 
  output$plotBank <- renderLeaflet({
    firmsBank <- getFile()
    firmsBank <- subset(firmsBank,type="Bank")
    
    firms_shpBank <- st_as_sf(firmsBank, coords = c("lon", "lat"), crs = 4326)
    firms_shpBank<-st_transform(firms_shpBank, 3414)
    firms_sp1Bank = as(firms_shpBank, 'Spatial')
    firms_sp2Bank = as(firms_sp1Bank, 'SpatialPoints')
    
    subzonesB <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_crB <- subzonesB[subzonesB$REGION_C == "CR",]
    cr_spB = as(subzone_crB, 'SpatialPolygons')
    
    cr_owinB <- as.owin.SpatialPolygons(cr_spB)
    
    point.pppB <- ppp(coordinates(firms_sp2Bank)[,1], coordinates(firms_sp2Bank)[,2], window = cr_owinB)
    
    Banks <- density(point.pppB, input$distance)
    
    r2<-raster(Banks)
    crs(r2) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r2, colors = "Reds", opacity = 0.4)%>%
      syncWith("maps")
    
  })
  
  #Plot KDE map for consultancy in KDE comparison view 
  output$plotConsultancy <- renderLeaflet({
    firms <- getFile()
    firms <- subset(firms,type=="Consultancy")
    
    firms_shp <- st_as_sf(firms, coords = c("lon", "lat"), crs = 4326)
    firms_shp<-st_transform(firms_shp, 3414)
    firms_sp1 = as(firms_shp, 'Spatial')
    firms_sp2 = as(firms_sp1, 'SpatialPoints')
    
    subzones <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_cr <- subzones[subzones$REGION_C == "CR",]
    cr_sp = as(subzone_cr, 'SpatialPolygons')
    
    cr_owin <- as.owin.SpatialPolygons(cr_sp)
    
    point.ppp <- ppp(coordinates(firms_sp2)[,1], coordinates(firms_sp2)[,2], window = cr_owin)
    
    Consultancy <- density(point.ppp, input$distance)
    
    r3<-raster(Consultancy)
    crs(r3) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r3, colors = "Reds", opacity = 0.4)%>%
      syncWith("maps")
  })
  
  output$plotAccountancy <- renderLeaflet({
    # generate bins based on input$bins from ui.R
    firms <- getFile()
    firms <- subset(firms,type=="Accountancy")
    
    firms_shp <- st_as_sf(firms, coords = c("lon", "lat"), crs = 4326)
    firms_shp<-st_transform(firms_shp, 3414)
    firms_sp1 = as(firms_shp, 'Spatial')
    firms_sp2 = as(firms_sp1, 'SpatialPoints')
    
    subzones <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_cr <- subzones[subzones$REGION_C == "CR",]
    cr_sp = as(subzone_cr, 'SpatialPolygons')
    
    cr_owin <- as.owin.SpatialPolygons(cr_sp)
    
    point.ppp <- ppp(coordinates(firms_sp2)[,1], coordinates(firms_sp2)[,2], window = cr_owin)
    
    Accountancy <- density(point.ppp, input$distance)
    
    r4<-raster(Accountancy)
    crs(r4) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r4, colors = "Reds", opacity = 0.4)%>%
      syncWith("maps")
  })
  
  #Plot KDE Comparison view for architecture firms 
  output$plotArchitectural <- renderLeaflet({
    firms <- getFile()
    firms <- subset(firms,type=="Architectural")
    
    firms_shp <- st_as_sf(firms, coords = c("lon", "lat"), crs = 4326)
    firms_shp<-st_transform(firms_shp, 3414)
    firms_sp1 = as(firms_shp, 'Spatial')
    firms_sp2 = as(firms_sp1, 'SpatialPoints')
    
    subzones <- readOGR(dsn="shp/master-plan-2014-subzone-boundary-no-sea", layer="MP14_SUBZONE_NO_SEA_PL")
    subzone_cr <- subzones[subzones$REGION_C == "CR",]
    cr_sp = as(subzone_cr, 'SpatialPolygons')
    
    cr_owin <- as.owin.SpatialPolygons(cr_sp)
    
    point.ppp <- ppp(coordinates(firms_sp2)[,1], coordinates(firms_sp2)[,2], window = cr_owin)
    
    Architectural <- density(point.ppp, input$distance)
    
    r5<-raster(Architectural)
    crs(r5) <- CRS("+init=epsg:3414")
    
    leaflet() %>%
      addTiles()%>%
      addRasterImage(r5, colors = "Spectral", opacity = 0.4)%>%
      syncWith("maps")
  })

  getAllCompaniesCBD <- reactive({
    firms <- getFile()
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    all_cbd <- firms[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms$postal_code), ]
    all_cbd_count <- nrow(all_cbd)
    return((all_cbd_count))
  })
  
  getAllCompaniesJurong <- reactive({
    firms <- getFile()
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    all_jur <- firms[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms$postal_code), ]
    all_jur_count <- nrow(all_jur)
    return(all_jur_count)
  })
  
  getAllCompanies <- reactive({
    firms <- getFile()
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    all_country_count <- nrow(firms)
    return((all_country_count))
  })
  
  getLegalLQCBD <- reactive({
    firms <- getFile()
    firms_legal <- firms[grep("Legal", firms$type), ]
    firms_legal <- firms_legal[!duplicated(firms_legal$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    legal_cbd <- firms_legal[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_legal$postal_code), ]
    legal_cbd_count <- nrow(legal_cbd)
    all_cbd_count <- getAllCompaniesCBD()
    legal_country_count <- nrow(firms_legal)
    all_country_count <- getAllCompanies()
    var <- locq(legal_cbd_count, all_cbd_count, legal_country_count, all_country_count)
    return(var)
  })
  
  getLegalLQJur <- reactive({
    firms <- getFile()
    firms_legal <- firms[grep("Legal", firms$type), ]
    firms_legal <- firms_legal[!duplicated(firms_legal$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    legal_jur <- firms_legal[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_legal$postal_code), ]
    legal_jur_count <- nrow(legal_jur)
    all_jur_count <- getAllCompaniesJurong()
    legal_country_count <- nrow(firms_legal)
    all_country_count <- getAllCompanies()
    var1 <- locq(legal_jur_count, all_jur_count, legal_country_count, all_country_count)
    return(var1)
  })
  
  getLegalCount <- reactive({
    firms <- getFile()
    firms_legal <- firms[grep("Legal", firms$type), ]
    firms_legal <- firms_legal[!duplicated(firms_legal$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    legal_cbd <- firms_legal[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_legal$postal_code), ]
    legal_cbd_count <- nrow(legal_cbd)
    return(legal_cbd_count)
  })
  
  getLegalCountJur <- reactive({
    firms <- getFile()
    firms_legal <- firms[grep("Legal", firms$type), ]
    firms_legal <- firms_legal[!duplicated(firms_legal$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    legal_jur <- firms_legal[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_legal$postal_code), ]
    legal_jur_count <- nrow(legal_jur)
    return(legal_jur_count)
  })
  
  getBanksCount <- reactive({
    firms <- getFile()
    firms_bank <- firms[grep("Bank", firms$type), ]
    #firms_bank <- firms_bank[!duplicated(firms_bank$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    bank_cbd <- firms_bank[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_bank$postal_code), ]
    bank_cbd_count <- nrow(bank_cbd)
    return(bank_cbd_count)
  })
  
  getBanksCountJur <- reactive({
    firms <- getFile()
    firms_bank <- firms[grep("Bank", firms$type), ]
    #firms_bank <- firms_bank[!duplicated(firms_bank$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    bank_jur <- firms_bank[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_bank$postal_code), ]
    bank_jur_count <- nrow(bank_jur)
    return(bank_jur_count)
  })
  
  getConsultancyCount <- reactive({
    firms <- getFile()
    firms_consultancy <- firms[grep("Consultancy", firms$type), ]
    firms_consultancy <- firms_consultancy[!duplicated(firms_consultancy$postal_code),]
    consultancy_cbd <- firms_consultancy[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_consultancy$postal_code), ]
    consultancy_cbd_count <- nrow(consultancy_cbd)
    return(consultancy_cbd_count)
  })  
  
  getConsultancyCountJur <- reactive({
    firms <- getFile()
    firms_consultancy <- firms[grep("Consultancy", firms$type), ]
    firms_consultancy <- firms_consultancy[!duplicated(firms_consultancy$postal_code),]
    consultancy_jur <- firms_consultancy[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_consultancy$postal_code), ]
    consultancy_jur_count <- nrow(consultancy_jur)
    return(consultancy_jur_count)
  })  
  
  getAccountancyCount <- reactive({
    firms <- getFile()
    firms_accountancy <- firms[grep("Accountancy", firms$type), ]
    firms_accountancy <- firms_accountancy[!duplicated(firms_accountancy$postal_code),]
    accountancy_cbd <- firms_accountancy[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_accountancy$postal_code), ]
    accountancy_cbd_count <- nrow(accountancy_cbd)
    return(accountancy_cbd_count)
  })  
  
  getAccountancyCountJur <- reactive({
    firms <- getFile()
    firms_accountancy <- firms[grep("Accountancy", firms$type), ]
    firms_accountancy <- firms_accountancy[!duplicated(firms_accountancy$postal_code),]
    accountancy_jur <- firms_accountancy[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_accountancy$postal_code), ]
    accountancy_jur_count <- nrow(accountancy_jur)
    return(accountancy_jur_count)
  })  
  
  getArchitecturalCount <- reactive({
    firms <- getFile()
    firms_architectural <- firms[grep("Architectural", firms$type), ]
    firms_architectural <- firms_architectural[!duplicated(firms_architectural$postal_code),]
    architectural_cbd <- firms_architectural[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_architectural$postal_code), ]
    architectural_cbd_count <- nrow(architectural_cbd)
    return(architectural_cbd_count)
  })  
  
  getArchitecturalCountJur <- reactive({
    firms <- getFile()
    firms_architectural <- firms[grep("Architectural", firms$type), ]
    firms_architectural <- firms_architectural[!duplicated(firms_architectural$postal_code),]
    architectural_jur <- firms_architectural[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_architectural$postal_code), ]
    architectural_jur_count <- nrow(architectural_jur)
    return(architectural_jur_count)
  })
  
  getBankLQCBD <- reactive({
    firms <- getFile()
    firms_bank <- firms[grep("Bank", firms$type), ]
    firms_bank <- firms_bank[!duplicated(firms_bank$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    bank_cbd <- firms_bank[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_bank$postal_code), ]
    bank_cbd_count <- nrow(bank_cbd)
    bank_country_count <- nrow(firms_bank)
    all_cbd_count <- getAllCompaniesCBD()
    all_country_count <- getAllCompanies()
    return(locq(bank_cbd_count, all_cbd_count, bank_country_count, all_country_count))
  })
  
  getBankLQJur <- reactive({
    firms <- getFile()
    firms_bank <- firms[grep("Bank", firms$type), ]
    firms_bank <- firms_bank[!duplicated(firms_bank$postal_code),]
    firms <- firms[!duplicated(firms[,c("postal_code","type")]),]
    bank_jur <- firms_bank[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_bank$postal_code), ]
    bank_jur_count <- nrow(bank_jur)
    all_jur_count <- getAllCompaniesJurong()
    bank_country_count <- nrow(firms_bank)
    all_country_count <- getAllCompanies()
    return(locq(bank_jur_count, all_jur_count, bank_country_count, all_country_count))
  })
  
  getConsultancyLQCBD <- reactive({
    firms <- getFile()
    firms_consultancy <- firms[grep("Consultancy", firms$type), ]
    firms_consultancy <- firms_consultancy[!duplicated(firms_consultancy$postal_code),]
    consultancy_cbd <- firms_consultancy[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_consultancy$postal_code), ]
    consultancy_cbd_count <- nrow(consultancy_cbd)
    consultancy_country_count <- nrow(firms_consultancy)
    all_cbd_count <- getAllCompaniesCBD()
    all_country_count <- getAllCompanies()
    return(locq(consultancy_cbd_count, all_cbd_count, consultancy_country_count, all_country_count))
  })
  
  getConsultancyLQJur <- reactive({
    firms <- getFile()
    firms_consultancy <- firms[grep("Consultancy", firms$type), ]
    firms_consultancy <- firms_consultancy[!duplicated(firms_consultancy$postal_code),]
    consultancy_jur <- firms_consultancy[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_consultancy$postal_code), ]
    consultancy_jur_count <- nrow(consultancy_jur)
    all_jur_count <- getAllCompaniesJurong()
    consultancy_country_count <- nrow(firms_consultancy)
    all_country_count <- getAllCompanies()
    return(locq(consultancy_jur_count, all_jur_count, consultancy_country_count, all_country_count))
  })
  
  getAccountancyLQCBD <- reactive({
    firms <- getFile()
    firms_accountancy <- firms[grep("Accountancy", firms$type), ]
    firms_accountancy <- firms_accountancy[!duplicated(firms_accountancy$postal_code),]
    accountancy_cbd <- firms_accountancy[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_accountancy$postal_code), ]
    accountancy_cbd_count <- nrow(accountancy_cbd)
    accountancy_country_count <- nrow(firms_accountancy)
    all_cbd_count <- getAllCompaniesCBD()
    all_country_count <- getAllCompanies()
    return(locq(accountancy_cbd_count, all_cbd_count, accountancy_country_count, all_country_count))
  })
  
  getAccountancyLQJur <- reactive({
    firms <- getFile()
    firms_accountancy <- firms[grep("Accountancy", firms$type), ]
    firms_accountancy <- firms_accountancy[!duplicated(firms_accountancy$postal_code),]
    accountancy_jur <- firms_accountancy[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_accountancy$postal_code), ]
    accountancy_jur_count <- nrow(accountancy_jur)
    all_jur_count <- getAllCompaniesJurong()
    accountancy_country_count <- nrow(firms_accountancy)
    all_country_count <- getAllCompanies()
    return(locq(accountancy_jur_count, all_jur_count, accountancy_country_count, all_country_count))
  })
  
  getArchitecturalLQCBD <- reactive({
    firms <- getFile()
    firms_architectural <- firms[grep("Architectural", firms$type), ]
    firms_architectural <- firms_architectural[!duplicated(firms_architectural$postal_code),]
    architectural_cbd <- firms_architectural[grep("Singapore 01|Singapore 02|Singapore 03|Singapore 04|Singapore 05|Singapore 06|Singapore 07|Singapore 08|Singapore 14|Singapore 15|Singapore 16|Singapore 09|Singapore 10|Singapore 11|Singapore 120|Singapore 13|Singapore 17|Singapore 18|Singapore 19|Singapore 20|Singapore 21|Singapore 22|Singapore 23|Singapore 24|Singapore 25|Singapore 26|Singapore 27|Singapore 28|Singapore 29|Singapore 30|Singapore 31|Singapore 32|Singapore 33|Singapore 34|Singapore 35|Singapore 36|Singapore 37|Singapore 38|Singapore 39|Singapore 40|Singapore 41|Singapore 42|Singapore 43|Singapore 44|Singapore 45|Singapore 57|Singapore 58|Singapore 59|Singapore 77", firms_architectural$postal_code), ]
    architectural_cbd_count <- nrow(architectural_cbd)
    architectural_country_count <- nrow(firms_architectural)
    all_cbd_count <- getAllCompaniesCBD()
    all_country_count <- getAllCompanies()
    return(locq(architectural_cbd_count, all_cbd_count, architectural_country_count, all_country_count))
  })
  
  getArchitecturalLQJur <- reactive({
    firms <- getFile()
    firms_architectural <- firms[grep("Architectural", firms$type), ]
    firms_architectural <- firms_architectural[!duplicated(firms_architectural$postal_code),]
    architectural_jur <- firms_architectural[grep("Singapore 608|Singapore 609|Singapore 6001|Singapore 6002", firms_architectural$postal_code), ]
    architectural_jur_count <- nrow(architectural_jur)
    all_jur_count <- getAllCompaniesJurong()
    architectural_country_count <- nrow(firms_architectural)
    all_country_count <- getAllCompanies()
    return(locq(architectural_jur_count, all_jur_count, architectural_country_count, all_country_count))
  })
  
  output$LQPlot <- renderPlot({
    values <- c(getLegalLQCBD(), getBankLQCBD(), getConsultancyLQCBD(), getAccountancyLQCBD(), getArchitecturalLQCBD())
    barplot(values,
            main = "Location Quotient in CBD",
            xlab = "Sector",
            ylab = "Location Quotient",
            names.arg = c("Legal", "Banks", "Consultancy", "Accountancy", "Architectural"),
            col = "darkred")
  })
  
  output$LQPlotJurong <- renderPlot({
    values <- c(getLegalLQJur(), getBankLQJur(), getConsultancyLQJur(), getAccountancyLQJur(), getArchitecturalLQJur())
    barplot(values,
            main = "Location Quotient in Jurong Lake District",
            xlab = "Sector",
            ylab = "Location Quotient",
            names.arg = c("Legal", "Banks", "Consultancy", "Accountancy", "Architectural"),
            col = "darkblue")
  })
  
  output$lqLegal <- renderUI({
    str3 <- paste("<b>Location Quotient in CBD:</b> ", getLegalLQCBD(), " ")
    str4 <- paste("<b>Location Quotient in Jurong Lake District:</b> ", getLegalLQJur(), " ")
    strl1 <- paste("<b>Number of Legal firms in CBD:</b> ", getLegalCount(), " ")
    strAll <- paste("<b>Number of firms in CBD:</b> ", getAllCompaniesCBD(), " ")
    strl2 <- paste("<b>Number of Legal firms in Jurong Lake District:</b> ", getLegalCountJur(), " ")
    strJur <- paste("<b>Number of firms in Jurong Lake District:</b> ", getAllCompaniesJurong(), " ")
    HTML(paste(str3, str4, strl1, strAll, strl2, strJur, sep = "<br/>"))
  })
  
  output$lqBank <- renderUI({
    strB1 <- paste("<b>Location Quotient in CBD:</b> ", getBankLQCBD(), " ")
    strB2 <- paste("<b>Location Quotient in Jurong Lake District:</b> ", getBankLQJur(), " ")
    strbnum1 <- paste("<b>Number of Banks in CBD:</b> ", getBanksCount(), " ")
    strAllBank <- paste("<b>Number of firms in CBD:</b> ", getAllCompaniesCBD(), " ")
    strbnum2 <- paste("<b>Number of Birms in Jurong Lake District:</b> ", getBanksCountJur(), " ")
    strJurBank <- paste("<b>Number of firms in Jurong Lake District:</b> ", getAllCompaniesJurong(), " ")
    HTML(paste(strB1, strB2, strbnum1, strAllBank, strbnum2, strJurBank, sep = "<br/>"))
  })
  
  output$lqConsultancy <- renderUI({
    strC1 <- paste("<b>Location Quotient in CBD:</b> ", getConsultancyLQCBD(), " ")
    strC2 <- paste("<b>Location Quotient in Jurong Lake District:</b> ", getConsultancyLQJur(), " ")
    strcnum1 <- paste("<b>Number of Consultancy Firms in CBD:</b> ", getConsultancyCount(), " ")
    strAllC <- paste("<b>Number of firms in CBD:</b> ", getAllCompaniesCBD(), " ")
    strcnum2 <- paste("<b>Number of Consultancy Firms in Jurong Lake District:</b> ", getConsultancyCountJur(), " ")
    strJurC <- paste("<b>Number of firms in Jurong Lake District:</b> ", getAllCompaniesJurong(), " ")
    HTML(paste(strC1, strC2, strcnum1, strAllC, strcnum2, strJurC, sep = "<br/>"))
  })
  
  output$lqAccountancy <- renderUI({
    strA1 <- paste("<b>Location Quotient in CBD:</b> ", getAccountancyLQCBD(), " ")
    strA2 <- paste("<b>Location Quotient in Jurong Lake District:</b> ", getAccountancyLQJur(), " ")
    stranum1 <- paste("<b>Number of Accounting Firms in CBD:</b> ", getAccountancyCount(), " ")
    strAllA <- paste("<b>Number of firms in CBD:</b> ", getAllCompaniesCBD(), " ")
    stranum2 <- paste("<b>Number Accounting Firms in Jurong Lake District:</b> ", getAccountancyCountJur(), " ")
    strJurA <- paste("<b>Number of firms in Jurong Lake District:</b> ", getAllCompaniesJurong(), " ")
    HTML(paste(strA1, strA2, stranum1, strAllA, stranum2, strJurA, sep = "<br/>"))
  })
  
  output$lqArchitectural <- renderUI({
    strAr1 <- paste("<b>Location Quotient in CBD:</b> ", getArchitecturalLQCBD(), " ")
    strAr2 <- paste("<b>Location Quotient in Jurong Lake District:</b> ", getArchitecturalLQJur(), " ")
    strarcnum1 <- paste("<b>Number of Architectural Firms in CBD:</b> ", getArchitecturalCount(), " ")
    strAllArc <- paste("<b>Number of firms in CBD:</b> ", getAllCompaniesCBD(), " ")
    strarcnum2 <- paste("<b>Number of Architectural Firms in Jurong:</b> ", getArchitecturalCountJur(), " ")
    strJurArc <- paste("<b>Number of firms in Jurong Lake District:</b> ", getAllCompaniesJurong(), " ")
    HTML(paste(strAr1, strAr2,strarcnum1, strAllArc, strarcnum2, strJurArc, sep = "<br/>"))
  })
  
  output$analysis <- renderUI({
    HTML(paste("<b>LQ Analysis</b>: <br/> If LQ < 1, the output is not sufficient to form a CBD and more firms of this sector are needed. It is also non-basic. <br/> If LQ > 1, the output is more than sufficient to form a CBD and exporting more firms is an option. It is basic."))
  })
  
  mpsz_svy21 <- reactive({
    crs.New <- "+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs"
    mpsz <- readOGR(dsn = "shp/master-plan-2014-subzone-boundary-no-sea", layer = "MP14_SUBZONE_NO_SEA_PL")
    mpsz_svy21 <- spTransform(mpsz, crs.New)
    return(mpsz_svy21)
  })
  
  firmsSpatialObj <- reactive({
    industries <- getFile()
    industries_shp <- st_as_sf(industries, coords = c("lon","lat"), crs = 4326)
    industries_shp <-st_transform(industries_shp, crs = 3414)
    industries_sp1 = as(industries_shp, 'Spatial')
    return(industries_sp1)
  })
  
  firmsGetCBDObj <- reactive({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    firms_points <- industries_sp1[mpsz_svy21,]
    firms_points <- remove.duplicates(industries_sp1)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    return(cbd)
  })
  
  firmsGetCBDPoints <- reactive({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    firms_points <- industries_sp1[mpsz_svy21,]
    firms_points <- remove.duplicates(industries_sp1)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    cbd_points <- firms_points[cbd,]
    return(cbd_points)
  }) 

  output$quadratAll <- renderPlot({
    cbd_points <- firmsGetCBDPoints()
    cbd_points$type <- factor(cbd_points$type)
    levels(cbd_points$type)
    cbd <- firmsGetCBDObj()
    window <- as.owin(cbd)
    mycolors = c('red','blue','green','orange','violet')
    firms_ppp <- ppp(x=cbd_points@coords[,1],y=cbd_points@coords[,2], window = window) 
    qc <- quadratcount(firms_ppp, nx = input$col_quadrat, ny= input$row_quadrat) 
    plot(qc,col="red", main="Professional Services in the CBD")
    plot(cbd_points,pch=1,cex=0.5, col = mycolors, add=T)
    legend("topright", 
           legend = c("Legal", "Banks", "Consultancy", "Accountancy", "Architectural"), 
           col = c("red", "blue", "darkgreen", "darkorange", "darkviolet"),
           rgb(0.8,0.4,0.1,0.7), 
           pch = c(17), 
           bty = "n", 
           pt.cex = 2, 
           cex = 1.2, 
           text.col = "black", 
           horiz = F , 
           inset = c(0.1, 0.1))
  })
  
  output$plotLegalQuadrat <- renderPlot({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    legal <- industries_sp1[industries_sp1$type=='Legal', ]
    legal_points <- legal[mpsz_svy21,]
    legal_points <- remove.duplicates(legal)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    window <- as.owin(cbd)
    cbd_legal <- legal_points[cbd,]
    legal_ppp <- ppp(x=legal_points@coords[,1],y=legal_points@coords[,2], window = window)
    qc_legal <- quadratcount(legal_ppp, nx = input$col_quadrat, ny= input$row_quadrat) 
    plot(qc_legal,col="black", main="Legal firms in the CBD")
    plot(cbd_legal,pch=16,cex=0.5, col="red", add=T)
  })
  
  output$plotBankQuadrat <- renderPlot({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    bank <- industries_sp1[industries_sp1$type=='Bank', ]
    bank_points <- bank[mpsz_svy21,]
    bank_points <- remove.duplicates(bank)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    window <- as.owin(cbd)
    cbd_bank <- bank_points[cbd,]
    bank_ppp <- ppp(x=bank_points@coords[,1],y=bank_points@coords[,2], window = window)
    qc_bank <- quadratcount(bank_ppp, nx = input$col_quadrat, ny= input$row_quadrat) 
    plot(qc_bank,col="black",main="Banking firms in the CBD")
    plot(cbd_bank,pch=16,cex=0.5, col="blue", add=T)
  })
  
  output$plotConsultancyQuadrat <- renderPlot({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    consultancy <- industries_sp1[industries_sp1$type=='Consultancy', ]
    consultancy_points <- consultancy[mpsz_svy21,]
    consultancy_points <- remove.duplicates(consultancy)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    window <- as.owin(cbd)
    cbd_consultancy <- consultancy_points[cbd,]
    consultancy_ppp <- ppp(x=consultancy_points@coords[,1],y=consultancy_points@coords[,2], window = window)
    qc_consultancy <- quadratcount(consultancy_ppp, nx = input$col_quadrat, ny= input$row_quadrat) 
    plot(qc_consultancy,col="black",main="Consultancy firms in the CBD")
    plot(cbd_consultancy,pch=16,cex=0.5, col="darkgreen", add=T)
  })
  
  output$plotAccountancyQuadrat <- renderPlot({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    Accountancy <- industries_sp1[industries_sp1$type=='Accountancy', ]
    Accountancy_points <- Accountancy[mpsz_svy21,]
    Accountancy_points <- remove.duplicates(Accountancy)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    window <- as.owin(cbd)
    cbd_Accountancy <- Accountancy_points[cbd,]
    Accountancy_ppp <- ppp(x=Accountancy_points@coords[,1],y=Accountancy_points@coords[,2], window = window)
    qc_Accountancy <- quadratcount(Accountancy_ppp, nx = input$col_quadrat, ny= input$row_quadrat) 
    plot(qc_Accountancy,col="black",main="Accountancy firms in the CBD")
    plot(cbd_Accountancy,pch=16,cex=0.5, col="darkorange", add=T)
  })
  
  output$plotArchitecturalQuadrat <- renderPlot({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    architectural <- industries_sp1[industries_sp1$type=='Architectural', ]
    architectural_points <- architectural[mpsz_svy21,]
    architectural_points <- remove.duplicates(architectural)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    window <- as.owin(cbd)
    cbd_architectural <- architectural_points[cbd,]
    architectural_ppp <- ppp(x=architectural_points@coords[,1],y=architectural_points@coords[,2], window = window)
    qc_architectural <- quadratcount(architectural_ppp, nx = input$col_quadrat, ny= input$row_quadrat) 
    plot(qc_architectural,col="black",main="Architectural firms in the CBD")
    plot(cbd_architectural,pch=16,cex=0.5, col="darkviolet", add=T)
  })
  
  output$pvalueAll <- renderUI({
    cbd_points <- firmsGetCBDPoints()
    cbd_points$type <- factor(cbd_points$type)
    levels(cbd_points$type)
    cbd <- firmsGetCBDObj()
    window <- as.owin(cbd)
    firms_ppp <- ppp(x=cbd_points@coords[,1],y=cbd_points@coords[,2], window = window) 
    check = quadrat.test(firms_ppp, method = "M", nsim = 999)
    strAr1 <- HTML(paste("<b>P-value:</b> ", check$p.value, " "))  
  })
  
  output$pvalueLegal <- renderUI({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    legal <- industries_sp1[industries_sp1$type=='Legal', ]
    legal_points <- legal[mpsz_svy21,]
    legal_points <- remove.duplicates(legal)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    window <- as.owin(cbd)
    cbd_legal <- legal_points[cbd,]
    legal_ppp <- ppp(x=legal_points@coords[,1],y=legal_points@coords[,2], window = window)
    check1 = quadrat.test(legal_ppp, method = "M", nsim = 999)
    strAr1 <- HTML(paste("<b>P-value:</b> ", check1$p.value, " "))  
  })
  
  output$pvalueBank <- renderUI({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    bank <- industries_sp1[industries_sp1$type=='Bank', ]
    bank_points <- bank[mpsz_svy21,]
    bank_points <- remove.duplicates(bank)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    window <- as.owin(cbd)
    cbd_bank <- bank_points[cbd,]
    bank_ppp <- ppp(x=bank_points@coords[,1],y=bank_points@coords[,2], window = window)
    check = quadrat.test(bank_ppp, method = "M", nsim = 999)
    strAr1 <- HTML(paste("<b>P-value:</b> ", check$p.value, " "))  
  })
  
  output$pvalueConsultancy <- renderUI({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    Consultancy <- industries_sp1[industries_sp1$type=='Consultancy', ]
    Consultancy_points <- Consultancy[mpsz_svy21,]
    Consultancy_points <- remove.duplicates(Consultancy)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    window <- as.owin(cbd)
    cbd_Consultancy <- Consultancy_points[cbd,]
    Consultancy_ppp <- ppp(x=Consultancy_points@coords[,1],y=Consultancy_points@coords[,2], window = window)
    check = quadrat.test(Consultancy_ppp, method = "M", nsim = 999)
    strAr1 <- HTML(paste("<b>P-value:</b> ", check$p.value, " "))  
  })
  
  output$pvalueAccountancy <- renderUI({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    Accountancy <- industries_sp1[industries_sp1$type=='Accountancy', ]
    Accountancy_points <- Accountancy[mpsz_svy21,]
    Accountancy_points <- remove.duplicates(Accountancy)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    window <- as.owin(cbd)
    cbd_Accountancy <- Accountancy_points[cbd,]
    Accountancy_ppp <- ppp(x=Accountancy_points@coords[,1],y=Accountancy_points@coords[,2], window = window)
    check = quadrat.test(Accountancy_ppp, method = "M", nsim = 999)
    strAr1 <- HTML(paste("<b>P-value:</b> ", check$p.value, " "))  
  })
  
  output$pvalueArchitectural <- renderUI({
    industries_sp1 <- firmsSpatialObj()
    mpsz_svy21 <- mpsz_svy21()
    Architectural <- industries_sp1[industries_sp1$type=='Architectural', ]
    Architectural_points <- Architectural[mpsz_svy21,]
    Architectural_points <- remove.duplicates(Architectural)
    cbd <- mpsz_svy21[mpsz_svy21@data$REGION_N=="CENTRAL REGION",]
    window <- as.owin(cbd)
    cbd_Architectural <- Architectural_points[cbd,]
    Architectural_ppp <- ppp(x=Architectural_points@coords[,1],y=Architectural_points@coords[,2], window = window)
    check = quadrat.test(Architectural_ppp, method = "M", nsim = 999)
    strAr1 <- HTML(paste("<b>P-value:</b> ", check$p.value, " "))  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

