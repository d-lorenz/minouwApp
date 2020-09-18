
# library(shiny)
library(raster)
library(rgdal)
library(rgeos)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(plyr)

csName <- "Case study X"
csShortDesc <- "Data/shortDesc_CSX.html"

areaPoly <- readRDS(file = "Data/input/areaPoly.RDS")
areaDept <- readRDS(file = "Data/input/areaDept.RDS")
areaFloo <- readRDS(file = "Data/input/areaFloo.RDS")
areaGrid <- readRDS(file = "Data/input/areaGrid.RDS")

mapCoo <- coordinates(areaPoly)

effoData <- readRDS(file = "Data/input/effoData.RDS")
effoAgg <- readRDS(file = "Data/input/effoAgg.RDS")
timeframe <- paste(rep(c("Q1", "Q2", "Q3", "Q4"), times = 5), " - ", rep(2012:2016, each = 4))
effoTime <- reactiveValues(Current = 1, Previous = NULL)
fuzzTime <- reactiveValues(Current = 1, Previous = NULL)
threValu <- values <- reactiveValues(value = 0)

fleetReg <- readRDS(file = "Data/input/fleetRegister.rds")

# UWC maps ####
specieName <- c("SKJ", "TTL")
skjMap <- readRDS(file = "Data/input/uwcSKJ.rds")
ttlMap <- readRDS(file = "Data/input/uwcTTL.rds")

skjFuzz <- readRDS(file = "Data/input/skjFuzz.RDS")
ttlFuzz <- readRDS(file = "Data/input/ttlFuzz.RDS")

# SeaFloor palettes ####
pal_biozon = colorFactor("Set3", areaFloo@data$Zones)(areaFloo@data$Zones)
pal_substr = colorFactor("Set3", areaFloo@data$EEZ)(areaFloo@data$EEZ)

# Effort palettes ####
pal_swar = colorQuantile(palette = c("#FFFFFF00" ,colorRampPalette(c("oldlace", "mediumaquamarine", "darkslateblue"))(49)),
                         domain = c(0, ceiling(max(log10(1 + effoData$SwepArea)))), n = 50, alpha = TRUE)
pal_fida = colorQuantile(palette = c("#FFFFFF00" ,colorRampPalette(c("oldlace", "mediumaquamarine", "darkslateblue"))(49)),
                         domain = c(0, ceiling(max(log10(1 + effoData$FishDays)))), n = 50, alpha = TRUE)
pal_fiho = colorQuantile(palette = c("#FFFFFF00" ,colorRampPalette(c("oldlace", "mediumaquamarine", "darkslateblue"))(49)),
                         domain = c(0, ceiling(max(log10(1 + effoData$FishHour)))), n = 50, alpha = TRUE)
pal_noef = colorQuantile(palette = c("#FFFFFF00" ,colorRampPalette(c("oldlace", "mediumaquamarine", "darkslateblue"))(99)),
                         domain = c(0, ceiling(max(log10(1 + effoData$NomiEffo)))), n = 100, alpha = TRUE)

pal_uwc_skj <- colorNumeric("Blues", domain = c(0, 3))
pal_uwc_ttl <- colorNumeric("Greens", domain = c(0, 3))

pal_fuzz <- colorNumeric(palette = c("#FFFFFF00" ,colorRampPalette(c("yellow1", "tomato", "darkorchid4"))(99)),
                         domain = c(0, 1), n = 100, alpha = TRUE)

## CLIENT ####

ui <- dashboardPage(
  dashboardHeader(title = paste0(csName, " dashboard")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Effort Data", tabName = "effortData", icon = icon("ship")),
      menuItem("Unwanted Catches", tabName = "discardData", icon = icon("pie-chart")),
      menuItem("Fuzzy Overlay", tabName = "fuzzyData", icon = icon("layer-group")),
      menuItem("Overlap Analysis", tabName = "overlap", icon = icon("microscope")),
      menuItem("Wordbook", tabName = "infoBook", icon = icon("info")),
      menuItem("References & Credits", tabName = "refere", icon = icon("book"))
    ),
    div(img(heigth = 100, width = 200, style = "position: relative; left: 5%;",
            src = "minouw_logo2_white.png"), style = "position: fixed; bottom: 0;")
  ),
  dashboardBody(
    tags$style(type = "text/css",
               "#mapEffort {height: calc(100vh - 80px) !important;}",
               "#mapFuzz {height: calc(100vh - 80px) !important;}",
               "#uwcMap {height: calc(100vh - 80px) !important;}"),
    
    fluidRow(
      tabItems(   # Intro Tab ####
                  tabItem(tabName = "dashboard", style = "padding: 10px;",
                          h1("Fictional fishery scenario near Palmyra Atoll"),
                          hr(),
                          fluidRow(column(width = 3, HTML("<h4 align='justify'> Combining work with local fishers 
                                to find practical solutions to reduce discards,
                                alongside scientific modelling on the impacts of 
                                discarding practices and solutions on marine ecosystems.</h4><hr>
                                <p><strong>CSX partner:</strong> Institute One (I1) and Organization Two (O2)</p>
<p><strong>Lead scientist:</strong> Dr. Famous (I1) and Mr. Adviser (O2)</p>
                                <a href=\"http://minouw-project.eu/case-study-1-5-bottom-trawling-in-sicily-italy/\">Read more on minouw-project.eu</a>")),
                                   box(status = "primary", width = 9,
                                       leafletOutput("mainmap"))
                          ),
                          hr(),
                          h1("The Environment"),
                          hr(),
                          fluidRow(column(width = 4,
                                          HTML("<h4 align='justify'>This paragraph
                                               is useful to provide some contextual information of the 
                                               case study. Specifically, the map at the top of the page
                                               is used to show the area of interest, the one on the right
                                               of this text is used to display the bathymetric profile.
                                               Every map is dynamic and it is possible to select three
                                               different background styles.</h4>")),
                                   box(width = 8, leafletOutput("mapBathy"))
                          ),
                          fluidRow(column(width = 4,
                                          HTML("<h4 align='justify'>This text box is also used to
                                               provide further information about the fishery and the
                                               environment in which it works. The map directly on the 
                                               right is used to show other spatialized layer such as
                                               seabed substrates, depth zones, geopolitical borders etc.</h4>")),
                                   box(width = 8, leafletOutput("mapSeabed"),
                                       wellPanel(selectInput(inputId = "flooLaye",
                                                             label = "Show", 
                                                             choices = c("Zones",
                                                                         "EEZ")),
                                                 style = "padding: 5px;")
                                   )
                          ),
                          hr(),
                          h1("The Fleet"),
                          hr(),
                          fluidRow(column(width = 6,
                                          HTML("<h4 align='justify'>This box is mainly destined to the 
                                               description of the fishing fleet and to its characteristics.
                                               Different structural features can be described along with the
                                               graphical support of the images on the right side.</h4>")),
                                   column(width = 6, plotOutput("fleetLoa"), style='height:170px')),
                          fluidRow(column(width = 6,
                                          HTML("<h4 align='justify'>Another space to add text.</h4>")),
                                   column(width = 6, plotOutput("fleetKw"), style='height:170px')),
                          fluidRow(column(width = 6,
                                          HTML("<h4 align='justify'>Another space to add text.</h4>")),
                                   column(width = 6, plotOutput("fleetMainGear"), style='height:170px')),
                          fluidRow(column(width = 6,
                                          HTML("<h4 align='justify'>Another space to add text.</h4>")),
                                   column(width = 6, plotOutput("fleetSecGear"), style='height:170px'))
                          
                  ),  # Effort Tab ####
                  tabItem(tabName = "effortData",
                          leafletOutput("mapEffort"),
                          absolutePanel(id = "tabEffort", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 350, left = 250, right = "auto", bottom = "auto",
                                        width = 330, height = "auto",
                                        style = "padding: 20px; margin: 1%;",
                                        h3("Fishing Effort Explorer"),
                                        selectInput(inputId = "effoLaye",
                                                    label = "Show", 
                                                    choices = c("Swept Area",
                                                                "Fishing Days",
                                                                "Fishing Hours", 
                                                                "Nominal Effort")),
                                        sliderTextInput(inputId = "effoObs", label = "Time:",
                                                        choices = timeframe,
                                                        selected = timeframe[1],
                                                        width = "100%",
                                                        animate = animationOptions(interval = 1500, loop = TRUE)),
                                        h4("Download Shapefiles"),
                                        div(downloadButton(outputId = "downloadEffo",
                                                         label = "Quarter"),
                                            downloadButton(outputId = "downloadEffoYear",
                                                           label = "Year"),
                                            style="text-align: center;")
                          )
                  ),  # Discard Tab ####
                  tabItem(tabName = "discardData",
                          leafletOutput("uwcMap")
                  ),  # Fuzzy Tab ####
                  tabItem(tabName = "fuzzyData",
                          leafletOutput("mapFuzz"),
                          absolutePanel(id = "tabFuzz", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 300, left = 250, right = "auto", bottom = "auto",
                                        width = 330, height = "auto",
                                        style = "padding: 20px; margin: 1%;",
                                        h3("Fuzzy Overlay Explorer"),
                                        sliderTextInput(inputId = "fuzzObs", label = "Time:",
                                                        choices = timeframe,
                                                        selected = timeframe[1],
                                                        width = "100%",
                                                        animate = animationOptions(interval = 1500, loop = TRUE)),
                                        selectInput(inputId = "fuzzEffo",
                                                    label = "Show", 
                                                    choices = c("Swept Area",
                                                                "Fishing Days",
                                                                "Fishing Hours", 
                                                                "Nominal Effort")),
                                        selectInput(inputId = "fuzzSpec",
                                                    label = "Show", 
                                                    choices = c("SKJ",
                                                                "TTL")),
                                        h4("Download Shapefiles"),
                                        div(downloadButton(outputId = "downloadFuzz",
                                                         label = "Quarter"),
                                            downloadButton(outputId = "downloadFuzzYear",
                                                           label = "Year"),
                                            style="text-align: center;")
                          )
                  ),  # Overlap Tab ####
                  tabItem(tabName = "overlap", style = "padding: 10px;",
                          h1("Overlap Analysis"),
                          hr(),
                          h3("Select Effort Threshold"),
                          fluidRow(column(width = 6, align = "center",
                                          fluidRow(column(width = 6, align = "center",
                                                          selectInput(inputId = "effMet",
                                                                      label = "Measure:",
                                                                      choices = c("Swept Area",
                                                                                  "Fishing Days",
                                                                                  "Fishing Hours",
                                                                                  "Nominal Effort"),
                                                                      selected = "Swept Area",
                                                                      multiple = FALSE,
                                                                      selectize = FALSE)),
                                                   column(width = 6, align = "center",
                                                          sliderInput("effoThre", "Effort threshold:",
                                                                      min = 0, max = pretty(max(effoAgg$SwepArea$value[effoAgg$SwepArea$variable == "AvgTot"]))[2],
                                                                      value = c(1, 10), step = 0.1))),
                                          plotOutput("effoDist", height = "150px"),
                                          plotOutput("effoBoxp", height = "150px")),
                                   column(width = 6, leafletOutput("mapEffThr"))),
                          hr(),
                          h3("Select UWC Threshold"),
                          fluidRow(column(width = 6, align = "center",
                                          selectInput(inputId = "uwcSpe",
                                                      label = "Species:",
                                                      choices = specieName,
                                                      selected = "SKJ",
                                                      multiple = FALSE,
                                                      selectize = FALSE),
                                          sliderInput(inputId = "uwcThre",
                                                      label = "UWC threshold:",
                                                      min = 0, max = 3,
                                                      value = c(1,2), step = 1)),
                                   column(width = 6, align = "center",
                                          leafletOutput("mapUWCThr"))),
                          hr(),
                          fluidRow(column(width = 12, align = "center",
                                          actionBttn(inputId = "getOverlap",
                                                     label = "Get Overlap",
                                                     style = "unite",
                                                     size = "lg",
                                                     color = "primary"))),
                          hr(),
                          fluidRow(column(width = 6, leafletOutput("mapRisk")),
                                   column(width = 6,
                                          div(style = 'overflow-x: scroll',
                                              DT::dataTableOutput("OLtable")))),
                          hr(),
                          fluidRow(column(width = 12, align = "center",
                                          downloadButton(outputId = "downloadOver",
                                                         label = "Download Shapefile")))
                  ), # Wordbook ####
                  tabItem(tabName = "infoBook", style = "padding: 10px;",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Measures of Effort",
                                               br(),
                                               p(align="justify",
                                                 "The ‘Data Collection’ has been the first step of the Task 5.1.
                                                  The raw data from the two main tracking devices, the AIS 
                                                  (Automatic Identification System) and the VMS (Vessel 
                                                  Monitoring System) has been collected and organized 
                                                  into databases. The spatio-temporal information has been 
                                                  processed following the standard analysis routines provided
                                                  by the vmsbase R package (Russo et al., 2014). The main step
                                                  were 'Raw Data Merging and Cleaning', 'Track Cutting and
                                                  Interpolation' and 'Fishing Points Identification'. The
                                                  Fishing Points Identification provides the first way to 
                                                  assess the fishing effort deployed by each vessel as the 
                                                  product of the number of fishing pings by the temporal 
                                                  frequency of pings and, if summed for all the vessels 
                                                  operating in a given area and temporal range, it allows 
                                                  assessing the fishing footprint of the fleet. However, 
                                                  the fishing power of different vessels varies according to
                                                  their characteristics (Russo et al., 2017)."),
                                               p(align="justify", "In order to take into account these differences, 
                                                 four different measures of the fishing effort were computed
                                                 for each fishing trip, and used to generate the aggregated
                                                 pattern of fishing effort for the whole fleets of the 
                                                 different case studies, namely:"),
                                               p(align="justify", strong("Fishing Hours:"), "Directly computed from the fishing time 
                                                 as the product of the number of fishing pings by the
                                                 temporal frequency of pings (10 minutes)."),
                                               p(align="justify", strong("Fishing Days:"), "The number of fishing days of each vessel for each month
                                                 was computed using the rationale of the R package fecR
                                                 (Scott et al., 2017). Basically, in case of fishing trips
                                                 within which a single gear is used, each date with any
                                                 fishing activity is allocated to 1 fishing day"),
                                               p(align="justify", strong("Swept Area:"), "The Swept Area was computed, only for the case studies involving
                                                 towed gears (trawling) according to Eigaard et al. (2016)
                                                 and Russo et al. (2017). For a given haul was computed as follow:",
                                                 strong(withMathJax("$$SWA = OFS × TL$$")),
                                                 "Where OFS is the overall footprint size of the trawling net
                                                 (in meters) and TL is the length of the haul (in meters).
                                                 While TL can be directly computed from interpolated pings 
                                                 corresponding to Fishing Points, the OFS was estimated, for 
                                                 each fishing vessel, using the following formula:",
                                                 strong(withMathJax("$$OFS =a × LOA+b$$")),
                                                 "Where the value of the terms a and b were selected, for 
                                                 each case studies, from the Table 4 of Eigaard et al. (2016)."),
                                               p(align="justify", strong("Nominal Effort:"), "The Nominal Effort was computed as the product of the fishing
                                                 days and the engine power (in Kw) of each vessel."),
                                               p(align="justify", "The fishing effort computed for each fishing tracks, as 
                                                 Fishing Days, Fishing Hours, Nominal Effort and Swept Area,
                                                 was aggregated at a monthly scale with respect to the grids.")),
                                      tabPanel("Unwanted Catches",
                                               br(),
                                               tabsetPanel(type = "tabs",
                                                           tabPanel("SKJ",
                                                                    tags$iframe(style="height:600px; width:100%",
                                                                                src="FAO_species_pdf/FAO Fact Sheets - Katsuwonus pelamis (Linnaeus, 1758).pdf")),
                                                           tabPanel("TTL",
                                                                    tags$iframe(style="height:600px; width:100%",
                                                                                src="FAO_species_pdf/FAO Fact Sheets - Caretta caretta (Linnaeus, 1758).pdf"))
                                               )),
                                      tabPanel("Fuzzy Overlay",
                                               br(),
                                               p(align="justify",
                                                 "The spatial distribution of the combination of both fishing effort
                                                 and unwanted catches was estimated through a fuzzy logic technique
                                                 by taking into consideration two types of layers: (1) the
                                                 spatiotemporal distribution of fishing effort and (2) the spatial
                                                 distribution of unwanted catches by each species under investigation.
                                                 The use of the fuzzy logic technique has started gaining recognition
                                                 in fisheries science in the last decades (e.g. Stelzenmuller et al.,
                                                 2010; Kavadas et al., 2015). Fuzzy logic represents an extension of
                                                 the classic binary logic, with the possibility of defining sets 
                                                 without clear boundaries of elements belonging to a given set 
                                                 (Malczewki, 1999; Biacino & Gerla, 2002). In particular, a Fuzzy
                                                 Linear membership function was used to transform the values of each 
                                                 type of layer to a 0 to 1 possibility continuous scale. Linear 
                                                 transformations are commonly used in fuzzification of deterministic 
                                                 criteria (Fisher, 2000). The Fuzzy Linear transformation applies a 
                                                 linear function between the specified minimum and maximum values and 
                                                 is estimated by the formula:",
                                                 strong(withMathJax("$$\\mu(x) = 0 \\text{ if } x < min$$")),
                                                 strong(withMathJax("$$\\mu(x) = 1 \\text{ if } x > max$$")),
                                                 strong(withMathJax("$$\\text{otherwise } \\mu(x) = \\frac{x - min}{max - min}$$")),
                                                 p(align="justify",
                                                   "where x is the value in each cell (planning unit) and min, 
                                                 max are the specified minimum and maximum values."),
                                                 p(align="justify",
                                                   "The spatial distribution of the combination of both fishing effort
                                                   and unwanted catches was derived as the fuzzy overlay of the two 
                                                   previously transformed (using Fuzzy Linear membership function) types 
                                                   of layers. Generally, there are several techniques used in a fuzzy 
                                                   overlay analysis, for investigating the relationships and quantifying 
                                                   the interactions. The combination approach used in this work was the 
                                                   Fuzzy Product (Zimmermann & Zysno, 1980). The Fuzzy Product overlay 
                                                   type will, for each cell, multiply each of the fuzzy values for all 
                                                   the input criteria. Values close to 1 indicate areas with an elevated 
                                                   likelihood of intense fishing effort and unwanted catches (at the same 
                                                   time).")
                                               )),
                                      tabPanel("Overlap Analysis",
                                               br(),
                                               p(align="justify",
                                                 "The overlap analysis tool offers the possibility to perform some typical GIS operations.
                                                  simplifying, it is possible to choose a threshold for one of the different 
                                                  effort measures present within the fishing effort dataset, to select the 
                                                  corresponding grid cells with values higher than the selected threshold, and 
                                                  combine them, making a spatial union, with the polygons that identify areas with unwanted catches.
                                                 "),
                                               p(align="justify",
                                                 "Practically, the procedures employed consist of:"),
                                               tags$ul(
                                                 tags$li("over {sp} for the spatial overlay between the effort grids and the polygons 
                                                          of unwated catches
                                                          areas. The overlay is performed with the over method of the sp package. The
                                                          method can be seen as a left outer join in SQL and the match is a spatial
                                                          intersection;"), 
                                                 tags$li("gIntersection {rgeos} Function for determining the intersection between two 
                                                          given geometries;"), 
                                                 tags$li("spTransform {sp} for map projection and datum transformation;"),
                                                 tags$li("extract {raster} Extract values from a spatial object at the locations of other 
                                                          spatial data. The extract method returns the values of 
                                                         the cells of the underlying object that are covered by another polygon.
                                                         The extrated values are then aggregated by summarizing functions (e.g. min, mean, max...)")
                                               ),
                                               p(align="justify",
                                                   "The resulting output, after clicking the get overlap button, will be a variable number 
                                                   of polygons derived from the combination of selected thresholds and overlap between the 
                                                   fishing effort datasets and areas with unwanted catches.")
                                               ))
                  ), # References ####
                  tabItem(tabName = "refere", style = "padding: 10px;",
                          h2("Credits"),
                          br(),
                          p(align="justify", "This application has been developed by Lorenzo D'Andrea for the Minouw Project
                            at the 'Models for Fisheries' Laboratory, Department of Biology of the University
                            of Rome Tor Vergata with the support of the grant: 'SVILUPPO DELLA PIATTAFORMA
                            WEB GIS E IMPLEMENTAZIONE BASE DATI PER IL PROGRAMMA H2020 MINOUW [Dipartimento
                            di Biologia, BIO/07]'"),
                          p(align="justify", "According to the AMENDMENT Reference No AMD-634495-25 - Ref. Ares(2017)3709074
                            of the 24/07/2017 about the Grant Agreement number: 634495 — Science,
                            Technology, and Society Initiative to minimize Unwanted Catches in European
                            Fisheries (MINOUW), and with particular reference to the activity planned
                            within the Working Package 5 (Control and monitoring), the Department of
                            Biology of the University of Rome Tor Vergata (UTV hereafter) will carry out
                            the specific activity described for Task 5.1 of the abovementioned WP5 
                            related to VMS/AIS data acquisition, GIS database and statistical
                            treatment, as well as contribute to the implementation of the GIS user
                            interface for exploitation of such data internally in the project or by
                            external interested parties (fishers; fisheries managers). The team dedicated
                            by UTV to MINOUW will be experts on the spatial analysis of fisheries
                            footprint led by researcher Dr. Tommaso Russo."),
                          br(),
                          p("Russo, T., D’Andrea, L., Erzini, K., Fonseca, P., Maynou, F.,
                            Demestre, M., Sbrana, M., Viva, C., Vitale, S., Fiorentino, F.,
                            Kavadas, S., 2018. Deliverable 5.9 - Spatial estimates of 
                            Fisheries footprint."),
                          a(href = "http://minouw-project.eu", "minouw-project.eu"),
                          br(), br(),
                          h4("Fishing Footprint"),
                          p("Eigaard, O.R., Bastardie, F., Breen, M., Dinesen, G.E., Hintzen,
                            N.T., Laffargue, P., Mortensen, L.O., Nielsen, J.R., Nilsson, H.C.,
                            Neill, F.G.O., Smith, C., Sørensen, T.K., Polet, H., Reid, D.G.,
                            Sala, A., Sko, M., Tully, O., Zengin, M., Rijnsdorp, A.D., 2016.
                            Estimating seabed pressure from demersal trawls, seines, and dredges
                            based on gear design and dimensions. ICES J. Mar. Sci. 73, i27-i43."),
                          p("Russo, T., Morello, E.B., Parisi, A., Scarcella, G., Angelini, S., Labanchi, L.,
                            Martinelli, M., D’Andrea, L., Santojanni, A., Arneri, E., Cataudella, S., 2017.
                            A model combining landings and VMS data to estimate landings by
                            fishing ground and harbor. Fisheries Research 199, 218–230."),
                          p("Russo, T., D’Andrea, L., Parisi, A., Cataudella, S., 2014.
                            VMSbase: An R-Package for VMS and Logbook Data Management
                            and Analysis in Fisheries Ecology. PLOS ONE 9."),
                          p("Scott F, Prista N, Reilly T (2017). fecR: Calculating fishing effort.
                            Available at:  https://cran.r-project.org/web/packages/fecR/vignettes/calculating_fishing_effort.pdf"),
                          br(),
                          h4("Fuzzy Overlay"),
                          p("Biacino, L. Gerla, G. 2002. Fuzzy logic, continuity and effectiveness. Archive for Mathematical Logic 41 (7): 643–667. doi:10.1007/s001530100128. ISSN 0933-5846.
                            Fisher, P. 2000. Fuzzy modelling. Taylor & Francis, London, 459 pp."),
                          p("Maina, I., Kavadas, S., Katsanevakis, S., Somarakis,
                             S., Tserpes, G., Georgakarakos, S., 2016.
                             A methodological approach to identify fishing grounds:
                             A case study on Greek trawlers. Fisheries Research 183, 326-339."),
                          p("Maina, I., Kavadas, S., Machias, A., Tsagarakis, K.,
                             Giannoulaki, M., 2018. Modelling the spatiotemporal distribution
                             of fisheries discards. Journal of Sea Research 139, 10-23."),
                          p("Malczewski, J. 1999. GIS and multicriteria decision 
                            analysis. Wiley & Sons, New York, 393 pp."),
                          p("Kavadas, S. Maina, I., Damalas, D., Dokos,
                             I., Pantazi, M., Vassilopoulou, V., 2015. Multi-Criteria
                             Decision Analysis as a tool to extract fishing footprints
                             and estimate fishing pressure. Mediterranean Marine Science 16."),
                          p("Stelzenmüller, V., Lee, J., Rogers, S.I., 2010. 
                            Quantifying cumulative impacts of human pressures on 
                            the marine environment: A geospatial modelling framework.
                            Marine Ecology Progress Series, 398, 19-32."),
                          p("Zimmermann H.J., Zysno P. 1980. Fuzzy Set Theory and
                            Its Application,” Kluwer-Nijhoff Publishing, Boston,
                            Dor- drecht, Lancaster, 363 pp."),
                          br(),
                          h4("Additional Layers"),
                          p(strong("Bathimetry: "), "Pante E, Simon-Bouhet B (2013) marmap: A Package for
                            Importing, Plotting and Analyzing Bathymetric and 
                            Topographic Data in R. PLoS ONE 8(9)"),
                          p(strong("Seabed Floor: "), "Information contained here
                            has been derived from data that is made available under
                            the European Marine Observation Data Network (EMODnet) 
                            Seabed Habitats project (http://www.emodnet-seabedhabitats.eu/), 
                            funded by the European Commission’s Directorate-General for 
                            Maritime Affairs and Fisheries (DG MARE)."),
                          HTML("<strong>OSM (default): </strong> by &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>"),
                          HTML("<br/><strong>Esri World: </strong> by &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"),
                          HTML("<br/><strong>Toner Lite: </strong> by <a href='http://stamen.com'>Stamen Design</a>,
<a href='http://creativecommons.org/licenses/by/3.0'>CC BY 3.0</a> &mdash;
                               Map data &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>")
                  )
      )
    ),
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
                              background-color: #043c6d;
                              }
                              .skin-blue .main-header .logo:hover {
                              background-color: #043c6d;
                              }
/* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #004987;
                              }        
/* toggle button when hovered  */                    
        .skin-blue .main-header .navbar .sidebar-toggle:hover {
                              background-color: #043c6d;
                              }
/* menuitem left border */
        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li:hover > a {
                              border-left-color: #ffc321;
                              }
/* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #043c6d;
                              }
        div.outer {
                              position: fixed;
                              top: 50px;
                              left: 0;
                              right: 0;
                              bottom: 0;
                              overflow: hidden;
                              padding: 0;
                              }
                              ')))
  )
)


## SERVER ####

server <- function(input, output, session) {
  output$mainmap <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = areaPoly, weight = 2, color = "#D4A5D4D1",
                  popup = includeHTML(csShortDesc), opacity = 0.95,
                  highlightOptions = highlightOptions(opacity = 1, weight = 5),
                  group = "Area of Study") %>%
      setView(lng = mapCoo[1], lat = mapCoo[2], zoom = 9) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri World", "Toner Lite"),
        overlayGroups = "Area of Study",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  ## Effort Map ####
  output$mapEffort <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = areaPoly, weight = 2, color = "#D4A5D4D1",
                  group = "Area of Study", fill = FALSE) %>%
      addPolygons(data = areaGrid, weight = 2, stroke = FALSE, layerId = paste0("1_", 1:nrow(areaGrid@data)),
                  fillColor = pal_swar(log10(1 + effoData$SwepArea[, 1])),
                  fillOpacity = 0.75, group = "Fishing Effort") %>%
      addLegend(colors = pal_swar(quantile(c(0, ceiling(max(log10(1 + effoData$SwepArea)))))),
                labels = round_any(quantile(c(0, ceiling(max(effoData$SwepArea)))), 10),
                position = "bottomright", title = "Km^2") %>%
      setView(lng = mapCoo[1], lat = mapCoo[2], zoom = 9) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri World", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  ## Effort input change observer
  selEffoData <- reactive({
    return(switch(EXPR = input$effoLaye,
                  'Swept Area' = "SwepArea",
                  'Fishing Days' = "FishDays",
                  'Fishing Hours' = "FishHour",
                  'Nominal Effort' = "NomiEffo"))
  })
  selEffopal <- reactive({
    return(switch(EXPR = input$effoLaye,
                  'Swept Area' = pal_swar,
                  'Fishing Days' = pal_fida,
                  'Fishing Hours' = pal_fiho,
                  'Nominal Effort' = pal_noef))
  })
  observeEvent(input$effoObs, {
    effoTime$Previous <- effoTime$Current
    effoTime$Current <- which(timeframe == input$effoObs)
  })
  observe({
    curEffoSet <- selEffoData()
    selEffoPal <- selEffopal()
    
    curEffoData <- data.frame(effoData[[curEffoSet]][, effoTime$Current])
    curPalCol <- selEffoPal(quantile(c(0, ceiling(max(log10(1 + effoData[[curEffoSet]]))))))
    curPalAcc <- switch(EXPR = curEffoSet,
                        "SwepArea" = 10,
                        "FishDays" = 1,
                        "FishHour" = 10,
                        "NomiEffo" = 1000)
    curPalLab <- round_any(x = quantile(c(0, ceiling(max(effoData[[curEffoSet]])))),
                           accuracy = curPalAcc)
    curUnit <- switch(EXPR = curEffoSet,
                      "SwepArea" = "Km^2",
                      "FishDays" = "Days",
                      "FishHour" = "Hours",
                      "NomiEffo" = "Kw/Days")
    
    leafletProxy("mapEffort") %>%
      addPolygons(data = areaGrid, weight = 2, stroke = FALSE,
                  layerId = paste0(effoTime$Current, "_", 1:nrow(areaGrid@data)),
                  fillColor = selEffoPal(log10(1 + curEffoData[,1])),
                  fillOpacity = 0.75) %>%
      clearControls() %>%
      addLegend(colors = curPalCol,
                labels = curPalLab,
                position = "bottomright",
                title = curUnit) %>%
      removeShape(layerId = paste0(effoTime$Previous, "_", 1:nrow(areaGrid@data)))
  })
  
  
  ## Fuzz Map ####
  output$mapFuzz <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = areaGrid, weight = 2, stroke = FALSE, layerId = paste0("1_", 1:nrow(areaGrid@data)),
                  fillColor = pal_fuzz(skjFuzz$SwepArea[, 1]),
                  fillOpacity = 0.75, group = "Fishing Effort") %>%
      addLegend(colors = pal_fuzz(c(0.1, 0.25, 0.5, 0.75, 1)),
                labels = c(0.1, 0.25, 0.5, 0.75, 1),
                position = "bottomright") %>%
      setView(lng = mapCoo[1], lat = mapCoo[2], zoom = 9) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri World", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  ## Fuzz input change observer
  selFuzzData <- reactive({
    return(switch(EXPR = input$fuzzEffo,
                  'Swept Area' = "SwepArea",
                  'Fishing Days' = "FishDays",
                  'Fishing Hours' = "FishHour",
                  'Nominal Effort' = "NomiEffo"))
  })
  selFuzzSpecie <- reactive({
    return(switch(EXPR = input$fuzzSpec,
                  'SKJ' = "skjFuzz",
                  'TTL' = "ttlFuzz"))
  })
  selFuzzYear <- reactive({
    return(which(timeframe == input$fuzzObs))
  })
  observeEvent(input$fuzzObs, {
    fuzzTime$Previous <- fuzzTime$Current
    fuzzTime$Current <- which(timeframe == input$fuzzObs)
  })
  
  observe({
    curFuzzSet <- selFuzzData()
    curFuzzSpe <- selFuzzSpecie()
    tmpDat <- get(curFuzzSpe)
    curEffoData <- data.frame(tmpDat[[curFuzzSet]][, fuzzTime$Current])
    leafletProxy("mapFuzz") %>%
      addPolygons(data = areaGrid, weight = 2, stroke = FALSE,
                  layerId = paste0(fuzzTime$Current, "_", 1:nrow(areaGrid@data)),
                  fillColor = pal_fuzz(curEffoData[,1]),
                  fillOpacity = 0.75) %>%
      clearControls() %>%
      addLegend(colors = pal_fuzz(c(0.1, 0.25, 0.5, 0.75, 1)),
                labels = c(0.1, 0.25, 0.5, 0.75, 1),
                position = "bottomright") %>%
      removeShape(layerId = paste0(fuzzTime$Previous, "_", 1:nrow(areaGrid@data)))
  })
  
  
  ## Map Depth ####
  output$mapBathy <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(noWrap = TRUE)) %>%
      addRasterImage(x = areaDept, colors = colorNumeric("Blues", reverse = TRUE, domain = NULL, na.color = NA),
                     opacity = 0.85, group = "Bathymetry") %>%
      addLegend(pal = colorNumeric("Blues", reverse = TRUE,
                                   domain = NULL, na.color = NA),
                values = pretty(areaDept@data@values),
                position = "bottomright") %>%
      addProviderTiles("Stamen.TonerLabels") %>%
      setView(lng = mapCoo[1], lat = mapCoo[2], zoom = 9) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri World", "Toner Lite"),
        overlayGroups = "Bathymetry",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$downloadBathy <- downloadHandler(
    filename = function() {
      paste0(csName, "_areaDept.rds")
    },
    content = function(file) {
      saveRDS(object = areaDept, file = file)
    }
  )
  
  output$downloadEffo <- downloadHandler(
    filename = function() {
      paste0("effortGrid_", 
             gsub(pattern = " ", replacement = "", x = input$effoLaye),
             "Quarter.zip")
    },
    content = function(file) {
      file.copy(paste0("www/shpFiles/", selEffoData(), ".zip"), file)
    },
    contentType = "application/zip"
  )
  output$downloadEffoYear <- downloadHandler(
    filename = function() {
      paste0("effortGrid_", 
             gsub(pattern = " ", replacement = "", x = input$effoLaye),
             "Year.zip")
    },
    content = function(file) {
      file.copy(paste0("www/shpFiles/", selEffoData(), "Year.zip"), file)
    },
    contentType = "application/zip"
  )
  
  output$downloadFuzz <- downloadHandler(
    filename = function() {
      paste0("fuzzyGrid_", 
             tolower(input$fuzzSpec), 
             gsub(pattern = " ", replacement = "", x = input$fuzzEffo),
             "Quarter.zip")
    },
    content = function(file) {
      file.copy(paste0("www/shpFiles/", tolower(input$fuzzSpec),
                       selFuzzData(),".zip"), file)
    },
    contentType = "application/zip"
  )
  output$downloadFuzzYear <- downloadHandler(
    filename = function() {
      paste0("fuzzyGrid_", 
             tolower(input$fuzzSpec), 
             gsub(pattern = " ", replacement = "", x = input$fuzzEffo),
             "Year.zip")
    },
    content = function(file) {
      file.copy(paste0("www/shpFiles/", tolower(input$fuzzSpec),
                       selFuzzData(),"Year.zip"), file)
    },
    contentType = "application/zip"
  )
  
  
  ## Map SeaFloor ####
  output$mapSeabed <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = mapCoo[1], lat = mapCoo[2], zoom = 9) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri World", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  observe({
    if (input$flooLaye == "Zones") {
      leafletProxy("mapSeabed") %>%
        clearControls() %>%
        clearShapes() %>%
        addLegend(pal = colorFactor("Set3", areaFloo@data$Zones),
                  values = unique(areaFloo@data$Zones),
                  position = "bottomleft") %>%
        addPolygons(data = areaFloo, weight = 2, stroke = FALSE, fillColor = pal_biozon,
                    fillOpacity = 0.75,
                    label = as.character(areaFloo@data$Zones))
    } 
    else if (input$flooLaye == "EEZ") {
      leafletProxy("mapSeabed") %>%
        clearControls() %>%
        clearShapes() %>%
        addLegend(pal = colorFactor("Set3", areaFloo@data$EEZ),
                  values = unique(areaFloo@data$EEZ),
                  position = "bottomleft") %>%
        addPolygons(data = areaFloo, weight = 2, stroke = FALSE, fillColor = pal_substr,
                    fillOpacity = 0.75,
                    label = as.character(areaFloo@data$EEZ))
    }
  })
  
  
  ## Fleet Register ####
  output$fleetReg <- DT::renderDataTable({
    DT::datatable(data = fleetReg, options = list(lengthMenu = c(5, 10, 25), pageLength = 5))
  })
  
  output$fleetLoa <- renderPlot(ggplot() +
                                  geom_histogram(data = fleetReg,
                                                 mapping = aes(Loa),
                                                 bins = 15) +
                                  theme_tufte(base_size = 14, ticks = F) +
                                  ggtitle("Length Over All") +
                                  theme(legend.position = "none",
                                        plot.title = element_text(size = 22, hjust = 0.5),
                                        axis.text.x = element_text(size = 12),
                                        axis.title = element_blank(),
                                        panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                        axis.text.y = element_text(size = 12),
                                        axis.ticks.y = element_blank()), height = 150, units = "px")
  
  output$fleetKw <- renderPlot(ggplot() +
                                 geom_histogram(data = fleetReg,
                                                mapping = aes(Power.Main),
                                                bins = 15) +
                                 theme_tufte(base_size = 14, ticks = F) +
                                 ggtitle("Engine Power") +
                                 theme(legend.position = "none",
                                       plot.title = element_text(size = 22, hjust = 0.5),
                                       axis.text.x = element_text(size = 12),
                                       axis.title = element_blank(),
                                       panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                       axis.text.y = element_text(size = 12),
                                       axis.ticks.y = element_blank()), height = 150, units = "px")
  
  output$fleetMainGear <- renderPlot(ggplot() +
                                       geom_bar(data = fleetReg,
                                                mapping = aes(Gear.Main.Code)) +
                                       theme_tufte(base_size = 14, ticks = F) +
                                       ggtitle("Main Gear") +
                                       theme(legend.position = "none",
                                             plot.title = element_text(size = 22, hjust = 0.5),
                                             axis.text.x = element_text(size = 12),
                                             axis.title = element_blank(),
                                             panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                             axis.text.y = element_text(size = 12),
                                             axis.ticks.y = element_blank()), height = 150, units = "px")
  
  output$fleetSecGear <- renderPlot(ggplot() +
                                      geom_bar(data = fleetReg,
                                               mapping = aes(Gear.Sec.Code)) +
                                      theme_tufte(base_size = 14, ticks = F) +
                                      ggtitle("Secondary Gear") +
                                      theme(legend.position = "none",
                                            plot.title = element_text(size = 22, hjust = 0.5),
                                            axis.text.x = element_text(size = 12),
                                            axis.title = element_blank(),
                                            panel.grid = element_line(size = 0.5, linetype = 2, colour = "grey20"),
                                            axis.text.y = element_text(size = 12),
                                            axis.ticks.y = element_blank()), height = 150, units = "px")
  
  selAggData <- reactive({
    outMetr <- switch(EXPR = input$effMet,
                      'Swept Area' = "SwepArea",
                      'Fishing Days' = "FishDays",
                      'Fishing Hours' = "FishHour",
                      'Nominal Effort' = "NomiEffo")
    threValu$value <- 0
    if(outMetr == "SwepArea"){
      updateSliderInput(session, "effoThre", value = c(1, 10),
                        min = 0, max = pretty(quantile(x = effoAgg$SwepArea$value, 0.99))[2],
                        step = 0.1)
    } else if (outMetr == "FishDays") {
      updateSliderInput(session, "effoThre", value = c(1, 10),
                        min = 0, max = pretty(quantile(x = effoAgg$FishDays$value, 0.99))[2],
                        step = 0.1)
    } else if (outMetr == "FishHour") {
      updateSliderInput(session, "effoThre", value = c(1, 10),
                        min = 0, max = pretty(quantile(x = effoAgg$FishHour$value, 0.99))[2],
                        step = 0.1)
    } else if (outMetr == "NomiEffo") {
      updateSliderInput(session, "effoThre", value = c(1, 10),
                        min = 0, max = pretty(quantile(x = effoAgg$NomiEffo$value, 0.99))[2],
                        step = 1)
    }
    return(outMetr)
  })
  observe({
    aggMet <- selAggData()
    tmpAggEff <- effoAgg[[aggMet]][effoAgg[[aggMet]]$value <= quantile(x = effoAgg[[aggMet]]$value, 0.99) & effoAgg[[aggMet]]$value > 0,]
    output$effoDist <- renderPlot(ggplot() +
                                    geom_density(data = tmpAggEff,
                                                 mapping = aes(x = value, y = ..scaled..,
                                                               color = variable, fill = variable),
                                                 alpha = 0.1) +
                                    theme_tufte(base_size = 14, ticks = F) +
                                    ggtitle("Effort Distribution") +
                                    geom_vline(xintercept = threValu$value,
                                               linetype = "dashed",
                                               color = "firebrick",
                                               size = 2) +
                                    scale_fill_brewer(palette = "Pastel1") +
                                    scale_color_brewer(palette = "Pastel1") +
                                    theme(legend.position = "none",
                                          plot.title = element_text(size = 22, hjust = 0.5),
                                          axis.text.x = element_text(size = 12),
                                          axis.title = element_blank(),
                                          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
                                          axis.text.y = element_text(size = 12),
                                          axis.ticks.y = element_blank()), height = 150, units = "px")
    
    # EffoDist Boxplot ####
    output$effoBoxp <- renderPlot(ggplot() +
                                    geom_boxplot(data = tmpAggEff,
                                                 mapping = aes(y = value)) +
                                    theme_tufte(base_size = 14, ticks = F) +
                                    geom_hline(yintercept = threValu$value,
                                               linetype = "dashed",
                                               color = "firebrick",
                                               size = 2) +
                                    theme(legend.position = "none",
                                          plot.title = element_text(size = 22, hjust = 0.5),
                                          axis.text.x = element_text(size = 12),
                                          axis.title = element_blank(),
                                          panel.grid = element_line(size = 0.05, linetype = 2, colour = "grey20"),
                                          axis.text.y = element_text(size = 12),
                                          axis.ticks.y = element_blank()) +
                                    coord_flip(), height = 150, units = "px")
  })
  
  
  ## Effort Threshold observer ####
  observe({
    threValu$value <- input$effoThre
  })
  selThrAvg <- reactive({
    curAvgSet <- selAggData()
    outAvg <- effoAgg[[curAvgSet]][effoAgg[[curAvgSet]]$variable == "AvgTot", "value"]
    outAvg[outAvg >= max(threValu$value) | outAvg <= min(threValu$value)] <- NA
    return(outAvg)
  })
  selAvgpal <- reactive({
    return(switch(EXPR = input$effMet,
                  'Swept Area' = pal_swar,
                  'Fishing Days' = pal_fida,
                  'Fishing Hours' = pal_fiho,
                  'Nominal Effort' = pal_noef))
  })
  observe({
    curAvg <- selThrAvg()
    selAvgPal <- selAvgpal()
    curPalCol <- selAvgPal(quantile(c(0, ceiling(max(log10(1 + curAvg), na.rm = TRUE)))))
    curPalAcc <- switch(EXPR = input$effMet,
                        "Swept Area" = 1,
                        "Fishing Days" = 1,
                        "Fishing Hours" = 1,
                        "Nominal Effort" = 100)
    curPalLab <- round_any(x = quantile(c(0, ceiling(max(curAvg, na.rm = TRUE)))),
                           accuracy = curPalAcc)
    leafletProxy("mapEffThr") %>%
      removeShape(layerId = paste0("effoAvg", 1:nrow(areaGrid@data))) %>%
      addPolygons(data = areaGrid, weight = 2, stroke = FALSE,
                  layerId = paste0("effoAvg", 1:nrow(areaGrid@data)),
                  fillColor = selAvgPal(log10(1 + curAvg)),
                  fillOpacity = 0.75, group = "Selected Effort") %>%
      clearControls() %>%
      addLegend(colors = curPalCol,
                labels = curPalLab,
                position = "bottomright")
  })
  
  
  ## Effort Threshold Map ####
  output$mapEffThr <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = areaGrid, weight = 2, stroke = FALSE, layerId = paste0("effoAvg", 1:nrow(areaGrid@data)),
                  fillColor = pal_swar(log10(1 + effoAgg$SwepArea[effoAgg$SwepArea$variable == "AvgTot", "value"])),
                  fillOpacity = 0.75, group = "Fishing Effort") %>%
      addLegend(colors = pal_swar(quantile(c(0, ceiling(max(log10(1 + effoAgg$SwepArea[effoAgg$SwepArea$variable == "AvgTot", "value"])))))),
                labels = round_any(quantile(c(0, ceiling(max(effoAgg$SwepArea[effoAgg$SwepArea$variable == "AvgTot", "value"])))), 1),
                position = "bottomright") %>%
      setView(lng = mapCoo[1], lat = mapCoo[2], zoom = 9) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri World", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  ## UWC Threshold observer ####
  selUWCpal <- reactive({
    return(switch(EXPR = input$uwcSpe,
                  SKJ = pal_uwc_skj,
                  TTL = pal_uwc_ttl))
  })
  observe({
    curSpe <- selUWCspe()
    curPal <- selUWCpal()
    outCol <- curPal(curSpe@data$GRIDCODE)
    outCol[!curSpe@data$GRIDCODE %in% seq(from = min(input$uwcThre), to = max(input$uwcThre), by = 1)] <- "#00000000"
    leafletProxy("mapUWCThr") %>%
      # removeShape(layerId = paste0("UWC", 1:length(leafletProxy("mapUWCThr")$x$calls[[2]]$args[[4]]))) %>%
      clearShapes() %>%
      addPolygons(data = curSpe, weight = 0.5, stroke = TRUE,
                  fillColor = outCol, group = "UWC",
                  color = curPal(0), fill = TRUE , fillOpacity = 0.5,
                  layerId = paste0("UWC", 1:nrow(curSpe@data)))
  })
  
  
  ## UWC Threshold Map ####
  output$mapUWCThr <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = areaPoly, weight = 2, color = "#D4A5D4D1",
                  group = "Area of Study", fill = FALSE) %>%
      addPolygons(data = skjMap, weight = 0.5, stroke = TRUE,
                  layerId = paste0("UWC", 1:nrow(skjMap@data)),
                  fillColor = pal_uwc_skj(skjMap@data$GRIDCODE),
                  color = pal_uwc_skj(0),
                  fillOpacity = 0.5, group = "UWC", fill = TRUE) %>%
      addLegend(pal = colorFactor("Greys", domain = 1:3),
                values = 1:3,
                position = "bottomright") %>%
      setView(lng = mapCoo[1], lat = mapCoo[2], zoom = 9) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri World", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  ## Map UWC ####
  output$uwcMap <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = areaPoly, weight = 2, color = "#D4A5D4D1",
                  group = "Area of Study", fill = FALSE) %>%
      addPolygons(data = skjMap, weight = 0.5, stroke = TRUE, fillColor = pal_uwc_skj(skjMap@data$GRIDCODE),
                  color = pal_uwc_skj(skjMap@data$GRIDCODE)[5], fillOpacity = 0.5, group = "SKJ", fill = TRUE) %>%
      addPolygons(data = ttlMap, weight = 0.5, stroke = TRUE, fillColor = pal_uwc_ttl(ttlMap@data$GRIDCODE),
                  color = pal_uwc_ttl(ttlMap@data$GRIDCODE)[5], fillOpacity = 0.5, group = "TTL", fill = TRUE) %>%
      addLegend(pal = colorFactor("Greys", domain = 1:3),
                values = 1:3,
                position = "bottomright") %>%
      setView(lng = mapCoo[1], lat = mapCoo[2], zoom = 9) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri World", "Toner Lite"),
        overlayGroups = c("SKJ", "TTL"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% hideGroup(c("TTL"))
  })
  
  
  ## Overlap observer ####
  selUWCspe <- reactive({
    return(switch(EXPR = input$uwcSpe,
                  SKJ = skjMap,
                  TTL = ttlMap))
  })
  selUWCpal <- reactive({
    return(switch(EXPR = input$uwcSpe,
                  SKJ = pal_uwc_skj,
                  TTL = pal_uwc_ttl))
  })
  
  observeEvent(input$getOverlap, {
    curSpe <- isolate(selUWCspe())
    curAvg <- isolate(selThrAvg())
    gridSel <- areaGrid[which(curAvg <= max(threValu$value) | curAvg >= min(threValu$value)),]
    speSel <- curSpe[curSpe@data$GRIDCODE %in% seq(from = min(input$uwcThre), to = max(input$uwcThre), by = 1),]
    polDisag <- gIntersection(gridSel,
                              speSel,
                              drop_lower_td = TRUE)
    outOL <- disaggregate(polDisag)
    
    if(!is.null(outOL)){
      OLdf <- SpatialPolygonsDataFrame(outOL, data.frame(id = 1:length(outOL),
                                                         Lon = round(coordinates(outOL)[,1],1),
                                                         Lat = round(coordinates(outOL)[,2],1)))
      leafletProxy("mapRisk") %>%
        removeShape(layerId = paste0("OL", 1:nrow(areaGrid@data))) %>%
        addPolygons(data = OLdf, weight = 0.5, stroke = TRUE,
                    fillColor = "Firebrick", group = "Overlap",
                    color = OLdf@data$id, fill = TRUE , fillOpacity = 0.75,
                    layerId = paste0("OL", 1:nrow(OLdf@data)),
                    label = as.character(OLdf@data$id)
        )
    } else {
      leafletProxy("mapRisk") %>%
        removeShape(layerId = paste0("OL", 1:nrow(areaGrid@data)))
    }
    
    if(!is.null(outOL)){
      outOLdf <- SpatialPolygonsDataFrame(outOL, data.frame(id = 1:length(outOL),
                                                            Lon = round(coordinates(outOL)[,1], 2),
                                                            Lat = round(coordinates(outOL)[,2], 2)))
      outOLproj <- spTransform(outOL, CRS("+proj=laea"))
      outOLdf@data <- cbind(outOLdf@data,
                            'Surface Area' = round(gArea(outOLproj, byid = TRUE)/(1000*1000), 2),
                            'Depth Low' = extract(areaDept, outOLdf, fun = min, na.rm = TRUE),
                            'Depth Med' = extract(areaDept, outOLdf, fun = median, na.rm = TRUE),
                            'Depth High' = extract(areaDept, outOLdf, fun = max, na.rm = TRUE))
      output$OLtable <- DT::renderDataTable(DT::datatable({outOLdf@data},
                                            options = list(searching = FALSE)))
    } else {
      output$OLtable <- DT::renderDataTable(DT::datatable({data.frame('Result' = "No polygons from intersection")},
                                            options = list(searching = FALSE)))
    }
    
  })
  
  output$downloadOver <- downloadHandler(
    filename = "overlapAnalysis.zip",
    content = function(file) {
      curSpe <- isolate(selUWCspe())
      curAvg <- isolate(selThrAvg())
      gridSel <- areaGrid[which(curAvg <= max(threValu$value) | curAvg >= min(threValu$value)),]
      speSel <- curSpe[curSpe@data$GRIDCODE %in% seq(from = min(input$uwcThre), to = max(input$uwcThre), by = 1),]
      polDisag <- gIntersection(gridSel,
                                speSel,
                                drop_lower_td = TRUE)
      outOL <- disaggregate(polDisag)
      
      outOLdf <- SpatialPolygonsDataFrame(outOL, data.frame(id = 1:length(outOL),
                                                            Lon = round(coordinates(outOL)[,1], 2),
                                                            Lat = round(coordinates(outOL)[,2], 2)))
      outOLproj <- spTransform(outOL, CRS("+proj=laea"))
      outOLdf@data <- cbind(outOLdf@data,
                            'Area' = round(gArea(outOLproj, byid = TRUE)/(1000*1000), 2),
                            'Dept_L' = extract(areaDept, outOLdf, fun = min, na.rm = TRUE),
                            'Dept_M' = extract(areaDept, outOLdf, fun = median, na.rm = TRUE),
                            'Dept_H' = extract(areaDept, outOLdf, fun = max, na.rm = TRUE))
      
      writeOGR(obj = outOLdf, dsn = "www/shpFiles/overOut", layer = "overlapOutput", driver = "ESRI Shapefile")
      zip(zipfile = file, files = paste0("www/shpFiles/overOut/", list.files(path = "www/shpFiles/overOut")))
      file.remove(paste0("www/shpFiles/overOut/", list.files(path = "www/shpFiles/overOut")))
      
      file.copy(file, file)
    },
    contentType = "application/zip"
  )
  
  
  ## Overlap Output ####
  output$mapRisk <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = areaPoly, weight = 2, color = "#D4A5D4D1",
                  group = "Area of Study", fill = FALSE) %>%
      addPolygons(data = areaGrid, weight = 0.5, stroke = TRUE,
                  layerId = paste0("OL", 1:nrow(areaGrid@data)),
                  fillColor = rgb(0,0,0,0),
                  color = rgb(0.9,0.9,0.9,0.3),
                  fillOpacity = 0.5, group = "Overlap", fill = TRUE) %>%
      setView(lng = mapCoo[1], lat = mapCoo[2], zoom = 9) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri World", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
}

shinyApp(ui, server)