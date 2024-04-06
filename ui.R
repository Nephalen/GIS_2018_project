library(leaflet)
library(shinyjs)
library(shinyWidgets)
library(plotly)

jsDeflateAdd <- "shinyjs.addDeflate = function(){
                markerGroup.checkIn(fireFeatureGroup);
}"
jsDeflateRemove <- "shinyjs.clearDeflate = function(group){
                /*
                map_content.eachLayer(function (layer){
                  console.log(layer.groupname);
                  if (typeof layer.getLayers != 'undefined'){
                    console.log(layer.getLayers().length);
                  }
                });*/

                var _this6 = map_content.layerManager
                var groupTable = map_content.layerManager._byGroup[group];
                if (!groupTable) {
                  return false;
                }

                var stamps = [];
                for (var k in groupTable) {
                  stamps.push(k);
                }

                stamps.forEach(function (stamp) {
                  var layerInfo = _this6._byStamp[stamp];
                  if (!layerInfo) {
                    return false;
                  }

                  if (typeof layerInfo.group === 'string') {
                    delete _this6._byGroup[layerInfo.group][stamp];
                  }
                  if (typeof layerInfo.layerId === 'string') {
                    delete _this6._byLayerId[_this6._layerIdKey(layerInfo.category, layerInfo.layerId)];
                  }
                  delete _this6._byCategory[layerInfo.category][stamp];
                  delete _this6._byStamp[stamp];
                  if (layerInfo.ctGroup) {
                    var ctGroup = _this6._byCrosstalkGroup[layerInfo.ctGroup];
                    var layersForKey = ctGroup[layerInfo.ctKey];
                    var idx = layersForKey ? layersForKey.indexOf(stamp) : -1;
                    if (idx >= 0) {
                      if (layersForKey.length === 1) {
                        delete ctGroup[layerInfo.ctKey];
                      } else {
                        layersForKey.splice(idx, 1);
                      }
                    }
                  }
                });

                var g = _this6._groupContainers[group];
                g.clearLayers();
}"

fluidPage(id = "main_container",
  useShinyjs(),
  extendShinyjs(text = jsDeflateAdd),
  extendShinyjs(text = jsDeflateRemove),
  
  #navigation bar
  navbarPage("GIS711 project", id = "nav",
             tabPanel("Data Explorer",
                      sidebarLayout(
                        sidebarPanel(id="map_control", class="panel panel-default", fixed=TRUE,
                                      top=60, left="auto", right=20, bottom="auto", height="auto",
                                      h2("Data Select"),
                                      checkboxGroupInput("park_regeion",
                                                         h3("Park Region"),
                                                         choices = list("Big Cypress National Preserve" = "BICY",
                                                                        "Everglades National Park" = "EVER"),
                                                         selected = list("BICY", "EVER")),
                                      sliderInput("fire_year",
                                                  h3("Year"),
                                                  min = 1980, max = 2017, value = c(2016, 2017), width = "100%"),
                                      checkboxGroupInput("fire_type",
                                                         h3("Fire Type"),
                                                         choices = list("Wild Fire Use" = "WFU",
                                                                        "Prescribed Fire" = "Prescribed fire",
                                                                        "Wild Fire" = "Wildfire"),
                                                         selected = "Wildfire"),
                                      pickerInput("specific_cause",
                                                  h3("Specific Cause"),
                                                  choices = list("Unknown" = "NULL",
                                                                 "burning dump" = "12",
                                                                 "smoking" = "10",
                                                                 "burning building" = "24",
                                                                 "vehicle" = "03",
                                                                 "resource management" = "17",
                                                                 "field burning" = "13",
                                                                 "slash burning" = "15",
                                                                 "lightning" = "01",
                                                                 "other, unknown" = "30",
                                                                 "exhaust" = "04",
                                                                 "recurrent" = "19",
                                                                 "power line" = "25",
                                                                 "land clearing" = "14",
                                                                 "aircraft" = "02",
                                                                 "brakes" = "07",
                                                                 "grudge fire" = "18",
                                                                 "31" = "31",
                                                                 "trash burning" = "11",
                                                                 "other, known" = "32",
                                                                 "fireworks" = "26",
                                                                 "cooking/warming" = "08"),
                                                  selected = "01", 
                                                  options = list(`actions-box` = TRUE, `size` = 6, `virtualScroll` = TRUE),
                                                  multiple = TRUE),
                                      actionButton("map_update", "Update")
                        ),
                        mainPanel(
                          tabsetPanel(id = "data_explorer_tabset",
                                      #map panel
                                      tabPanel("Map",
                                               div(class="container",
                                                   tags$head(
                                                     includeCSS("style.css"),
                                                     includeCSS("https://unpkg.com/leaflet.markercluster@1.4.1/dist/MarkerCluster.css"),
                                                     includeCSS("https://unpkg.com/leaflet.markercluster@1.4.1/dist/MarkerCluster.Default.css"),
                                                     includeScript("gomap.js"),
                                                     includeScript("https://unpkg.com/leaflet.markercluster@1.4.1/dist/leaflet.markercluster.js"),
                                                     includeScript("https://unpkg.com/leaflet.markercluster.layersupport@2.0.1/dist/leaflet.markercluster.layersupport-src.js"),
                                                     includeScript("https://unpkg.com/Leaflet.Deflate@1.0.0-alpha.5/dist/L.Deflate.js")
                                                   ),
                                                   
                                                   leafletOutput("map_content", width = "100%", height = "750px")
                                               )         
                                      ),
                                      
                                      tabPanel("Data Table",
                                               DT::dataTableOutput("data_explore_table")
                                      ),
                                      
                                      conditionalPanel("false", icon("crosshair"))
                          )
                        )
                      )
             ),
             
             tabPanel("Meta Data Explorer",
                      plotlyOutput("meta_data_plot", height="500px"),
                      hr(),
                      fluidRow(
                        column(4,
                               checkboxGroupInput("meta_park_regeion",
                                                   h3("Park Region"),
                                                   choices = list("Big Cypress National Preserve" = "BICY",
                                                                  "Everglades National Park" = "EVER"),
                                                   selected = list("BICY", "EVER"))),
                        column(4,
                               checkboxGroupInput("meta_fire_type",
                                                  h3("Fire Type"),
                                                  choices = list("Wild Fire Use" = "WFU",
                                                                 "Prescribed Fire" = "Prescribed fire",
                                                                 "Wild Fire" = "Wildfire"),
                                                  selected = "Wildfire")
                               ),
                        column(4,
                               pickerInput("meta_specific_cause",
                                           h3("Specific Cause"),
                                           choices = list("Unknown" = "NULL",
                                                          "burning dump" = "12",
                                                          "smoking" = "10",
                                                          "burning building" = "24",
                                                          "vehicle" = "03",
                                                          "resource management" = "17",
                                                          "field burning" = "13",
                                                          "slash burning" = "15",
                                                          "lightning" = "01",
                                                          "other, unknown" = "30",
                                                          "exhaust" = "04",
                                                          "recurrent" = "19",
                                                          "power line" = "25",
                                                          "land clearing" = "14",
                                                          "aircraft" = "02",
                                                          "brakes" = "07",
                                                          "grudge fire" = "18",
                                                          "31" = "31",
                                                          "trash burning" = "11",
                                                          "other, known" = "32",
                                                          "fireworks" = "26",
                                                          "cooking/warming" = "08"),
                                           selected = "01", 
                                           options = list(`actions-box` = TRUE, `size` = 6, `virtualScroll` = TRUE),
                                           multiple = TRUE))#,
                               #actionButton("meta_update", "Update"))
                      )
             )
  )
  
  
)

