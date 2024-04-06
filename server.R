library(RPostgreSQL)
library(sf)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(sp)
library(shinyjs)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(tictoc)

fire_info <- NULL

function(input, output, session){
  #database connection setup
  db_con <- dbConnect(PostgreSQL(), dbname = "GIS711_proj_test1", user = "jwang75", host = "localhost", password = "123456")
  
  #leaflet plugin setup
  deflatePlugin <- htmlDependency("leaflet.deflate", "0.3.0",
                                    src = c(href = "https://unpkg.com/Leaflet.Deflate@1.0.0-alpha.5/dist"),
                                    script = "L.Deflate.js")
  registerPlugin <- function(map, plugin){
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  
  #loading query script
  load_park_bas_query <- "SELECT unit_code, ST_FORCE2D(geom) FROM boundaries"
  load_fire_bas_query <- "SELECT gid, geom, fireoccurr, firecalend, newcat, specificca, controldat, gisacres AS area, cost FROM fire, fire_info"
  load_roads_bas_query <- "SELECT gid, geom FROM roads"
  load_buildings_bas_query <- "SELECT gid, geom FROM buildings"
  load_meta_bas_query <- "SELECT SUM(gisacres) AS area, COUNT(DISTINCT fire.fireoccurr) AS count, SUM(cost) AS cost, SUM(cost)/SUM(gisacres) AS avg_cost, COUNT(buildings.gid) AS building_count, firecalend FROM fire, fire_info, buildings WHERE fire.fireid = fire_info.fireid AND ST_INTERSECTS(ST_TRANSFORM(buildings.geom, 26959), fire.geom)"
  
  #park data loading function with condition from input in UI
  #loaded data in CRS 26959
  load_park <- function(){
    park_condition <- paste("\'", input$park_regeion, "\'", sep="")
    park_condition <- paste(park_condition, collapse = ",")
    park_condition <- paste(c("WHERE unit_code IN (", park_condition, ")"), collapse = "")
    load_park_query <- paste(load_park_bas_query, park_condition, collapse = " ")
    
    park_geom <- st_read(db_con, query = load_park_query)
    return(park_geom)
  }
  
  #fire data loading function with condition from input in UI
  #loaded data in CRS 26959
  load_fire <- function(){
    fire_condition <- "WHERE fire.fireid = fire_info.fireid AND "
    
    #add park condition
    #TODO: transform the hard-coded range to dynamically fetched from database
    if (length(input$park_regeion) <= 0){
      #non selected
      fire_condition <- paste(fire_condition, "FALSE", collapse = " ")
    }else if (length(input$park_regeion) < 2){
      #need condition to pick range
      park_condition <- paste("\'", input$park_regeion, "\'", sep="")
      park_condition <- paste(park_condition, collapse = ",")
      park_condition <- paste(c("unitcode IN (", park_condition, ")"), collapse = "")
      fire_condition <- paste(fire_condition, park_condition, collapse = " ")
    }else{
      #all selected, don't care about the condition
      fire_condition <- paste(fire_condition, "TRUE", collapse = " ")
    }
  
    fire_condition <- paste(fire_condition, "AND", collapse = " ")
    
    #add type of fire condition based on newCat
    #TODO: transform the hard-coded range to dynamically fetched from database
    if (length(input$fire_type) <= 0){
      #non selected
      fire_condition <- paste(fire_condition, "FALSE", collapse = " ")
    }else if (length(input$fire_type) < 3){
      #need condition to pick range
      type_condition <- paste("\'", input$fire_type, "\'", sep="")
      type_condition <- paste(type_condition, collapse=",")
      type_condition <- paste(c("newcat IN (", type_condition, ")"), collapse = "")
      fire_condition <- paste(fire_condition, type_condition, collapse = " ")
    }else{
      #all selected, don't care about the condition
      fire_condition <- paste(fire_condition, "TRUE", collapse = " ")
    }
    
    fire_condition <- paste(fire_condition, "AND", collapse = " ")
    
    #add fire calendar year condition
    #TODO: transform the hard-coded range to dynamically fetched from database
    if (input$fire_year[1] == 1980 && input$fire_year[2] == 2017){
      #all selected, don't care about the condition
      fire_condition <- paste(fire_condition, "TRUE", collapse = " ")
    }else{
      year_condition <- paste(c("firecalend >=", input$fire_year[1], "AND firecalend <=", input$fire_year[2]), collapse = " ")
      fire_condition <- paste(fire_condition, year_condition, collapse = " ")
    }
    
    fire_condition <- paste(fire_condition, "AND", collapse = " ")
    
    #add specific cause of fire condition
    #TODO: transform the hard-coded range to dynamically fetched from database
    if (length(input$specific_cause) <= 0){
      #non selected
      fire_condition <- paste(fire_condition, "FALSE", collapse = " ")
    }else if (length(input$specific_cause) < 21){
      spcc_condition <- paste("\'", input$specific_cause, "\'", sep="")
      spcc_condition <- paste(spcc_condition, collapse = ",")
      spcc_condition <- paste(c("(specificca IN(", spcc_condition, ")"), collapse="")
      
      if ("NULL" %in% input$specific_cause){
        spcc_condition <- paste(spcc_condition, "OR specificca IS NULL", collapse= " ")
      }
      spcc_condition <- paste(spcc_condition, ")", collapse = "")
      
      fire_condition <- paste(fire_condition, spcc_condition, collapse = " ")
    }else{
      #all selected, don't care about the condition
      fire_condition <- paste(fire_condition, "TRUE", collapse = " ")
    }
    
    #query statement combine
    load_fire_query <- paste(load_fire_bas_query, fire_condition, collapse = " ")
    load_fire_query <- paste(load_fire_query, "ORDER BY ST_AREA(geom) DESC", collapse = " ")
    
    fire_geom <- st_read(db_con, query = load_fire_query)
    return(fire_geom)
  }
  
  #load single fire info for tag
  get_fire_info_tag <- function(gid){
    tic(paste("popup: ", gid))
    #fire_info_query <- paste("SELECT fireoccurr, controldat FROM fire WHERE gid =", gid, collapse = " ")
    #fire_info <- st_read(db_con, query = fire_info_query)
    tar_fire_info <- fire_info[fire_info$gid == gid,]
    
    content <- as.character(tagList(
      tags$h4("Fire Occurrence ID:", tar_fire_info$fireoccurr),
      sprintf("Year: %s", tar_fire_info$firecalend), tags$br(),
      sprintf("Type: %s", tar_fire_info$newcat), tags$br(),
      sprintf("Specific Cause: %s", tar_fire_info$specificca), tags$br(),
      sprintf("Controlled Date: %s", as.character(tar_fire_info$controldat)), tags$br(),
      sprintf("Area: %s", tar_fire_info$area), tags$br(),
      sprintf("Cost: %s", tar_fire_info$cost)
    ))
    toc()
    return(content)
  }
  
  #roads data loading function with condition from input in UI
  #loaded data in CRS 26917
  load_roads <- function(){
    load_roads_query <- load_roads_bas_query
    
    road_geom <- st_read(db_con, query = load_roads_query)
    return(road_geom)
  }
  
  #buildings data loading function with condition from input in UI
  #loaded data in CRS 26917
  load_buildings <- function(){
    load_buildings_query <- load_buildings_bas_query
    
    buildings_geom <- st_read(db_con, query = load_buildings_query)
    return(buildings_geom)
  }
  
  #load metadata
  load_meta <- function(){
    meta_condition <- "AND"
    
    #setup park condition
    if (length(input$meta_park_regeion) <= 0){
      #non selected
      meta_condition <- paste(meta_condition, "FALSE", collapse = " ")
    }else if (length(input$meta_park_regeion) < 2){
      #need condition to pick range
      park_condition <- paste("\'", input$meta_park_regeion, "\'", sep="")
      park_condition <- paste(park_condition, collapse = ",")
      park_condition <- paste(c("unitcode IN (", park_condition, ")"), collapse = "")
      meta_condition <- paste(meta_condition, park_condition, collapse = " ")
    }else{
      #all selected, don't care about the condition
      meta_condition <- paste(meta_condition, "TRUE", collapse = " ")
    }
    
    meta_condition <- paste(meta_condition, "AND", collapse = " ")
    
    #setup fire cause condition
    if (length(input$meta_fire_type) <= 0){
      #non selected
      meta_condition <- paste(meta_condition, "FALSE", collapse = " ")
    }else if (length(input$meta_fire_type) < 3){
      #need condition to pick range
      type_condition <- paste("\'", input$meta_fire_type, "\'", sep="")
      type_condition <- paste(type_condition, collapse=",")
      type_condition <- paste(c("newcat IN (", type_condition, ")"), collapse = "")
      meta_condition <- paste(meta_condition, type_condition, collapse = " ")
    }else{
      #all selected, don't care about the condition
      meta_condition <- paste(meta_condition, "TRUE", collapse = " ")
    }
    
    meta_condition <- paste(meta_condition, "AND", collapse = " ")
    
    #set up sepecific cause condition
    if (length(input$meta_specific_cause) <= 0){
      #non selected
      meta_condition <- paste(meta_condition, "FALSE", collapse = " ")
    }else if (length(input$meta_specific_cause) < 21){
      spcc_condition <- paste("\'", input$meta_specific_cause, "\'", sep="")
      spcc_condition <- paste(spcc_condition, collapse = ",")
      spcc_condition <- paste(c("(specificca IN(", spcc_condition, ")"), collapse="")
      
      if ("NULL" %in% input$meta_specific_cause){
        spcc_condition <- paste(spcc_condition, "OR specificca IS NULL", collapse= " ")
      }
      spcc_condition <- paste(spcc_condition, ")", collapse = "")
      
      meta_condition <- paste(meta_condition, spcc_condition, collapse = " ")
    }else{
      #all selected, don't care about the condition
      meta_condition <- paste(meta_condition, "TRUE", collapse = " ")
    }
    
    #query statement combine
    load_meta_query <- paste(load_meta_bas_query, meta_condition, collapse = " ")
    load_meta_query <- paste(load_meta_query, "GROUP BY firecalend", collapse = " ")
    
    load_meta_prefix <- "SELECT COALESCE(f.area, 0) AS area, 
                          COALESCE(f.count, 0) AS count, 
                          COALESCE(f.cost, 0) AS cost, 
                          COALESCE(f.avg_cost, 0) AS avg_cost, 
                          COALESCE(f.building_count, 0) AS building_count, f2.firecalend
                          FROM (SELECT DISTINCT firecalend FROM fire) f2 LEFT JOIN("
    load_meta_postfix <- ") f USING (firecalend) ORDER BY f2.firecalend"
    load_meta_query <- paste(load_meta_prefix, load_meta_query, load_meta_postfix, collapse = " ")
    
    meta_data_year <- st_read(db_con, query = load_meta_query)
    
    return(meta_data_year)
  }
  
  #static map setup
  output$map_content <- renderLeaflet({
    leaflet() %>%
    addTiles() %>%
    addMapPane("park_pane", zIndex = 410) %>%
    addMapPane("fire_pane", zIndex = 490) %>%
    addMapPane("road_pane", zIndex = 430) %>%
    addMapPane("building_pane", zIndex = 440) %>%
    setView(lng=-80.931138, lat=25.623935, zoom = 9) %>%
    addLayersControl(overlayGroups = c("Fire", "Park", "Roads", "Buildings"),
                     position = "bottomleft") %>%
    #registerPlugin(deflatePlugin) %>%
    onRender("function(el, x){
             var fireFeatureGroup = new L.Deflate({minSize: 10, markerCluster: true});
             
             this.layerManager._byGroup['Fire'] = {};
             this.layerManager._groupContainers['Fire'] = fireFeatureGroup;
             fireFeatureGroup.groupname = 'Fire';
             fireFeatureGroup.addTo(this);
             
             map_content = this;
    }")
  }) 
  
  #meta table setup
  output$meta_data_plot <- renderPlotly({
    meta_data_year <- load_meta()
    
    plot_ly() %>%
      add_lines(x = meta_data_year$firecalend, y=meta_data_year$area, yaxis = "y1", name="Area in acre", color = I("red")) %>%
      add_lines(x = meta_data_year$firecalend, y=meta_data_year$count, yaxis = "y2", name="Incident count", color = I("blue")) %>%
      add_lines(x = meta_data_year$firecalend, y=meta_data_year$cost, yaxis = "y3", name="Yearly cost", color = I("green")) %>%
      add_lines(x = meta_data_year$firecalend, y=meta_data_year$avg_cost, yaxis = "y4", name="Cost per acre", color = I("purple")) %>%
      add_lines(x = meta_data_year$firecalend, y=meta_data_year$building_count, yaxis = "y5", name="Impacted building number", color = I("orange")) %>%
      layout(
        yaxis = list(
          side = "left"
          ,title = "Area in acre"
          ,color = "red"
        ),
        yaxis2 = list(
          overlaying = "y"
          ,title = "Incident count", anchor = "free"
          ,color = "blue"
        ),
        yaxis3 = list(
          side = "right", overlaying = "y"
          ,title = "Yearly cost"
          ,color = "green"
        ),
        yaxis4 = list(
          side = "right", overlaying = "y", position = 1
          ,title = "Cost per acre", anchor = "free"
          ,color = "purple"
        ),
        yaxis5 = list(
          side = "right", overlaying = "y", position = 2
          ,title = "Impacted building number", anchor = "free"
          ,color = "orange"
        ),
        xaxis = list(
          zeroline = FALSE, dtick = 1, title = "Year"
        ),
        margin = list(
          pad = 49, b = 140, l = 93, r = 0
        ),
        legend = list(
          x = 1.15
        )
      )
    
  })
  
  #static loading road and building data
  leafletProxy("map_content") %>%
  addPolylines(data = st_transform(load_roads(), crs = 4326), group = "Roads", color = "#195eff", weight = 1,
                opacity = 1.0, fillOpacity = 1.0,
                options = pathOptions(pane = "road_pane")) %>%
  hideGroup("Roads") %>%
  addCircles(data = st_transform(load_buildings(), 4326), group = "Buildings", color = "#aa0cff",
             options = pathOptions(pane = "building_pane")) %>%
  hideGroup("Buildings")

  #dynamic input handler - change in Park region
  observeEvent(
    input$map_update
  ,
  {
    park_geom <- load_park()
      
    if (dim(park_geom)[1] > 0){
      #returned result is not empty
      park_geom <- st_transform(park_geom, crs = 4326)
        
      leafletProxy("map_content") %>%
        clearGroup("Park") %>%
        addPolygons( data = park_geom, group = "Park", color = "#79f718", weight = 1, smoothFactor = 0.5,
                     opacity = 1.0, fillColor = "#79f718", fillOpacity = 0.5,
                     options = pathOptions(pane = "park_pane"))
    }else{
      #empty returned result
      leafletProxy("map_content") %>%
        clearGroup("Park")
    }
  }, ignoreNULL = FALSE)
  
  #dynamic input handler - change in Fire
  observeEvent(
    input$map_update
  ,
  {
    tic("load fire data")
    fire_geom <- load_fire()
    toc()
    
    if (dim(fire_geom)[1] > 0){
      #returned result is not empty
      tic("transform geom")
      #fire_geom <- ms_simplify(fire_geom, keep = 0.05, keep_shapes = TRUE, sys=TRUE, no_repair = TRUE)
      fire_geom <- st_transform(fire_geom, crs=4326)
      fire_info <<- fire_geom
      gc()
      toc()
      
      tic("render leaflet")
      js$clearDeflate("Fire")
      leafletProxy("map_content") %>%
        #clearGroup("Fire") %>%
        #addMarkers(data = fire_geom, layerId = ~gid, group = "Fire")
        addPolygons( data = fire_geom, layerId = ~gid, group = "Fire", color = "#444444", weight = 1, smoothFactor = 1,
                     opacity = 1.0, fillOpacity = 0.5,
                     fillColor = "red",
                     highlightOptions = highlightOptions(color = "white", weight = 2,
                                                         bringToFront = TRUE),
                     options = pathOptions(pane = "fire_pane"))
      toc()
      
      output$data_explore_table <- DT::renderDataTable({
        fire_table_data <- fire_info %>%
          select(
            gid = gid,
            Occurence = fireoccurr,
            Year = firecalend,
            Type = newcat,
            Cause = specificca,
            Controlled= controldat,
            Area = area,
            Cost = cost
          )
        st_geometry(fire_table_data) <- NULL
        
        geom_centr <- st_centroid(fire_info)
        
        fire_table_data$Lat <- st_coordinates(geom_centr)[,2]
        fire_table_data$Long <- st_coordinates(geom_centr)[,1]
        
        fire_table_data <- fire_table_data %>%
          mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-gid="', gid, '"><i class="fa fa-crosshairs"></i></a>', sep="")) %>%
          select(-gid, -Lat, -Long)
        
        action <- DT::dataTableAjax(session, fire_table_data)
        
        DT::datatable(fire_table_data, options = list(ajax = list(url = action)), escape = FALSE)
      })
    }else{
      #empty returned result
      js$clearDeflate("Fire")
      #leafletProxy("map_content") %>%
      #  clearGroup("Fire")
      output$data_explore_table <- DT::renderDataTable({
        DT::datatable(NULL)
      })
    }
  }, ignoreNULL = FALSE)
  
  #data table to map click handle
  observeEvent(input$goto, {
    if (is.null(input$goto))
      return()
    
    isolate({
      map <- leafletProxy("map_content")
      map %>% clearPopups()
      dist <- 0.5
      gid <- input$goto$gid
      lat <- input$goto$lat
      lng <- input$goto$lng
      
      content <- get_fire_info_tag(gid)
      map %>%
        addPopups(lng, lat, content, layerId = gid) 
      map %>%
        fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  #popup click event handler
  observeEvent(input$map_content_shape_click, {
    leafletProxy("map_content") %>% clearPopups()
    
    event <- input$map_content_shape_click
    if (is.null(event)){
      return()
    }
    
    if (event$group == "Fire"){
      isolate({
        content <- get_fire_info_tag(event$id)
        leafletProxy("map_content") %>%
          addPopups(event$lng, event$lat, content, layerId = event$id)
      })
    }
  })
  
  #meta data table update
  #observeEvent(
  #  input$meta_update
  #  ,
  #  {
  #    meta_data_year <- load_meta()
  #    print(input$meta_data_show)
  #    plotlyProxy("meta_data_plot", session) %>%
  #     plotlyProxyInvoke("deleteTraces", as.integer(c(1,2,3,4,5)))%>%
  #      plotlyProxyInvoke("addTraces", 
  #                        list(list(x = meta_data_year$firecalend, y=meta_data_year$area, yaxis = "y1", name="Area in acre", line = list(color = "red"), visible = (1 %in% input$meta_data_show)),
  #                             list(x = meta_data_year$firecalend, y=meta_data_year$count, yaxis = "y2", name="Incident count", line = list(color = "blue"), visible = (2 %in% input$meta_data_show)),
  #                             list(x = meta_data_year$firecalend, y=meta_data_year$cost, yaxis = "y3", name="Yearly cost", line = list(color = "green"), visible = (3 %in% input$meta_data_show)),
  #                             list(x = meta_data_year$firecalend, y=meta_data_year$avg_cost, yaxis = "y4", name="Cost per acre", line = list(color = "purple"), visible = (4 %in% input$meta_data_show)),
  #                             list(x = meta_data_year$firecalend, y=meta_data_year$building_count, yaxis = "y5", name="Impacted building number", line = list(color = "orange"), visible = (5 %in% input$meta_data_show))
  #                        )
  #      )
  #  }
  #)
  
}
