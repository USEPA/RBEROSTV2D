
### Leaflet Watershed Comparison Module for Salish Sea Model Vintages

# Options
options(stringsAsFactors=FALSE, scipen = 999)

# Settings
projection <- "EPSG: 5072"
map_project <- "EPSG: 4326"
date <- Sys.Date()
user <- Sys.getenv("USERNAME")

# Load Libraries
pacman::p_load("leaflet", "sf", "dplyr", "tidyverse", "janitor")

# Path
ssm_path <- paste0(working_dir, "RBEROST-Pacific/Preprocessing/Inputs/SSM_Models/")

SSM4 <- st_read(paste0(ssm_path,"SSM4_watershed.shp"))%>% #*# USer to ensure this is up to date with new vintage info
  janitor::clean_names()%>%
  st_transform(map_project)

SSM5 <- st_read(paste0(ssm_path,"SSM5_watershed.shp"))%>% #*# USer to ensure this is up to date with new vintage info
  janitor::clean_names()%>%
  st_transform(map_project)

# Create data attributes for JavaScript access
ssm4_data <- list(
  names = SSM4$wtrshd, #*# USer to ensure this is up to date with new vintage info
  ids = SSM4$wtrs_id #*# USer to ensure this is up to date with new vintage info
)

ssm5_data <- list(
  names = SSM5$ssm5_nm, #*# USer to ensure this is up to date with new vintage info
  ids = SSM5$ssm5_id #*# USer to ensure this is up to date with new vintage info
)

Salish_Sea_Mapper <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.25)) %>%
  addTiles(options = tileOptions(opacity = 0.5)) %>%
  addPolygons(
    data = SSM4 %>% st_transform(map_project), 
    layerId = paste0("ssm4_", 1:nrow(SSM4)),
    color = "blue", 
    weight = 0.5, 
    opacity = 1, 
    fillOpacity = 0.4, 
    fillColor = "blue",
    group = "SSM4"
  ) %>%
  addPolygons(
    data = SSM5 %>% st_transform(map_project), 
    layerId = paste0("ssm5_", 1:nrow(SSM5)),
    color = "darkred", 
    weight = 0.5, 
    opacity = 1, 
    fillOpacity = 0.2, 
    fillColor = "red",
    group = "SSM5"
  ) %>%
  addLayersControl(
    overlayGroups = c("SSM4", "SSM5"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  htmlwidgets::onRender(paste0("
    function(el, x) {
      var map = this;
      
      // Store the data in JavaScript variables
      var ssm4Data = ", jsonlite::toJSON(ssm4_data), ";
      var ssm5Data = ", jsonlite::toJSON(ssm5_data), ";
      
      var clickedLayers = [];
      var clickTimer = null;
      var clickLatLng = null;
      
      // Listen to individual polygon click events
      map.eachLayer(function(layer) {
        if (layer instanceof L.Polygon && layer.options.layerId) {
          layer.on('click', function(e) {
            // REMOVED: L.DomEvent.stopPropagation(e); - This was blocking SSM5 clicks
            
            var currentLatLng = e.latlng;
            
            // If this is a new click location, reset
            if (!clickLatLng || 
                Math.abs(clickLatLng.lat - currentLatLng.lat) > 0.0001 || 
                Math.abs(clickLatLng.lng - currentLatLng.lng) > 0.0001) {
              clickedLayers = [];
              clickLatLng = currentLatLng;
            }
            
            // Add this layer to the clicked layers
            if (clickedLayers.indexOf(layer) === -1) {
              clickedLayers.push(layer);
            }
            
            // Clear existing timer
            if (clickTimer) {
              clearTimeout(clickTimer);
            }
            
            // Set a short delay to collect all simultaneous clicks
            clickTimer = setTimeout(function() {
              console.log('Processing', clickedLayers.length, 'clicked layers');
              
              // Group layers by type
              var ssm4Layers = [];
              var ssm5Layers = [];
              
              clickedLayers.forEach(function(layer) {
                var layerId = layer.options.layerId;
                var layerType = layerId.split('_')[0];
                
                if (layerType === 'ssm4') {
                  ssm4Layers.push(layer);
                } else if (layerType === 'ssm5') {
                  ssm5Layers.push(layer);
                }
              });
              
              // Create separate popups that stay open until manually closed
              
              // Create SSM4 popup if there are SSM4 layers
              if (ssm4Layers.length > 0) {
                var ssm4Content = '';
                
                ssm4Layers.forEach(function(layer, index) {
                  var layerId = layer.options.layerId;
                  var layerIndex = parseInt(layerId.split('_')[1]) - 1;
                  
                  if (index > 0) ssm4Content += '<hr>';
                  
                  ssm4Content += '<div style=\"background-color: rgba(0,0,255,0.2); padding: 5px; margin: 2px; border: 1px solid blue;\">';
                  // REMOVED: ssm4Content += '<b>SSM4 Layer</b><br>';
                  ssm4Content += '<b>Watershed Name:</b> ' + (ssm4Data.names[layerIndex] || 'N/A') + '<br>';
                  ssm4Content += '<b>Data ID:</b> ' + (ssm4Data.ids[layerIndex] || 'N/A');
                  ssm4Content += '</div>';
                });
                
                // Create offset position for SSM4 popup
                var ssm4LatLng = L.latLng(clickLatLng.lat + 0.0005, clickLatLng.lng - 0.0005);
                
                L.popup({
                  closeButton: true,
                  autoClose: false,
                  closeOnClick: false,
                  closeOnEscapeKey: true
                })
                  .setLatLng(ssm4LatLng)
                  .setContent('<div style=\"max-width: 200px; border: 2px solid blue;\"><h4 style=\"margin: 0 0 5px 0; color: blue;\">SSM4 Data</h4>' + ssm4Content + '</div>')
                  .openOn(map);
              }
              
              // Create SSM5 popup if there are SSM5 layers
              if (ssm5Layers.length > 0) {
                var ssm5Content = '';
                
                ssm5Layers.forEach(function(layer, index) {
                  var layerId = layer.options.layerId;
                  var layerIndex = parseInt(layerId.split('_')[1]) - 1;
                  
                  if (index > 0) ssm5Content += '<hr>';
                  
                  ssm5Content += '<div style=\"background-color: rgba(255,0,0,0.2); padding: 5px; margin: 2px; border: 1px solid red;\">';
                  // REMOVED: ssm5Content += '<b>SSM5 Layer</b><br>';
                  ssm5Content += '<b>Watershed Name:</b> ' + (ssm5Data.names[layerIndex] || 'N/A') + '<br>';
                  ssm5Content += '<b>Data ID:</b> ' + (ssm5Data.ids[layerIndex] || 'N/A');
                  ssm5Content += '</div>';
                });
                
                // Create offset position for SSM5 popup
                var ssm5LatLng = L.latLng(clickLatLng.lat - 0.0005, clickLatLng.lng + 0.0005);
                
                L.popup({
                  closeButton: true,
                  autoClose: false,
                  closeOnClick: false,
                  closeOnEscapeKey: true
                })
                  .setLatLng(ssm5LatLng)
                  .setContent('<div style=\"max-width: 200px; border: 2px solid red;\"><h4 style=\"margin: 0 0 5px 0; color: red;\">SSM5 Data</h4>' + ssm5Content + '</div>')
                  .addTo(map);
              }
              
              // Reset for next click
              clickedLayers = [];
              clickLatLng = null;
              clickTimer = null;
              
            }, 50); // 50ms delay to collect overlapping clicks
          });
        }
      });
    }
  "))

beepr::beep()

Salish_Sea_Mapper



















