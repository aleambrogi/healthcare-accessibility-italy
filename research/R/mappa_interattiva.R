library(sf)
library(leaflet)
library(dplyr)
library(jsonlite)
library(htmlwidgets)
library(geojsonsf)
library(readr)
library(rmapshaper)

# 1. Carica dati
shp <- st_read("C:/Users/samsung/Documents/MAGISTRALE/PRIMO ANNO/APPLIED STATISTICS/GRINS Indice SSN/Datasets/ISTAT shape file/Limiti2021/Com2021/Com2021.shp", quiet = TRUE) %>%
  ms_simplify(keep = 0.01, keep_shapes = TRUE)
dati <- read_csv("C:/Users/samsung/Documents/MAGISTRALE/PRIMO ANNO/APPLIED STATISTICS/GRINS Indice SSN/Scripts/DATASET_DIST/stress_comuni.csv")

# 2. Join dati e trasformazione CRS
map_data <- shp %>% 
  right_join(dati, by = c("PRO_COM" = "PRO_COM")) %>%
  st_transform(crs = 4326)
map_data <- map_data %>%
  st_make_valid()

map_data <- map_data %>%
  select(COMUNE, total_index_avg, stress_SSN_avg, mobility_index_avg, tt_ps1_avg, tt_ps2_avg, tt_ps_ped,
         tt_ts_avg, tt_airport_avg, tt_port_avg, TOT_P, TOT_Male, TOT_Female, TOT_EMP, geometry)

# 3. Calcola colore per ogni comune usando i quartili dell'indice
quartili <- quantile(map_data$total_index_avg, probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 1), na.rm = TRUE)

pal <- colorBin(
  palette = "YlOrRd",
  domain = map_data$total_index_avg,
  bins = quartili,
  na.color = "transparent"
)

map_data$fillColor <- pal(map_data$total_index_avg)

# 4. Costruisci popup HTML con tutte le variabili richieste
map_data$popup_info <- paste0(
  "<b>", map_data$COMUNE, "</b><br/>",
  "SSN Final Index: ", round(map_data$total_index_avg, 4), "<br/>",
  "Stress SSN index: ", round(map_data$stress_SSN_avg, 4), "<br/>",
  "Mobility Index: ", round(map_data$mobility_index_avg, 4), "<br/>",
  "Travel Time First ER: ", round(map_data$tt_ps1_avg, 4), " min<br/>",
  "Travel Time Second ER: ", round(map_data$tt_ps2_avg, 4), " min<br/>",
  "Travel Time Pediatric ER: ", round(map_data$tt_ps_ped, 4), " min<br/>",
  "Travel Time Train Station: ", round(map_data$tt_ts_avg, 4), " min<br/>",
  "Travel Time Airport: ", round(map_data$tt_airport_avg, 4), " min<br/>",
  "Travel Time Port: ", round(map_data$tt_port_avg, 4), " min<br/>",
  "Total Population: ", map_data$TOT_P, "<br/>",
  "Total Male: ", map_data$TOT_Male, "<br/>",
  "Total Female: ", map_data$TOT_Female, "<br/>",
  "Total Employed: ", map_data$TOT_EMP
)


map_data <- map_data %>%
  dplyr::select(COMUNE, total_index_avg, fillColor, popup_info, geometry)

# 5. Converti dati in GeoJSON, mantenendo proprietà importanti
geojson_data <- geojsonsf::sf_geojson(map_data)
object.size(geojson_data) %>% format(units = "MB")

# 6. Calcola coordinate centroidi per la ricerca
centroids <- st_centroid(map_data)
coords <- st_coordinates(centroids)

# 7. Crea lista JSON con nome e coordinate per ricerca
lista_comuni <- toJSON(data.frame(
  name = map_data$COMUNE,
  lat = coords[, 2],
  lon = coords[, 1]
), auto_unbox = TRUE)

mappa <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
  addProviderTiles("OpenStreetMap") %>%
  setView(lng = 12.5, lat = 42.5, zoom = 6)
mappa <- mappa %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = map_data$total_index_avg,
    title = "SSN Final Index",
    opacity = 0.7,
    labFormat = labelFormat(digits = 2)
  )

# 8. Crea la mappa leaflet, centrata sull'Italia
mappa <- htmlwidgets::onRender(mappa, sprintf("
  function(el, x) {
    var comuni = %s;
    var map = this;
    var layerByComune = {};

    var geojson = L.geoJSON(%s, {
      style: function(feature) {
        return {
          color: '#444444',
          weight: 0.1,
          fillColor: feature.properties.fillColor,
          fillOpacity: 0.7
        };
      },
      onEachFeature: function(feature, layer) {
        var popupContent = feature.properties.popup_info;
        layer.bindPopup(popupContent);
        layerByComune[feature.properties.COMUNE] = {
          layer: layer,
          popupContent: popupContent
        };
      }
    }).addTo(map);

    function isMobile() {
      return /Android|iPhone|iPad|iPod|Opera Mini|IEMobile|WPDesktop/i.test(navigator.userAgent);
    }

    var container = document.createElement('div');
    container.style = 'position: absolute; bottom: 30px; left: 10px; z-index: 9999; background: white; padding: 8px; border-radius: 4px; width: 280px; box-shadow: 0 0 8px rgba(0,0,0,0.2);';

    var input = document.createElement('input');
    input.type = 'text';
    input.placeholder = 'Search for your city...';
    input.style = 'width: 100%%; padding: ' + (isMobile() ? '10px' : '6px') + '; font-size: ' + (isMobile() ? '18px' : '16px') + '; border: 1px solid #ccc; border-radius: 3px;';
    input.id = 'searchInput';
    container.appendChild(input);

    var results = document.createElement('ul');
    results.id = 'searchResults';
    results.style = 'list-style-type: none; margin: 5px 0 0 0; padding: 0; max-height: 150px; overflow-y: auto; font-size: 14px;';
    container.appendChild(results);

    el.appendChild(container);

    input.addEventListener('input', function() {
      var val = this.value.toLowerCase();
      results.innerHTML = '';
      if (val.length < 2) return;

      var filtered = comuni.filter(function(c) {
        return c.name.toLowerCase().startsWith(val);
      });

      filtered.forEach(function(c) {
        var li = document.createElement('li');
        li.textContent = c.name;
        li.style.cursor = 'pointer';
        li.style.padding = '4px 6px';
        li.onmouseover = function() { this.style.backgroundColor = '#eee'; };
        li.onmouseout = function() { this.style.backgroundColor = 'white'; };
        li.onclick = function() {
          results.innerHTML = '';
          input.value = c.name;

          var comuneData = layerByComune[c.name];
          if (comuneData) {
            var bounds = comuneData.layer.getBounds();
            var center = bounds.getCenter();

            var latOffset = isMobile() ? 0.08 : 0;
            var lonOffset = isMobile() ? 0.05 : 0;
            var adjustedLat = center.lat + latOffset;
            var adjustedLng = center.lng + lonOffset;

            var zoomLevel = isMobile() ? 10 : 12;

            map.setView([adjustedLat, adjustedLng], zoomLevel, { animate: true });

            setTimeout(function() {
              var popup = L.popup({
                maxWidth: 300,
                closeButton: true,
                autoPan: true
              })
              .setLatLng(center)
              .setContent(comuneData.popupContent)
              .openOn(map);
            }, 600); // tempo per assicurare che il centramento sia completato
          }
        };
        results.appendChild(li);
      });
    });
  }
", lista_comuni, geojson_data))

# 10. Salva mappa in HTML
saveWidget(mappa, "mappa_interattiva/index.html", selfcontained = TRUE)

