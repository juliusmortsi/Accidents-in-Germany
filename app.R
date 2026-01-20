# =========================================================================
# ACCIDENTS IN GERMANY - SHINY APP WITH HISTOGRAM & MAP VISUALIZATIONS
# =========================================================================
# This app visualizes accident data from Germany using interactive
# histograms and a geographic map. It supports two datasets:
# 1. state_long.csv - State-level accident data
# 2. casualties_long.csv - Casualties data by demographics
# =========================================================================

# --------------------------
# LOAD REQUIRED PACKAGES
# --------------------------
library(shiny) # Core Shiny framework
library(shinythemes) # Pre-built themes for Shiny apps
library(tidyverse) # Data manipulation (dplyr, ggplot2, etc.)
library(plotly) # Interactive plots
library(DT) # Interactive data tables
library(leaflet) # Interactive maps
library(sf) # Spatial data handling

# --------------------------
# GLOBAL VARIABLES (LINT FIXES)
# --------------------------
# Cleanly declare data frame columns to silence R CMD check/lint warnings
utils::globalVariables(c(
  "state", "type_of_injury", "location_of_injury", "year", "outcome",
  "sex", "age_group", "area", "cases", "name", "total_cases",
  "avg_cases", "month", "Total", "mode"
))

# --------------------------
# LOAD DATA
# --------------------------
# Load state-level accident data
state_data <- read_csv("state_long.csv")

# Load casualties data and condense 11 age groups into 6 logical categories
casualties_data <- read_csv("casualties_long.csv") %>%
  mutate(age_group = case_when(
    age_group %in% c("under 15 years", "15 to under 18 years") ~ "< 18 (Minors)",
    age_group %in% c("18 to under 21 years", "21 to under 25 years") ~ "18-24 (Young Drivers)",
    age_group %in% c("25 to under 35 years", "35 to under 45 years") ~ "25-44 (Middle-Aged)",
    age_group %in% c("45 to under 55 years", "55 to under 65 years") ~ "45-64 (Mature Adults)",
    age_group %in% c("65 to under 75 years", "75 years and over") ~ "65+ (Seniors)",
    age_group == "age unknown" ~ "Unknown",
    TRUE ~ age_group
  )) %>%
  # Ensure logical chronological order
  mutate(age_group = factor(age_group, levels = c(
    "< 18 (Minors)", "18-24 (Young Drivers)", "25-44 (Middle-Aged)",
    "45-64 (Mature Adults)", "65+ (Seniors)", "Unknown"
  )))

# Load Germany state boundaries for mapping (pre-fetched for portability)
germany_states <- readRDS("germany_states.rds")

# --------------------------
# USER INTERFACE (UI)
# --------------------------
ui <- fluidPage(
  # Apply a clean, modern theme
  theme = shinytheme("flatly"),

  # Custom CSS for state info panel
  tags$head(
    tags$style(HTML("
      /* === Global & Layout === */
      html, body { height: 100vh; overflow-y: auto; padding: 0 10px !important; }
      .container-fluid { padding: 0 !important; }

      /* === Components === */
      .state-info-panel {
        position: absolute; background: white; padding: 15px 20px; border-radius: 8px; z-index: 1000;
        box-shadow: 0 4px 12px rgba(0,0,0,0.3); min-width: 250px; display: none; pointer-events: auto;
      }
      .state-info-panel h3 { margin: 0 0 10px 0; color: #333; font-size: 18px; border-bottom: 2px solid #007bff; padding-bottom: 5px; }
      .state-info-panel .info-row { margin: 8px 0; display: flex; justify-content: space-between; }
      .state-info-panel .info-label { font-weight: bold; color: #555; }
      .state-info-panel .info-value { color: #007bff; font-weight: bold; }
      .state-info-panel .close-btn { position: absolute; top: 5px; right: 10px; cursor: pointer; font-size: 20px; color: #999; font-weight: bold; }
      .state-info-panel .close-btn:hover { color: #333; }

      /* === Themes (Custom Palette) === */
      /* Colors: Teal(#007E7E, #309898), Orange/Yel(#F4631E, #FF9F00), Red(#CB041F, #AD0000) */

      /* Sidebar: Orange */
      .well { background: linear-gradient(135deg, #F4631E 0%, #FF9F00 100%) !important; border: none !important; color: white !important; box-shadow: 0 4px 15px rgba(244, 99, 30, 0.3) !important; }
      .well h4, .well .control-label { color: white !important; font-weight: bold; }
      .well hr { border-top: 1px solid rgba(255,255,255,0.3); }
      .well .btn-default { background: rgba(255,255,255,0.2) !important; color: white !important; border: 1px solid rgba(255,255,255,0.5) !important; }

      /* Tabs: Teals */
      .nav-tabs { background: linear-gradient(to right, #007E7E 0%, #309898 100%) !important; border-bottom: 2px solid #309898 !important; border-radius: 8px 8px 0 0; padding: 5px 5px 0 5px; margin-bottom: 15px; box-shadow: 0 4px 10px rgba(0,0,0,0.1); }
      .nav-tabs > li > a { color: rgba(255,255,255,0.9) !important; border: none !important; font-weight: bold; transition: all 0.3s ease; margin-right: 5px; border-radius: 6px 6px 0 0 !important; }
      .nav-tabs > li > a:hover { background: rgba(255,255,255,0.2) !important; color: white !important; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        background: #309898 !important; color: #fff !important; border: none !important; box-shadow: 0 -2px 10px rgba(0,0,0,0.1);
      }

      /* Title: Centered Bold */
      .app-title { text-align: center; width: 100%; margin: 10px 0 15px 0; }
      .app-title h2 { font-family: 'Arial Black', sans-serif; font-weight: 900 !important; color: #AD0000 !important; font-size: 28px; letter-spacing: -1px; text-transform: uppercase; margin: 0; }

      /* KPI Cards: Reds (Swapped) */
      .kpi-row { margin-bottom: 20px; padding: 0 10px; }
      .kpi-card {
        background: linear-gradient(135deg, #CB041F 0%, #AD0000 100%); /* Bright to Dark Red */
        border-radius: 12px; padding: 15px; text-align: center;
        box-shadow: 0 4px 15px rgba(173, 0, 0, 0.3); border: 1px solid rgba(255,255,255,0.2);
        transition: transform 0.3s ease; height: 110px; display: flex; flex-direction: column; justify-content: center;
        color: #fff;
      }
      .kpi-card:hover { transform: translateY(-3px); box-shadow: 0 8px 25px rgba(0,0,0,0.2); }
      .kpi-title { color: rgba(255,255,255,0.9); font-size: 11px; font-weight: 700; text-transform: uppercase; margin-bottom: 5px; letter-spacing: 1px; }
      .kpi-value { color: #fff; font-size: 24px; font-weight: 900; margin-bottom: 2px; text-shadow: 0 1px 2px rgba(0,0,0,0.2); }
      .kpi-trend { font-size: 11px; font-weight: 700; }
      .trend-up { color: #FF9F00; } .trend-down { color: #007E7E; } .trend-neutral { color: #fff; }

      /* === Animations (Splash) === */
      #splash-overlay { position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; background: #f8f9fa; z-index: 9999; display: flex; align-items: center; justify-content: center; transition: opacity 1s ease-out, visibility 1s; }
      .splash-container { position: relative; width: 800px; height: 200px; overflow: hidden; }
      .car-sprite { position: absolute; font-size: 80px; transition: transform 0.3s ease-out; }
      .car-left { left: -100px; animation: drive-right-human 3s 1 linear forwards; }
      .car-taxi { right: -100px; animation: drive-left-taxi 3s 1 linear forwards; }
      .ambulance { right: -100px; opacity: 0; animation: drive-ambulance-left 3s 1 linear forwards; }
      .explosion { position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%) scale(0); font-size: 100px; animation: explode 3s 1 linear forwards; opacity: 0; }
      @keyframes drive-right-human { 0% { left: -100px; opacity: 1; } 15% { left: 45%; opacity: 1; } 18% { left: 45%; opacity: 0; } 100% { left: 45%; opacity: 0; } }
      @keyframes drive-left-taxi { 0% { right: -100px; opacity: 1; } 15% { right: 45%; opacity: 1; } 18% { right: 45%; opacity: 0; } 100% { right: 45%; opacity: 0; } }
      @keyframes drive-ambulance-left { 0%, 20% { right: -100px; opacity: 0; } 25% { right: -100px; opacity: 1; } 85% { right: 110%; opacity: 1; } 100% { right: 110%; opacity: 0; } }
      @keyframes explode { 0%, 14% { transform: translate(-50%, -50%) scale(0); opacity: 0; } 15% { transform: translate(-50%, -50%) scale(1.5); opacity: 1; } 25% { transform: translate(-50%, -50%) scale(2.0); opacity: 0; } 100% { transform: translate(-50%, -50%) scale(2.0); opacity: 0; } }
      .flip-tumble { animation: tumble 1.0s ease-out forwards !important; }
      @keyframes tumble { 0% { transform: scaleX(-1) rotate(0deg) translate(0, 0); } 30% { transform: scaleX(-1) rotate(120deg) translate(150px, -100px); opacity: 1; } 100% { transform: scaleX(-1) rotate(720deg) translate(300px, 50px); opacity: 0; } }
    ")),

    # SYSTEMATIC CRASH LOGIC
    tags$audio(id = "crash-sound", src = "https://assets.mixkit.co/active_storage/sfx/2592/2592-preview.mp3", preload = "auto"),
    tags$script(HTML("
      // Audio unlock
      $(document).one('click', function() {
        var audio = document.getElementById('crash-sound');
        if (audio) {
          audio.play().then(() => { audio.pause(); audio.currentTime = 0; window.audioReady = true; })
          .catch(e => console.log('Audio unlock check:', e));
        }
      });

      function performCrashSequence() {
        var carH = document.querySelector('.car-left');
        var crashAudio = document.getElementById('crash-sound');

        if(carH) carH.classList.remove('flip-tumble');

        // Collision Event (at 0.45s in 3s loop)
        setTimeout(() => {
          if (window.audioReady && crashAudio) {
            crashAudio.currentTime = 0;
            crashAudio.play().catch(e => {});
          }
          if (Math.random() > 0.6 && carH) carH.classList.add('flip-tumble');
        }, 450);
      }

      // Hide splash after 2.8s (just before 3s loop finishes)
      setTimeout(() => {
        var splash = document.getElementById('splash-overlay');
        if(splash) {
          splash.style.opacity = '0';
          setTimeout(() => { splash.style.visibility = 'hidden'; }, 1000);
        }
      }, 2800);

      performCrashSequence();
    "))
  ),

  # Splash Overlay
  div(
    id = "splash-overlay",
    div(
      class = "splash-container",
      div(class = "car-left car-sprite", div(style = "transform: scaleX(-1)", "🏃🏼")),
      div(class = "car-taxi car-sprite", "🚕"),
      div(class = "ambulance car-sprite", "🚑"),
      div(class = "explosion", "💥")
    )
  ),

  # Title panel (Centered and Bold)
  div(
    class = "app-title",
    h2("Accidents in Germany - Dashboard")
  ),

  # KPI Summary Row
  fluidRow(
    class = "kpi-row",
    column(3, uiOutput("kpi_total")),
    column(3, uiOutput("kpi_peak")),
    column(3, uiOutput("kpi_trend")),
    column(3, uiOutput("kpi_hotspot"))
  ),

  # Main layout with sidebar and main panel
  sidebarLayout(
    # ============================================
    # SIDEBAR PANEL - Filters and Controls
    # ============================================
    sidebarPanel(
      width = 3,

      # Dataset selection dropdown
      selectInput(
        "dataset",
        "📊 Select Dataset:",
        choices = c("State Accidents" = "state", "Casualties" = "casualties"),
        selected = "state"
      ),
      hr(),

      # ========================================
      # FILTERS FOR STATE ACCIDENTS DATASET
      # ========================================
      conditionalPanel(
        condition = "input.dataset == 'state'",
        h4("State Accidents Filters"),


        # Filter by specific states
        selectInput(
          "state_filter",
          "Filter by State:",
          choices = c("All States" = "all"),
          multiple = TRUE
        ),

        # Filter by type of injury
        selectInput(
          "injury_type",
          "Type of Injury:",
          choices = c("All" = "all"),
          multiple = TRUE
        ),

        # Filter by location (inside/outside built-up areas)
        selectInput(
          "location_filter",
          "Location:",
          choices = c("All" = "all"),
          multiple = TRUE
        ),

        # Year range slider for temporal filtering
        sliderInput(
          "year_range",
          "Year Range:",
          min = 2011,
          max = 2023,
          value = c(2011, 2023),
          step = 1,
          sep = ""
        )
      ),

      # ========================================
      # FILTERS FOR CASUALTIES DATASET
      # ========================================
      conditionalPanel(
        condition = "input.dataset == 'casualties'",
        h4("Casualties Filters"),


        # Filter by mode of transport
        selectInput(
          "mode_filter",
          "Mode of Transport:",
          choices = c("All" = "all"),
          multiple = TRUE
        ),

        # Filter by outcome (killed, injured, etc.)
        selectInput(
          "outcome_filter",
          "Outcome:",
          choices = c("All" = "all"),
          multiple = TRUE
        ),

        # Filter by sex
        selectInput(
          "sex_filter",
          "Sex:",
          choices = c("All" = "all", "male", "female")
        ),

        # Filter by age group
        selectInput(
          "age_filter",
          "Age Group:",
          choices = c("All" = "all"),
          multiple = TRUE
        )
      ),
      hr(),

      # ========================================
      # MAP SETTINGS (only for State Accidents AND Map Tab)
      # ========================================
      conditionalPanel(
        condition = "input.dataset == 'state' && input.main_tabs == 'map_tab'",
        h4("Map Settings"),

        # Color scheme for choropleth map
        selectInput(
          "color_scheme",
          "Color Scheme:",
          choices = c("YlOrRd", "Blues", "Greens", "Reds", "RdYlGn"),
          selected = "YlOrRd"
        )
      ),
      hr(),
      hr(),

      # Download button for filtered data
      downloadButton("download_data", "Download Filtered Data")
    ),

    # ============================================
    # MAIN PANEL - Visualization Tabs
    # ============================================
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        # ========================================
        # MAP TAB - Always visible but shows message for Casualties
        # ========================================
        tabPanel(
          "Germany Map",
          value = "map_tab", # Add value for easier reference in hideTab/showTab
          br(),
          leafletOutput("germany_map", height = "60vh")
        ),

        # ========================================
        # MONTHLY TRENDS (HEATMAP)
        # ========================================
        tabPanel(
          "Monthly Trends",
          br(),
          plotlyOutput("heatmap_plot", height = "60vh")
        ),

        # ========================================
        # DISTRIBUTION (PIE CHART)
        # ========================================
        tabPanel(
          "Distribution",
          br(),
          plotlyOutput("pie_chart_plot", height = "60vh")
        )
      )
    )
  )
)

# =========================================================================
# SERVER LOGIC
# =========================================================================
server <- function(input, output, session) {
  # ========================================
  # DYNAMIC UI UPDATES
  # ========================================
  # Update filter choices when app loads or data changes
  observe({
    # Update state accident filters
    updateSelectInput(session, "state_filter",
      choices = c("All States" = "all", unique(state_data$state))
    )
    updateSelectInput(session, "injury_type",
      choices = c("All" = "all", unique(state_data$type_of_injury))
    )
    updateSelectInput(session, "location_filter",
      choices = c("All" = "all", unique(state_data$location_of_injury))
    )

    # Update casualties filters
    updateSelectInput(session, "mode_filter",
      choices = c("All" = "all", unique(casualties_data$mode))
    )
    updateSelectInput(session, "outcome_filter",
      choices = c("All" = "all", unique(casualties_data$outcome))
    )
    updateSelectInput(session, "age_filter",
      choices = c("All" = "all", levels(casualties_data$age_group))
    )
  })

  # Toggle map tab visibility based on dataset
  observeEvent(input$dataset, {
    if (input$dataset == "casualties") {
      hideTab(inputId = "main_tabs", target = "map_tab")
      # Switch to histogram tab if we were on the map tab
      if (input$main_tabs == "map_tab") {
        updateTabsetPanel(session, "main_tabs", selected = "📊 Histogram")
      }
    } else {
      showTab(inputId = "main_tabs", target = "map_tab")
    }
  })

  # ========================================
  # REACTIVE DATA FILTERING
  # ========================================
  # Create reactive filtered dataset based on user selections
  filtered_data <- reactive({
    # Start with the selected dataset
    if (input$dataset == "state") {
      df <- state_data

      # Apply state filter
      if (!"all" %in% input$state_filter && !is.null(input$state_filter)) {
        df <- df %>% filter(state %in% input$state_filter)
      }

      # Apply injury type filter
      if (!"all" %in% input$injury_type && !is.null(input$injury_type)) {
        df <- df %>% filter(type_of_injury %in% input$injury_type)
      }

      # Apply location filter
      if (!"all" %in% input$location_filter && !is.null(input$location_filter)) {
        df <- df %>% filter(location_of_injury %in% input$location_filter)
      }

      # Apply year range filter
      if (!is.null(input$year_range)) {
        df <- df %>% filter(year >= input$year_range[1], year <= input$year_range[2])
      }
    } else {
      # Casualties dataset
      df <- casualties_data

      # Apply mode of transport filter
      if (!"all" %in% input$mode_filter && !is.null(input$mode_filter)) {
        df <- df %>% filter(mode %in% input$mode_filter)
      }

      # Apply outcome filter
      if (!"all" %in% input$outcome_filter && !is.null(input$outcome_filter)) {
        df <- df %>% filter(outcome %in% input$outcome_filter)
      }

      # Apply sex filter
      if (input$sex_filter != "all") {
        df <- df %>% filter(sex == input$sex_filter)
      }

      # Apply age group filter
      if (!"all" %in% input$age_filter && !is.null(input$age_filter)) {
        df <- df %>% filter(age_group %in% input$age_filter)
      }

      # Handle "Total" removal (filter out summary rows)
      df <- df %>% filter(mode != "Total", area != "Total", outcome != "Total")
    }

    # Clean data: remove missing values and convert cases to numeric
    df <- df %>% filter(cases != "-", !is.na(cases))
    df$cases <- as.numeric(df$cases)
    df <- df %>% filter(!is.na(cases))

    return(df)
  })

  # ========================================
  # INTERACTIVE CHART FILTERING
  # ========================================
  # Handle clicks on the Pie Chart to update sidebar filters
  observeEvent(event_data("plotly_click", source = "pie_source"), {
    click_data <- event_data("plotly_click", source = "pie_source")
    req(click_data)

    # Get the label (category) clicked from customdata
    clicked_cat <- click_data$customdata
    req(clicked_cat)

    # Identify which variable is currently being plotted in the pie chart
    is_state <- input$dataset == "state"
    group_var <- if (is_state) "type_of_injury" else "mode"
    filter_id <- if (is_state) "injury_type" else "mode_filter"

    # Toggle logic: if the clicked category is already the only one selected, OR 'Others' is clicked, reset to 'all'
    current_val <- input[[filter_id]]

    # Robust string matching
    clicked_str <- trimws(as.character(clicked_cat))

    # If the user clicks 'Others', reset the filter to 'all' to show the full distribution again
    if (clicked_str == "Others") {
      updateSelectInput(session, filter_id, selected = "all")
      return()
    }

    # If the filter is currently set to exactly one item and it matches the click, reset it
    if (length(current_val) == 1 && trimws(as.character(current_val)) == clicked_str) {
      updateSelectInput(session, filter_id, selected = "all")
    } else {
      # Otherwise, set the filter to just this category (drill-down)
      updateSelectInput(session, filter_id, selected = clicked_cat)
    }
  })

  # ========================================
  # GERMANY MAP RENDERING
  # ========================================
  # Create interactive choropleth map of Germany
  output$germany_map <- renderLeaflet({
    # Only render if State Accidents is selected
    req(input$dataset == "state")
    req(filtered_data())

    # Aggregate data by state (sum and average)
    map_data <- filtered_data() %>%
      group_by(state) %>%
      summarise(
        total_cases = sum(cases, na.rm = TRUE),
        avg_cases = mean(cases, na.rm = TRUE),
        .groups = "drop"
      )

    # Join aggregated data with spatial boundaries
    germany_data <- germany_states %>%
      left_join(map_data, by = c("name" = "state"))

    # Create color palette for choropleth
    pal <- colorNumeric(
      palette = input$color_scheme,
      domain = germany_data$total_cases,
      na.color = "#808080" # Gray for missing data
    )

    # Create HTML labels for hover tooltips
    labels <- sprintf(
      "<strong>%s</strong><br/>Total Cases: %s<br/>Average: %.1f",
      germany_data$name,
      format(germany_data$total_cases, big.mark = ","),
      germany_data$avg_cases
    ) %>% lapply(htmltools::HTML)

    # Build the leaflet map
    map <- leaflet(germany_data) %>%
      # Add base map tiles (No labels in background to avoid duplication)
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      # Set view to center on Germany
      setView(lng = 10.4515, lat = 51.1657, zoom = 6) %>%
      # Add colored polygons for each state
      addPolygons(
        fillColor = ~ pal(total_cases),
        weight = 2,
        opacity = 1,
        color = "#333333",
        dashArray = "",
        fillOpacity = 0.7,
        layerId = ~name, # Important for click events
        # Highlight on hover
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        # Show tooltip on hover
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      # Add legend
      addLegend(
        pal = pal,
        values = ~total_cases,
        opacity = 0.7,
        title = "Total Cases",
        position = "bottomright"
      )

    # Add state labels
    centroids <- suppressWarnings(st_centroid(germany_data))
    coords <- st_coordinates(centroids)

    map <- map %>%
      addLabelOnlyMarkers(
        lng = coords[, 1],
        lat = coords[, 2],
        label = germany_data$name,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color" = "#000000",
            "font-size" = "13px",
            "font-weight" = "bold",
            "text-shadow" = "2px 2px 3px rgba(255, 255, 255, 0.9), -1px -1px 2px rgba(255, 255, 255, 0.9)"
          )
        ),
        layerId = germany_data$name,
        group = "stateLabels"
      )


    # Prepare JSON data for the JS panel to ensure all states have data
    js_stats <- germany_data %>%
      sf::st_drop_geometry() %>%
      dplyr::transmute(
        name = as.character(name),
        total = ifelse(is.na(total_cases), "0", format(total_cases, big.mark = ",")),
        average = ifelse(is.na(avg_cases), "0.0", sprintf("%.1f", avg_cases))
      )

    stats_list <- setNames(
      lapply(1:nrow(js_stats), function(i) list(total = js_stats$total[i], average = js_stats$average[i])),
      js_stats$name
    )
    stats_json <- jsonlite::toJSON(stats_list, auto_unbox = TRUE)

    # Add JavaScript for enhanced click interaction with data panel
    map <- map %>%
      htmlwidgets::onRender(sprintf("
        function(el, x) {
          var map = this;
          var clickedLayerId = null;
          var originalStyles = {};
          var stateData = %s;

          console.log('Leaflet R onRender - Robust Data Handling Active');

          // Create data panel element
          var infoPanel = document.createElement('div');
          infoPanel.className = 'state-info-panel';
          infoPanel.id = 'state-info-panel';
          el.appendChild(infoPanel);

          function showDataPanel(stateName, data, pos) {
            var panel = document.getElementById('state-info-panel');
            panel.innerHTML = `
              <span class='close-btn' onclick='document.getElementById(\"state-info-panel\").style.display=\"none\"'>×</span>
              <h3>${stateName}</h3>
              <div class='info-row'>
                <span class='info-label'>Total Cases:</span>
                <span class='info-value'>${data.total}</span>
              </div>
              <div class='info-row'>
                <span class='info-label'>Average Cases:</span>
                <span class='info-value'>${data.average}</span>
              </div>
              <div style='margin-top: 15px; font-size: 12px; color: #666; text-align: center;'>
                Click state again or map to reset
              </div>
            `;

            // Positioning logic
            if (pos) {
              panel.style.top = (pos.y - 50) + 'px';
              panel.style.left = (pos.x + 30) + 'px';
            } else {
              panel.style.top = '20px';
              panel.style.right = '20px';
              panel.style.left = 'auto';
            }

            panel.style.display = 'block';
          }

          function resetAllStates() {
            console.log('JS: Resetting all states to normal');
            map.setView([51.1657, 10.4515], 6); // Reset zoom to Germany
            map.eachLayer(function(layer) {
              if (layer.options && layer.options.layerId) {
                var lid = String(layer.options.layerId).trim();

                if (layer instanceof L.Polygon || (layer.options && layer.options.fillOpacity !== undefined)) {
                  // Restore Styles
                  if (originalStyles[lid]) {
                    layer.setStyle({
                      fillOpacity: originalStyles[lid].fillOpacity,
                      weight: originalStyles[lid].weight,
                      opacity: originalStyles[lid].opacity,
                      color: originalStyles[lid].color
                    });
                  }
                  // Restore Interaction
                  if (layer.getElement()) {
                    layer.getElement().style.pointerEvents = 'auto';
                  }
                }
              }
              // Restore Marker Labels
              if (layer.options && layer.options.pane === 'markerPane') {
                layer.setOpacity(1);
              }
            });
            document.getElementById('state-info-panel').style.display = 'none';
            clickedLayerId = null;
          }

          function isolateState(targetId, pos) {
            console.log('JS: Isolating ->', targetId);
            map.eachLayer(function(layer) {
              if (layer.options && layer.options.layerId) {
                var lid = String(layer.options.layerId).trim();

                if (layer instanceof L.Polygon || (layer.options && layer.options.fillOpacity !== undefined)) {
                  var element = layer.getElement();
                  if (lid === targetId) {
                    // Selected state: Focus style + enable click
                    layer.setStyle({
                      fillOpacity: 1,
                      weight: 8,
                      opacity: 1,
                      color: '#FF0000'
                    });
                    layer.bringToFront();
                    if (element) element.style.pointerEvents = 'auto';

                    // Zoom to state
                    map.fitBounds(layer.getBounds(), { padding: [50, 50] });
                  } else {
                    // Other states: Faded style + DISABLE INTERACTION (stops highlights/tooltips)
                    layer.setStyle({
                      fillOpacity: 0.05,
                      weight: 0.5,
                      opacity: 0.1,
                      color: '#cccccc'
                    });
                    if (element) element.style.pointerEvents = 'none';
                  }
                }
              }
              // Marker Labels
              if (layer.options && layer.options.pane === 'markerPane' && layer.options.layerId) {
                var mlid = String(layer.options.layerId).trim();
                if (mlid !== targetId) {
                  layer.setOpacity(0);
                } else {
                  layer.setOpacity(1);
                  if (layer.bringToFront) layer.bringToFront();
                }
              }
            });

            if (stateData[targetId]) {
              showDataPanel(targetId, stateData[targetId], pos);
            }
          }

          // Initial scan to store original styles and bind click listeners
          map.eachLayer(function(layer) {
            if (layer.options && layer.options.layerId) {
              var lid = String(layer.options.layerId).trim();

              if (layer instanceof L.Polygon || (layer.options && layer.options.fillOpacity !== undefined)) {
                originalStyles[lid] = {
                  fillOpacity: layer.options.fillOpacity,
                  weight: layer.options.weight,
                  opacity: layer.options.opacity,
                  fillColor: layer.options.fillColor,
                  color: layer.options.color || '#333333'
                };
              }

              layer.on('click', function(e) {
                L.DomEvent.stopPropagation(e);
                var containerPoint = map.mouseEventToContainerPoint(e.originalEvent);
                if (clickedLayerId === lid) {
                  resetAllStates();
                } else {
                  isolateState(lid, containerPoint);
                  clickedLayerId = lid;
                }
              });
            }
          });

          // Empty map click resets everything
          map.on('click', function(e) {
            resetAllStates();
          });
        }
      ", stats_json))


    return(map)
  })

  # ========================================
  # MAIN HISTOGRAM PLOT
  # ========================================
  # Create interactive histogram using plotly
  output$histogram_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()

    # Determine which variable and bins to use
    if (input$dataset == "state") {
      var <- input$state_var
      bins <- input$state_bins
    } else {
      var <- input$casualties_var
      bins <- input$casualties_bins
    }

    # Extract the data column
    plot_data <- df[[var]]

    # Apply log transformation if requested
    if (input$log_scale && var == "cases") {
      plot_data <- log10(plot_data + 1)
      xlab <- paste0("log10(", var, " + 1)")
    } else {
      xlab <- var
    }

    # Create the histogram
    p <- plot_ly(
      x = plot_data, type = "histogram", nbinsx = bins,
      marker = list(
        color = "rgb(158, 202, 225)",
        line = list(color = "rgb(8, 48, 107)", width = 1.5)
      )
    ) %>%
      layout(
        title = paste("Distribution of", xlab),
        xaxis = list(title = xlab),
        yaxis = list(title = "Frequency"),
        bargap = 0.1
      )

    # Add density curve overlay if requested
    if (input$show_density && var == "cases") {
      # Calculate kernel density
      dens <- density(plot_data, na.rm = TRUE)

      # Scale density to match histogram height
      hist_data <- hist(plot_data, breaks = bins, plot = FALSE)
      scale_factor <- max(hist_data$counts) / max(dens$y)

      # Add density line
      p <- p %>% add_lines(
        x = dens$x, y = dens$y * scale_factor,
        name = "Density",
        line = list(color = "rgb(255, 127, 14)", width = 2)
      )
    }

    return(p)
  })

  # ========================================
  # MONTHLY TRENDS HEATMAP
  # ========================================
  output$heatmap_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()

    # 1. Aggregate cases by month/year and state/mode
    if (input$dataset == "state") {
      heatmap_data <- df %>%
        group_by(state, month) %>%
        summarise(Total = sum(cases, na.rm = TRUE), .groups = "drop")

      # Ensure months are in order (lowercase to match state_long.csv)
      month_levels <- c(
        "january", "february", "march", "april", "may", "june",
        "july", "august", "september", "october", "november", "december"
      )
      heatmap_data$month <- factor(heatmap_data$month, levels = month_levels)

      p_data <- heatmap_data %>%
        spread(month, Total, fill = 0) %>%
        as.data.frame()

      x_axis_labels <- month_levels
      y_var <- "state"
      plot_title <- "Monthly Accident Intensity by State"
    } else {
      heatmap_data <- df %>%
        group_by(mode, year) %>%
        summarise(Total = sum(cases, na.rm = TRUE), .groups = "drop")

      p_data <- heatmap_data %>%
        spread(year, Total, fill = 0) %>%
        as.data.frame()

      x_axis_labels <- sort(unique(heatmap_data$year))
      y_var <- "mode"
      plot_title <- "Yearly Casualties by Transport Mode"
    }

    # 2. Reshape for heatmap
    row_names <- p_data[[y_var]]
    z_matrix <- as.matrix(p_data[, -1, drop = FALSE])

    # 3. Create Heatmap
    plot_ly(
      x = x_axis_labels,
      y = row_names,
      z = z_matrix,
      type = "heatmap",
      colorscale = "YlOrRd"
    ) %>%
      layout(
        title = plot_title,
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })

  # ========================================
  # KPI REACTIVE LOGIC
  # ========================================

  # Card 1: Total Accidents
  output$kpi_total <- renderUI({
    val <- sum(filtered_data()$cases, na.rm = TRUE)
    div(
      class = "kpi-card",
      div(class = "kpi-title", "Total Accidents"),
      div(class = "kpi-value", format(val, big.mark = ",")),
      div(class = "kpi-trend trend-neutral", "Across filtered period")
    )
  })

  # Card 2: Peak Category
  output$kpi_peak <- renderUI({
    df <- filtered_data()
    group_var <- if (input$dataset == "state") "type_of_injury" else "mode"

    top_val <- df %>%
      group_by(!!sym(group_var)) %>%
      summarise(Total = sum(cases), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      slice(1)

    label <- if (nrow(top_val) > 0) as.character(top_val[[group_var]]) else "N/A"

    div(
      class = "kpi-card",
      div(class = "kpi-title", paste("Peak", gsub("_", " ", group_var))),
      div(class = "kpi-value", style = "font-size: 14px", label),
      div(class = "kpi-trend trend-neutral", "Primary category")
    )
  })

  # Card 3: Growth Trend (% change vs first year in range)
  output$kpi_trend <- renderUI({
    df <- filtered_data()
    years <- sort(unique(df$year))

    if (length(years) < 2) {
      return(div(
        class = "kpi-card",
        div(class = "kpi-title", "Yearly Trend"),
        div(class = "kpi-value", "N/A"),
        div(class = "kpi-trend trend-neutral", "Select multiple years")
      ))
    }

    y_start <- years[1]
    y_end <- years[length(years)]

    val_start <- sum(df$cases[df$year == y_start], na.rm = TRUE)
    val_end <- sum(df$cases[df$year == y_end], na.rm = TRUE)

    pct_change <- ((val_end - val_start) / val_start) * 100
    is_up <- pct_change > 0

    div(
      class = "kpi-card",
      div(class = "kpi-title", paste("Trend:", y_start, "-", y_end)),
      div(class = "kpi-value", sprintf("%.1f%%", pct_change)),
      div(
        class = paste("kpi-trend", if (is_up) "trend-up" else "trend-down"),
        if (is_up) "📈 Increase" else "📉 Decrease"
      )
    )
  })

  # Card 4: Hotspot (Riskiest State / Region)
  output$kpi_hotspot <- renderUI({
    df <- filtered_data()

    # If viewing casualties, hotspot is by Area, if states, hotspot is by State
    hot_var <- if (input$dataset == "state") "state" else "area"

    top_hot <- df %>%
      group_by(!!sym(hot_var)) %>%
      summarise(Total = sum(cases), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      slice(1)

    label <- if (nrow(top_hot) > 0) as.character(top_hot[[hot_var]]) else "N/A"

    div(
      class = "kpi-card",
      div(class = "kpi-title", paste("Hotspot:", hot_var)),
      div(class = "kpi-value", style = "font-size: 16px", label),
      div(class = "kpi-trend trend-up", "Most impacted")
    )
  })

  # ========================================
  # DISTRIBUTION PIE CHART
  # ========================================
  output$pie_chart_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()

    # Identify variable to group by
    group_var <- if (input$dataset == "state") "type_of_injury" else "mode"

    # 1. Aggregate and group into top 6 + others
    agg_data <- df %>%
      group_by(!!sym(group_var)) %>%
      summarise(Total = sum(cases, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total))

    if (nrow(agg_data) > 6) {
      top_cats <- head(agg_data[[group_var]], 6)
      pie_data <- agg_data %>%
        mutate(grp = if_else(!!sym(group_var) %in% top_cats, as.character(!!sym(group_var)), "Others")) %>%
        group_by(grp) %>%
        summarise(Total = sum(Total), .groups = "drop") %>%
        arrange(desc(Total))
    } else {
      pie_data <- agg_data %>% rename(grp = !!sym(group_var))
    }

    # 2. Create Pie Chart
    plot_ly(
      pie_data,
      labels = ~grp, values = ~Total, type = "pie",
      customdata = ~grp, # Pass labels to click event
      source = "pie_source", # For interactive filtering
      textinfo = "label+percent",
      insidetextorientation = "horizontal",
      marker = list(colors = RColorBrewer::brewer.pal(8, "Set3"))
    ) %>%
      layout(
        title = paste("Proportion by", gsub("_", " ", group_var), "(Top 6 + Others)"),
        showlegend = TRUE,
        margin = list(b = 50, t = 80, l = 50, r = 50),
        automargin = TRUE
      )
  })

  # Legacy outputs removed

  # ========================================
  # DOWNLOAD HANDLER
  # ========================================
  # Allow users to download filtered data as CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$dataset, "_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
}

# =========================================================================
# RUN THE APPLICATION
# =========================================================================
shinyApp(ui, server)
