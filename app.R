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
# LOAD DATA
# --------------------------
# Load state-level accident data
state_data <- read_csv("state_long.csv")

# Load casualties data
casualties_data <- read_csv("casualties_long.csv")

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
      .state-info-panel {
        position: absolute;
        background: white;
        padding: 15px 20px;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.3);
        z-index: 1000;
        min-width: 250px;
        font-family: Arial, sans-serif;
        display: none;
        pointer-events: auto; /* Ensure panel itself can be clicked */
      }
      .state-info-panel h3 {
        margin: 0 0 10px 0;
        color: #333;
        font-size: 18px;
        border-bottom: 2px solid #007bff;
        padding-bottom: 5px;
      }
      .state-info-panel .info-row {
        margin: 8px 0;
        display: flex;
        justify-content: space-between;
      }
      .state-info-panel .info-label {
        font-weight: bold;
        color: #555;
      }
      .state-info-panel .info-value {
        color: #007bff;
        font-weight: bold;
      }
      .state-info-panel .close-btn {
        position: absolute;
        top: 5px;
        right: 10px;
        cursor: pointer;
        font-size: 20px;
        color: #999;
        font-weight: bold;
      }
      .state-info-panel .close-btn:hover {
        color: #333;
      }

      /* ============================================================ */
      /* CRASH ANIMATION SYSTEM                                       */
      /* ============================================================ */
      .crash-container {
        display: inline-block;
        position: relative;
        width: 160px;
        height: 60px;
        vertical-align: middle;
        margin-left: 20px;
        overflow: hidden;
      }

      .car-sprite {
        position: absolute;
        font-size: 24px;
        transition: transform 0.3s ease-out;
      }

      .car-left { left: -30px; animation: drive-right 4s infinite linear; }
      .car-person { right: -30px; animation: drive-left-person 4s infinite linear; }

      .explosion {
        position: absolute;
        left: 50%;
        top: 50%;
        transform: translate(-50%, -50%) scale(0);
        font-size: 30px;
        animation: explode 4s infinite linear;
        opacity: 0;
      }

      /* Base Movement */
      @keyframes drive-right {
        0% { left: -40px; opacity: 1; }
        40% { left: 45%; opacity: 1; }
        45% { left: 45%; opacity: 0; }
        100% { left: 45%; opacity: 0; }
      }

      @keyframes drive-left-person {
        0% { right: -40px; opacity: 1; }
        40% { right: 45%; opacity: 1; }
        45% { right: 45%; opacity: 0; }
        100% { right: 45%; opacity: 0; }
      }

      @keyframes explode {
        0%, 39% { transform: translate(-50%, -50%) scale(0); opacity: 0; }
        41% { transform: translate(-50%, -50%) scale(1.3); opacity: 1; }
        55% { transform: translate(-50%, -50%) scale(1.6); opacity: 0; }
        100% { transform: translate(-50%, -50%) scale(1.6); opacity: 0; }
      }

      /* Randomized Flip Effect */
      .flip-tumble {
        animation: tumble 0.8s ease-out forwards !important;
      }

      @keyframes tumble {
        0% { transform: rotate(0deg) translate(0, 0); }
        30% { transform: rotate(120deg) translate(40px, -40px); opacity: 1; }
        100% { transform: rotate(720deg) translate(80px, 20px); opacity: 0; }
      }
    ")),

    # SYSTEMATIC CRASH LOGIC
    tags$audio(id = "crash-sound", src = "https://assets.mixkit.co/active_storage/sfx/2592/2592-preview.mp3", preload = "auto"),
    tags$script(HTML("
      // Interaction unlock for audio
      $(document).one('click', function() {
        var audio = document.getElementById('crash-sound');
        if (audio) {
          audio.play().then(() => { audio.pause(); audio.currentTime = 0; window.audioReady = true; })
          .catch(e => console.log('Audio unlock check:', e));
        }
      });

      function performCrash() {
        var carL = document.querySelector('.car-left');
        var carP = document.querySelector('.car-person');
        var audio = document.getElementById('crash-sound');

        // 1. Reset classes
        if(carL) carL.classList.remove('flip-tumble');
        if(carP) carP.classList.remove('flip-tumble');

        // 2. Schedule Event (Collision at 1.6s)
        setTimeout(() => {
          // Play Sound
          if (window.audioReady && audio) {
            audio.currentTime = 0;
            audio.play().catch(e => {});
          }

          // Randomize Flung Effect (40% chance for person)
          if (Math.random() > 0.6 && carP) carP.classList.add('flip-tumble');

        }, 1600);
      }

      // Initial start and loop (matches 4s CSS animation)
      setTimeout(() => {
        performCrash();
        setInterval(performCrash, 4000);
      }, 0);
    "))
  ),

  # Title panel with crash animation
  titlePanel(
    windowTitle = "Accidents in Germany",
    title = div(
      "Accidents in Germany - Histogram & Map Visualizations",
      div(
        class = "crash-container",
        div(class = "car-left car-sprite", "🚗"),
        div(class = "car-person car-sprite", "🚶"),
        div(class = "explosion", "💥")
      )
    )
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

        # Variable to plot in histogram
        selectInput(
          "state_var",
          "Histogram Variable:",
          choices = c("Cases" = "cases", "Year" = "year", "Month" = "month"),
          selected = "cases"
        ),

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
        ),

        # Number of bins for histogram
        sliderInput(
          "state_bins",
          "Number of Bins:",
          min = 10,
          max = 100,
          value = 30
        )
      ),

      # ========================================
      # FILTERS FOR CASUALTIES DATASET
      # ========================================
      conditionalPanel(
        condition = "input.dataset == 'casualties'",
        h4("Casualties Filters"),

        # Variable to plot in histogram
        selectInput(
          "casualties_var",
          "Histogram Variable:",
          choices = c("Cases" = "cases", "Year" = "year"),
          selected = "cases"
        ),

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
        ),

        # Number of bins for histogram
        sliderInput(
          "casualties_bins",
          "Number of Bins:",
          min = 10,
          max = 100,
          value = 30
        )
      ),
      hr(),

      # ========================================
      # MAP SETTINGS (only for State Accidents)
      # ========================================
      conditionalPanel(
        condition = "input.dataset == 'state'",
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

      # ========================================
      # VISUALIZATION OPTIONS
      # ========================================
      # Add density curve to histogram
      checkboxInput("show_density", "Show Density Curve", FALSE),

      # Apply log scale to cases
      checkboxInput("log_scale", "Log Scale (for Cases)", FALSE),
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
          "🗺️ Germany Map",
          value = "map_tab", # Add value for easier reference in hideTab/showTab
          br(),
          leafletOutput("germany_map", height = "700px")
        ),

        # ========================================
        # HISTOGRAM TAB
        # ========================================
        tabPanel(
          "📊 Histogram",
          br(),
          plotlyOutput("histogram_plot", height = "600px")
        ),

        # ========================================
        # MULTI-HISTOGRAM TAB (Faceted)
        # ========================================
        tabPanel(
          "📈 Multi-Histogram",
          br(),
          plotlyOutput("multi_histogram", height = "600px")
        ),

        # ========================================
        # DATA TABLE TAB
        # ========================================
        tabPanel(
          "📋 Data Table",
          br(),
          DTOutput("data_table")
        ),

        # ========================================
        # SUMMARY STATISTICS TAB
        # ========================================
        tabPanel(
          "ℹ️ Summary Statistics",
          br(),
          verbatimTextOutput("summary_stats")
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
      choices = c("All" = "all", unique(casualties_data$age_group))
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
    }

    # Clean data: remove missing values and convert cases to numeric
    df <- df %>% filter(cases != "-", !is.na(cases))
    df$cases <- as.numeric(df$cases)
    df <- df %>% filter(!is.na(cases))

    return(df)
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
      # Add base map tiles
      addProviderTiles(providers$CartoDB.Positron) %>%
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
    centroids <- st_centroid(germany_data)
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
  # MULTI-HISTOGRAM (FACETED)
  # ========================================
  # Create faceted histograms by category
  output$multi_histogram <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()

    if (input$dataset == "state") {
      # Facet by injury type for state data
      p <- ggplot(df, aes(x = cases)) +
        geom_histogram(bins = input$state_bins, fill = "steelblue", color = "white") +
        facet_wrap(~type_of_injury, scales = "free") +
        theme_minimal() +
        labs(
          title = "Distribution of Cases by Injury Type",
          x = "Cases",
          y = "Frequency"
        )
    } else {
      # Facet by outcome for casualties data
      p <- ggplot(df, aes(x = cases)) +
        geom_histogram(bins = input$casualties_bins, fill = "coral", color = "white") +
        facet_wrap(~outcome, scales = "free") +
        theme_minimal() +
        labs(
          title = "Distribution of Cases by Outcome",
          x = "Cases",
          y = "Frequency"
        )
    }

    # Convert to interactive plotly
    return(ggplotly(p))
  })

  # ========================================
  # DATA TABLE
  # ========================================
  # Display filtered data in interactive table
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      filter = "top"
    )
  })

  # ========================================
  # SUMMARY STATISTICS
  # ========================================
  # Display descriptive statistics
  output$summary_stats <- renderPrint({
    df <- filtered_data()

    # Print header
    cat(strrep("=", 60), "\n")
    cat("SUMMARY STATISTICS\n")
    cat(strrep("=", 60), "\n\n")

    # Basic information
    cat("Dataset:", input$dataset, "\n")
    cat("Number of rows:", nrow(df), "\n")
    cat("Number of columns:", ncol(df), "\n\n")

    # Cases distribution
    cat("Cases Distribution:\n")
    cat(strrep("-", 40), "\n")
    print(summary(df$cases))
    cat("\n")

    # Additional statistics
    cat("Standard Deviation:", sd(df$cases, na.rm = TRUE), "\n")
    cat("Variance:", var(df$cases, na.rm = TRUE), "\n")
    cat("Total Cases:", sum(df$cases, na.rm = TRUE), "\n\n")

    # Dataset-specific breakdowns
    if (input$dataset == "state") {
      # Breakdown by injury type
      cat("Breakdown by Type of Injury:\n")
      cat(strrep("-", 40), "\n")
      print(df %>%
        group_by(type_of_injury) %>%
        summarise(
          Total = sum(cases, na.rm = TRUE),
          Mean = mean(cases, na.rm = TRUE),
          Median = median(cases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Total)))

      # Breakdown by state
      cat("\n\nBreakdown by State:\n")
      cat(strrep("-", 40), "\n")
      print(df %>%
        group_by(state) %>%
        summarise(
          Total = sum(cases, na.rm = TRUE),
          Mean = mean(cases, na.rm = TRUE),
          Median = median(cases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Total)))
    } else {
      # Breakdown by outcome
      cat("Breakdown by Outcome:\n")
      cat(strrep("-", 40), "\n")
      print(df %>%
        group_by(outcome) %>%
        summarise(
          Total = sum(cases, na.rm = TRUE),
          Mean = mean(cases, na.rm = TRUE),
          Median = median(cases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Total)))
    }
  })

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
