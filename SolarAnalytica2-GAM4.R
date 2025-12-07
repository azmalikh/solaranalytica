library(shiny)
library(shinydashboard)
library(dplyr)
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(loo)
library(tidyr)
library(brms)

ui <- dashboardPage(
  dashboardHeader(title = "SolarAnalytica"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "data_input", icon = icon("upload")),
      menuItem("Model Fitting", tabName = "model_fitting", icon = icon("cog")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
    .dropzone {
      border: 2px dashed #ccc;
      padding: 20px;
      text-align: center;
      cursor: pointer;
    }
    .dropzone.hover {
      border-color: #007bff;
    }
  ")),
      tags$script(HTML("
    $(document).ready(function() {
      $('.dropzone').each(function() {
        var dropzone = $(this);
        var fileInput = dropzone.find('input[type=\"file\"]');

        dropzone.on('dragover', function(e) {
          e.preventDefault();
          e.stopPropagation();
          dropzone.addClass('hover');
        });

        dropzone.on('dragleave', function(e) {
          e.preventDefault();
          e.stopPropagation();
          dropzone.removeClass('hover');
        });

        dropzone.on('drop', function(e) {
          e.preventDefault();
          e.stopPropagation();
          dropzone.removeClass('hover');

          var files = e.originalEvent.dataTransfer.files;
          if (files.length) {
            fileInput.prop('files', files);
            fileInput.trigger('change'); // Trigger the change event
          }
        });

        dropzone.on('click', function() {
          fileInput.click();
        });

        fileInput.on('change', function() {
          // You can add code here to handle the file selection
          // For example, display the file name
          var fileName = $(this).val().split('\\\\').pop();
          dropzone.find('p').text('File selected: ' + fileName);
        });
      });
    });
  "))
    ),
    tabItems(
      tabItem(tabName = "data_input",
              h2("Data Input"),
              fluidRow(
                column(width = 6,
                       div(id = "saidi_dropzone", class = "dropzone",
                           p("Drag and drop SAIDI CSV file here"),
                           tags$input(type = "file", id = "saidi_file", name = "saidi_file", style = "display: none;", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                       ),
                       verbatimTextOutput("saidi_info")
                ),
                column(width = 6,
                       div(id = "saifi_dropzone", class = "dropzone",
                           p("Drag and drop SAIFI CSV file here"),
                           tags$input(type = "file", id = "saifi_file", name = "saifi_file", style = "display: none;", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                       ),
                       verbatimTextOutput("saifi_info")
                )
              ),
              fluidRow(
                column(width = 6,
                       div(id = "eens_dropzone", class = "dropzone",
                           p("Drag and drop EENS CSV file here"),
                           tags$input(type = "file", id = "eens_file", name = "eens_file", style = "display: none;", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                       ),
                       verbatimTextOutput("eens_info")
                ),
                column(width = 6,
                       div(id = "solar_dropzone", class = "dropzone",
                           p("Drag and drop Solar Irradiance CSV file here"),
                           tags$input(type = "file", id = "solar_file", name = "solar_file", style = "display: none;", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                       ),
                       verbatimTextOutput("solar_info")
                )
              ),
              fluidRow(
                column(width = 6,
                       div(id = "load_profile_dropzone", class = "dropzone",
                           p("Drag and drop Load Profile CSV file here"),
                           tags$input(type = "file", id = "load_profile_file", name = "load_profile_file", style = "display: none;", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                       ),
                       verbatimTextOutput("load_profile_info")
                ),
                column(width = 6,
                       div(id = "energy_sales_dropzone", class = "dropzone",
                           p("Drag and drop Energy Sales CSV file here"),
                           tags$input(type = "file", id = "energy_sales_file", name = "energy_sales_file", style = "display: none;", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                       ),
                       verbatimTextOutput("energy_sales_info")
                )
              ),
              fluidRow(
                column(width = 6,
                       div(id = "solar_netmeter_dropzone", class = "dropzone",
                           p("Drag and drop Solar Net Meter Reading CSV file here"),
                           tags$input(type = "file", id = "solar_netmeter_file", name = "solar_netmeter_file", style = "display: none;", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                       ),
                       verbatimTextOutput("solar_netmeter_info")
                )
              )
      ),
      tabItem(tabName = "model_fitting",
              h2("Model Fitting"),
              actionButton("run_model", "Run Analysis", class = "btn-primary")
      ),
      tabItem(tabName = "results",
              h2("Results"),
              box(title = "Model Summary", verbatimTextOutput("model_summary")),
              box(title = "Diagnostic Plots", plotOutput("diagnostic_plots")),
              box(title = "Posterior Distribution",
                  selectInput("posterior_index", "Select Index:",
                              choices = c("SAIDI", "SAIFI", "EENS")),
                  selectInput("posterior_parameter", "Select Parameter:",
                              choices = c("s(RESPenetrationLevel)", "s(SolarIrradiance)", "s(LoadProfile)",
                                          "SolarIrradiance", "RESPenetrationLevel", "LoadProfile",
                                          "Intercept")), 
                  plotOutput("posterior_plot")),
              box(title = "Spline Effects",
                  selectInput("spline_index", "Select Index:",
                              choices = c("SAIDI", "SAIFI", "EENS")),
                  checkboxInput("original_scale_spline", "Display on Original Scale", value = FALSE),
                  plotOutput("spline_plot")),
              box(title = "WAIC/LOO Comparison", tableOutput("waic_loo_comparison")) # TableOutput
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Function to read data from specific format
  read_data <- function(file_path, col_name) {
    tryCatch({
      data <- read.csv(file_path, na.strings = c("", "NA", "NULL"), skip = 1)
      # Extract the data from the second column
      data <- data[, 2, drop = FALSE]
      colnames(data) <- col_name # Rename the column
      data <- data %>%
        dplyr::mutate_at(dplyr::vars(dplyr::all_of(col_name)), as.numeric) # Use dplyr::mutate_at
      return(data)
    }, error = function(e) {
      showNotification(paste("Error processing", col_name, "data:", e$message), type = "error")
      return(NULL)
    })
  }
  
  # Reactive values to store file paths
  file_paths <- reactiveValues(
    saidi = NULL,
    saifi = NULL,
    eens = NULL,
    solar = NULL,
    load_profile = NULL,
    energy_sales = NULL,
    solar_netmeter = NULL
  )
  
  # Observer for SAIDI file
  observeEvent(input$saidi_file, {
    file_paths$saidi <- input$saidi_file$datapath
    output$saidi_info <- renderPrint({
      if (!is.null(file_paths$saidi)) {
        cat("SAIDI file loaded:", input$saidi_file$name)
      } else {
        cat("No SAIDI file loaded.")
      }
    })
  })
  
  # Observer for SAIFI file
  observeEvent(input$saifi_file, {
    file_paths$saifi <- input$saifi_file$datapath
    output$saifi_info <- renderPrint({
      if (!is.null(file_paths$saifi)) {
        cat("SAIFI file loaded:", input$saifi_file$name)
      } else {
        cat("No SAIFI file loaded.")
      }
    })
  })
  
  # Observer for EENS file
  observeEvent(input$eens_file, {
    file_paths$eens <- input$eens_file$datapath
    output$eens_info <- renderPrint({
      if (!is.null(file_paths$eens)) {
        cat("EENS file loaded:", input$eens_file$name)
      } else {
        cat("No EENS file loaded.")
      }
    })
  })
  
  # Observer for Solar file
  observeEvent(input$solar_file, {
    file_paths$solar <- input$solar_file$datapath
    output$solar_info <- renderPrint({
      if (!is.null(file_paths$solar)) {
        cat("Solar file loaded:", input$solar_file$name)
      } else {
        cat("No Solar file loaded.")
      }
    })
  })
  
  # Observer for Load Profile file
  observeEvent(input$load_profile_file, {
    file_paths$load_profile <- input$load_profile_file$datapath
    output$load_profile_info <- renderPrint({
      if (!is.null(file_paths$load_profile)) {
        cat("Load Profile file loaded:", input$load_profile_file$name)
      } else {
        cat("No Load Profile file loaded.")
      }
    })
  })
  
  # Observer for Energy Sales file
  observeEvent(input$energy_sales_file, {
    file_paths$energy_sales <- input$energy_sales_file$datapath
    output$energy_sales_info <- renderPrint({
      if (!is.null(file_paths$energy_sales)) {
        cat("Energy Sales file loaded:", input$energy_sales_file$name)
      } else {
        cat("No Energy Sales file loaded.")
      }
    })
  })
  
  # Observer for Solar Netmeter file
  observeEvent(input$solar_netmeter_file, {
    file_paths$solar_netmeter <- input$solar_netmeter_file$datapath
    output$solar_netmeter_info <- renderPrint({
      if (!is.null(file_paths$solar_netmeter)) {
        cat("Solar Netmeter file loaded:", input$solar_netmeter_file$name)
      } else {
        cat("No Solar Netmeter file loaded.")
      }
    })
  })
  
  # Reactive expression to read and process data
  processed_data <- reactive({
    req(file_paths$saidi, file_paths$saifi, file_paths$eens, file_paths$solar, file_paths$energy_sales, file_paths$solar_netmeter, file_paths$load_profile)
    
    # Read data
    saidi_data <- read_data(file_paths$saidi, "SAIDI")
    saifi_data <- read_data(file_paths$saifi, "SAIFI")
    eens_data <- read_data(file_paths$eens, "EENS")
    solar_data <- read_data(file_paths$solar, "SolarIrradiance")
    energy_sales_data <- read_data(file_paths$energy_sales, "EnergySales")
    solar_netmeter_data <- read_data(file_paths$solar_netmeter, "SolarNetmeterReading")
    load_profile_data <- read_data(file_paths$load_profile, "LoadProfile")
    
    # Calculate RES Penetration Level for each month
    RES_Penetration <- tryCatch({
      (solar_netmeter_data$SolarNetmeterReading / energy_sales_data$EnergySales) * 100
    }, error = function(e) {
      showNotification(paste("Error calculating RES Penetration:", e$message), type = "error")
      return(NULL)
    })
    
    # Combine data into a single dataframe
    model_data <- data.frame(
      SAIDI = saidi_data$SAIDI,
      SAIFI = saifi_data$SAIFI,
      EENS = eens_data$EENS,
      SolarIrradiance = solar_data$SolarIrradiance,
      RESPenetrationLevel = RES_Penetration,
      LoadProfile = load_profile_data$LoadProfile
    )
    
    # Data Augmentation using Monte Carlo Simulation
    augment_data <- function(data, n_sim = 988) { # Changed n_sim to 988
      augmented_data <- data.frame(matrix(NA, nrow = n_sim, ncol = ncol(data)))
      colnames(augmented_data) <- colnames(data)
      
      for (col in colnames(data)) {
        mean_val <- mean(data[[col]], na.rm = TRUE)
        sd_val <- sd(data[[col]], na.rm = TRUE)
        
        # Generate random values from a normal distribution
        simulated_values <- rnorm(n_sim, mean = mean_val, sd = sd_val)
        
        # Apply specific conditions based on the column
        if (col %in% c("SAIDI", "SAIFI")) {
          simulated_values[simulated_values < 0] <- 0.1  # Set negative values to 0.1
        } else {
          simulated_values <- abs(simulated_values)  # Take absolute value for other columns
        }
        
        augmented_data[[col]] <- simulated_values
      }
      return(augmented_data)
    }
    
    # Augment the data to reach 1000 samples
    if (nrow(model_data) < 1000) {
      n_sim <- 1000 - nrow(model_data) #Changed 720 to 1000
      augmented_data <- augment_data(model_data, n_sim)
      model_data <- rbind(model_data, augmented_data)
    }
    
    # Standardize variables
    standardize <- function(x) {
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    }
    
    model_data <- model_data %>%
      mutate(
        SolarIrradiance = standardize(SolarIrradiance),
        RESPenetrationLevel = standardize(RESPenetrationLevel),
        LoadProfile = standardize(LoadProfile)
      )
    
    # Remove rows with NA values
    model_data <- na.omit(model_data)
    
    list(model_data = model_data)
    
  })
  
  # Reactive value for model fitting
  model_results <- eventReactive(input$run_model, {
    
    # Show a progress bar
    progress <- shiny::Progress$new()
    progress$set(message = "Running...", value = 0) # Modified initial message
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    data <- processed_data()
    
    if (!is.null(data$model_data)) {
      
      # Define the brms formulas
      # Spline Models
      brm_formula_saidi_spline <- bf(SAIDI ~ s(SolarIrradiance) + s(RESPenetrationLevel) + s(LoadProfile),
                                     family = Gamma(link = "log"))
      
      brm_formula_saifi_spline <- bf(SAIFI ~ s(SolarIrradiance) + s(RESPenetrationLevel) + s(LoadProfile),
                                     family = Gamma(link = "log"))
      
      brm_formula_eens_spline <- bf(EENS ~ s(SolarIrradiance) + s(RESPenetrationLevel) + s(LoadProfile),
                                    family = Gamma(link = "log"))
      
      # Linear Models
      brm_formula_saidi_linear <- bf(SAIDI ~ SolarIrradiance + RESPenetrationLevel + LoadProfile,
                                     family = Gamma(link = "log"))
      
      brm_formula_saifi_linear <- bf(SAIFI ~ SolarIrradiance + RESPenetrationLevel + LoadProfile,
                                     family = Gamma(link = "log"))
      
      brm_formula_eens_linear <- bf(EENS ~ SolarIrradiance + RESPenetrationLevel + LoadProfile,
                                    family = Gamma(link = "log"))
      
      # Fit Bayesian GAMs (Spline Models)
      updateProgress(detail = "Fitting SAIDI Spline Model")
      
      # PRIORS
      prior <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(0, 10)", class = "Intercept"))
      
      model_gamma_saidi_spline <- tryCatch({
        brm(brm_formula_saidi_spline,
            data = data$model_data,
            iter = 2000, warmup = 1000, chains = 4, cores = 4,
            seed = 123,
            prior = prior, # Add prior argument
            control = list(adapt_delta = 0.999, max_treedepth = 15))
      }, error = function(e) {
        showNotification(paste("Error fitting SAIDI Spline Model:", e$message), type = "error")
        return(NULL)
      })
      
      updateProgress(detail = "Fitting SAIFI Spline Model")
      
      # PRIORS
      prior <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(0, 10)", class = "Intercept"))
      
      model_gamma_saifi_spline <- tryCatch({
        brm(brm_formula_saifi_spline,
            data = data$model_data,
            iter = 2000, warmup = 1000, chains = 4, cores = 4,
            seed = 123,
            prior = prior, # Add prior argument
            control = list(adapt_delta = 0.999, max_treedepth = 15))
      }, error = function(e) {
        showNotification(paste("Error fitting SAIFI Spline Model:", e$message), type = "error")
        return(NULL)
      })
      
      updateProgress(detail = "Fitting EENS Spline Model")
      
      # PRIORS
      prior <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(0, 10)", class = "Intercept"))
      
      model_gamma_eens_spline <- tryCatch({
        brm(brm_formula_eens_spline,
            data = data$model_data,
            iter = 2000, warmup = 1000, chains = 4, cores = 4,
            seed = 123,
            prior = prior, # Add prior argument
            control = list(adapt_delta = 0.999, max_treedepth = 15))
      }, error = function(e) {
        showNotification(paste("Error fitting EENS Spline Model:", e$message), type = "error")
        return(NULL)
      })
      
      # Fit Bayesian GLMs (Linear Models)
      updateProgress(detail = "Fitting SAIDI Linear Model")
      
      # PRIORS
      prior <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(0, 10)", class = "Intercept"))
      
      model_gamma_saidi_linear <- tryCatch({
        brm(brm_formula_saidi_linear,
            data = data$model_data,
            iter = 2000, warmup = 1000, chains = 4, cores = 4,
            seed = 123,
            prior = prior, # Add prior argument
            control = list(adapt_delta = 0.999, max_treedepth = 15))
      }, error = function(e) {
        showNotification(paste("Error fitting SAIDI Linear Model:", e$message), type = "error")
        return(NULL)
      })
      
      updateProgress(detail = "Fitting SAIFI Linear Model")
      
      # PRIORS
      prior <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(0, 10)", class = "Intercept"))
      
      model_gamma_saifi_linear <- tryCatch({
        brm(brm_formula_saifi_linear,
            data = data$model_data,
            iter = 2000, warmup = 1000, chains = 4, cores = 4,
            seed = 123,
            prior = prior, # Add prior argument
            control = list(adapt_delta = 0.999, max_treedepth = 15))
      }, error = function(e) {
        showNotification(paste("Error fitting SAIFI Linear Model:", e$message), type = "error")
        return(NULL)
      })
      
      updateProgress(detail = "Fitting EENS Linear Model")
      
      # PRIORS
      prior <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(0, 10)", class = "Intercept"))
      
      model_gamma_eens_linear <-  tryCatch({
        brm(brm_formula_eens_linear,
            data = data$model_data,
            iter = 2000, warmup = 1000, chains = 4, cores = 4,
            seed = 123,
            prior = prior, # Add prior argument
            control = list(adapt_delta = 0.999, max_treedepth = 15))
      }, error = function(e) {
        showNotification(paste("Error fitting EENS Linear Model:", e$message), type = "error")
        return(NULL)
      })
      
      # Calculate WAIC and LOO for all models
      updateProgress(detail = "Calculating WAIC/LOO")
      waic_saidi_spline <- tryCatch({waic(model_gamma_saidi_spline)}, error = function(e) {showNotification(paste("Error calculating waic_saidi_spline:", e$message), type = "error"); return(NULL)})
      loo_saidi_spline <- tryCatch({loo(model_gamma_saidi_spline)}, error = function(e) {showNotification(paste("Error calculating loo_saidi_spline:", e$message), type = "error"); return(NULL)})
      
      waic_saifi_spline <- tryCatch({waic(model_gamma_saifi_spline)}, error = function(e) {showNotification(paste("Error calculating waic_saifi_spline:", e$message), type = "error"); return(NULL)})
      loo_saifi_spline <- tryCatch({loo(model_gamma_saifi_spline)}, error = function(e) {showNotification(paste("Error calculating loo_saifi_spline:", e$message), type = "error"); return(NULL)})
      
      waic_eens_spline <- tryCatch({waic(model_gamma_eens_spline)}, error = function(e) {showNotification(paste("Error calculating waic_eens_spline:", e$message), type = "error"); return(NULL)})
      loo_eens_spline <- tryCatch({loo(model_gamma_eens_spline)}, error = function(e) {showNotification(paste("Error calculating loo_eens_spline:", e$message), type = "error"); return(NULL)})
      
      waic_saidi_linear <- tryCatch({waic(model_gamma_saidi_linear)}, error = function(e) {showNotification(paste("Error calculating waic_saidi_linear:", e$message), type = "error"); return(NULL)})
      loo_saidi_linear <- tryCatch({loo(model_gamma_saidi_linear)}, error = function(e) {showNotification(paste("Error calculating loo_saidi_linear:", e$message), type = "error"); return(NULL)})
      
      waic_saifi_linear <- tryCatch({waic(model_gamma_saifi_linear)}, error = function(e) {showNotification(paste("Error calculating waic_saifi_linear:", e$message), type = "error"); return(NULL)})
      loo_saifi_linear <- tryCatch({loo(model_gamma_saifi_linear)}, error = function(e) {showNotification(paste("Error calculating loo_saifi_linear:", e$message), type = "error"); return(NULL)})
      
      waic_eens_linear <- tryCatch({waic(model_gamma_eens_linear)}, error = function(e) {showNotification(paste("Error calculating waic_eens_linear:", e$message), type = "error"); return(NULL)})
      loo_eens_linear <- tryCatch({loo(model_gamma_eens_linear)}, error = function(e) {showNotification(paste("Error calculating loo_eens_linear:", e$message), type = "error"); return(NULL)})
      
      list(saidi_spline = model_gamma_saidi_spline, saifi_spline = model_gamma_saifi_spline, eens_spline = model_gamma_eens_spline,
           saidi_linear = model_gamma_saidi_linear, saifi_linear = model_gamma_saifi_linear, eens_linear = model_gamma_eens_linear,
           waic_saidi_spline = waic_saidi_spline, loo_saidi_spline = loo_saidi_spline,
           waic_saifi_spline = waic_saifi_spline, loo_saifi_spline = loo_saifi_spline,
           waic_eens_spline = waic_eens_spline, loo_eens_spline = loo_eens_spline,
           waic_saidi_linear = waic_saidi_linear, loo_saidi_linear = loo_saidi_linear,
           waic_saifi_linear = waic_saifi_linear, loo_saifi_linear = loo_saifi_linear,
           waic_eens_linear = waic_eens_linear, loo_eens_linear = loo_eens_linear)
    } else {
      showNotification("Data processing failed. Check data files and format.", type = "error")
      return(NULL)
    }
  })
  
  # Output: Model Summary
  output$model_summary <- renderPrint({
    models <- model_results()
    if(!is.null(models)) {
      cat("SAIDI Spline Model Summary:\n")
      print(summary(models$saidi_spline))
      cat("\nSAIFI Spline Model Summary:\n")
      print(summary(models$saifi_spline))
      cat("\nEENS Spline Model Summary:\n")
      print(summary(models$eens_spline))
      cat("\nSAIDI Linear Model Summary:\n")
      print(summary(models$saidi_linear))
      cat("\nSAIFI Linear Model Summary:\n")
      print(summary(models$saifi_linear))
      cat("\nEENS Linear Model Summary:\n")
      print(summary(models$eens_linear))
    } else {
      cat("Model fitting failed. Check data and model specification.")
    }
  })
  

  # Output: Diagnostic Plots (Trace + Bell Curve/Density)
  output$diagnostic_plots <- renderPlot({
    models <- model_results()
    if (!is.null(models)) {
      
      # Use 'mcmc_combo' to show both Trace (lines) and Density (bell curve)
      # We use 'regex_pars' to automatically find the Intercept and Spline parameters
      
      # 1. SAIDI Diagnostics
      p1 <- mcmc_combo(as.array(models$saidi_spline), 
                       regex_pars = c("Intercept", "^sds_")) + 
        ggtitle("SAIDI Diagnostics")
      
      # 2. SAIFI Diagnostics
      p2 <- mcmc_combo(as.array(models$saifi_spline), 
                       regex_pars = c("Intercept", "^sds_")) + 
        ggtitle("SAIFI Diagnostics")
      
      # 3. EENS Diagnostics
      p3 <- mcmc_combo(as.array(models$eens_spline), 
                       regex_pars = c("Intercept", "^sds_")) + 
        ggtitle("EENS Diagnostics")
      
      # Stack them using gridExtra (Required for combo plots)
      gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
      
    } else {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Run the analysis to see diagnostics.")
    }
  })
  
  output$posterior_plot <- renderPlot({
    req(model_results())
    all_results <- model_results() 
    
    # 1. Get User Inputs
    selected_index <- input$posterior_index       
    selected_parameter <- input$posterior_parameter 
    
    # 2. Select the Specific Model
    is_spline <- grepl("s\\(", selected_parameter)
    
    if (selected_index == "SAIDI") {
      model <- if (is_spline) all_results$saidi_spline else all_results$saidi_linear
    } else if (selected_index == "SAIFI") {
      model <- if (is_spline) all_results$saifi_spline else all_results$saifi_linear
    } else { # EENS
      model <- if (is_spline) all_results$eens_spline else all_results$eens_linear
    }
    
    validate(
      need(!is.null(model), "Model not found. Please run the model first.")
    )
    
    # 3. Convert ONLY the selected model to a data frame
    posterior_draws <- as.data.frame(model)
    
    # 4. Map the Parameter Name
    target_param <- NULL
    
    if (is_spline) {
      # For Splines: Map "s(Var)" -> "sds_sVar_1"
      var_clean <- gsub("s\\((.*)\\)", "\\1", selected_parameter)
      candidates <- c(
        paste0("sds_s", var_clean, "_1"), 
        paste0("sds_s", var_clean)
      )
      target_param <- intersect(candidates, names(posterior_draws))[1]
      
    } else {
      # For Linear: Map "Var" -> "b_Var"
      if (selected_parameter == "Intercept") {
        target_param <- "b_Intercept"
      } else {
        # Check if it's already named correctly, otherwise add prefix
        if (selected_parameter %in% names(posterior_draws)) {
          target_param <- selected_parameter
        } else {
          target_param <- paste0("b_", selected_parameter)
        }
      }
    }
    
    validate(
      need(!is.null(target_param) && target_param %in% names(posterior_draws), 
           paste("Parameter", target_param, "not found in model results."))
    )
    
    # 5. Plot (CHANGED BACK TO 'pars' TO FIX THE WEIRD PLOT)
    mcmc_areas(posterior_draws, 
               pars = target_param,  # <--- CHANGED THIS BACK
               prob = 0.95, 
               point_est = "mean") +
      ggtitle(paste("Posterior Distribution:", selected_parameter)) +
      theme_minimal()
  })
  
  # Output: Spline Effect Plots
  output$spline_plot <- renderPlot({
    models <- model_results()
    if (!is.null(models)) {
      # Get the selected index from the input
      selected_index <- input$spline_index
      
      # Get the value of the checkbox
      original_scale <- input$original_scale_spline
      
      # Select the appropriate model based on the selected index
      selected_model <- switch(selected_index,
                               "SAIDI" = models$saidi_spline,
                               "SAIFI" = models$saifi_spline,
                               "EENS"  = models$eens_spline)
      
      # Check if the model is valid
      if (is.null(selected_model)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
             main = paste("Spline plot for", selected_index, "failed. Check model specification or data."),  # Specific message
             cex.main = 0.9) # Reduced font size to fit the message
        return()
      }
      
      # Generate conditional effects
      conditional_effects_plot <- conditional_effects(selected_model, scale = ifelse(original_scale, "response", "linear"))
      
      # Plot the effects
      plot(conditional_effects_plot, points = TRUE)
    } else {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
           main = "Model fitting failed. Check data files and format.", # General message
           cex.main = 0.9) # Reduced font size to fit the message
    }
  })
  
  # Output: WAIC/LOO Comparison
  output$waic_loo_comparison <- renderTable({
    models <- model_results()
    if (!is.null(models)) {
      data.frame(
        Model = c("SAIDI Spline", "SAIDI Linear", "SAIFI Spline", "SAIFI Linear", "EENS Spline", "EENS Linear"),
        WAIC = c(
          ifelse(is.null(models$waic_saidi_spline), NA, models$waic_saidi_spline$estimates[1, 1]),
          ifelse(is.null(models$waic_saidi_linear), NA, models$waic_saidi_linear$estimates[1, 1]),
          ifelse(is.null(models$waic_saifi_spline), NA, models$waic_saifi_spline$estimates[1, 1]),
          ifelse(is.null(models$waic_saifi_linear), NA, models$waic_saifi_linear$estimates[1, 1]),
          ifelse(is.null(models$waic_eens_spline), NA, models$waic_eens_spline$estimates[1, 1]),
          ifelse(is.null(models$waic_eens_linear), NA, models$waic_eens_linear$estimates[1, 1])
        ),
        LOO = c(
          ifelse(is.null(models$loo_saidi_spline), NA, models$loo_saidi_spline$estimates[1, 1]),
          ifelse(is.null(models$loo_saidi_linear), NA, models$loo_saidi_linear$estimates[1, 1]),
          ifelse(is.null(models$loo_saifi_spline), NA, models$loo_saifi_spline$estimates[1, 1]),
          ifelse(is.null(models$loo_saifi_linear), NA, models$loo_saifi_linear$estimates[1, 1]),
          ifelse(is.null(models$loo_eens_spline), NA, models$loo_eens_spline$estimates[1, 1]),
          ifelse(is.null(models$loo_eens_linear), NA, models$loo_eens_linear$estimates[1, 1])
        )
      )
    } else {
      NULL
    }
  })
}

shinyApp(ui = ui, server = server)