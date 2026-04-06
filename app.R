## =========================
##Oncoprint Shiny App
## Allows file upload, parameter configuration, and visualization
## ERMIS I. MICHAIL DELOPOULOS 4/6/2026
## =========================

## Load packages 
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(shinyjs)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(data.table)
  library(DT)
  library(RColorBrewer)
  library(circlize)
  
  # Load plotting packages 
  library(grid)
  library(ComplexHeatmap)
})

## ========================================
##                  UI 
## ========================================


ui <- fluidPage(
  useShinyjs(),
  
  titlePanel(
    div(
      h2("Oncoprint Generator 🧬"),
      h4(
        "Upload data, configure parameters, and generate oncoprints",
        style = "margin-left: 5px;"
      )
    )
  ),
  
  tabsetPanel(
    id = "main_tabs",
    
    ## ========================================
    ## TAB 1: Instructions 
    ## ========================================
    tabPanel(
      "📖 Instructions",
      
      sidebarLayout(
        ## SIDEBAR: File Uploads ONLY
        sidebarPanel(
          width = 3,
          
          h3("📁 Data Files"),
          #br(),
          
          fileInput(
            "file_clinical",
            "Clinical Data",
            accept = ".csv"
          ),
          fileInput(
            "file_mutations",
            "Mutation SNV Data",
            accept = ".csv"
          ),
          #hr(),
          
         # helpText("Optional"),
          fileInput(
            "file_cnv",
            "Mutation CNV Data (Optional)",
            accept = ".csv"
          ),
          #helpText("Optional"),
          fileInput(
            "file_fusions",
            "Fusion Data (Optional)",
            accept = ".csv"
          ),
         # helpText("Optional"), 
          fileInput(
            "file_clusters",
            "Cluster Assignments (Optional)",
            accept = ".csv"
          )
        ),
        
        ## MAIN: Instructions content
        mainPanel(
          width = 9,
          br(),
          h3("Welcome to the Oncoprint Generator!"),
          br(),
          
          h4("📁 Step 1: Upload Your Data"),
          tags$ul(
            tags$li(tags$b("Clinical Data:"), " Patient information with Pt_ID and Diagnosis_Type"),
            tags$li(tags$b("Mutation Data:"), " Genetic alterations with Pt_ID, Gene, Type, and CellType"),
            tags$li(tags$b("Fusion Data:"), " (Optional) Gene fusion events"),
            tags$li(tags$b("CNV Data:"), " (Optional) Copy number variations"),
            tags$li(tags$b("Cluster Assignments:"), " (Optional) Pre-computed methylation clusters")
          ),
          
          h4("⚙️ Step 2: Configure Parameters"),
          #p("Go to the", tags$b("Oncoprint tab"), "to configure visualization parameters."),
          tags$ul(
            tags$li(tags$b("Go to the 'Oncoprint tab':"), "configure visualization parameters"),
          ),
          
          h4("🎨 Step 3: Generate Visualization"),
          tags$ul(
            tags$li(tags$b("Click 'Generate Oncoprint':"), " create your visualization"),
          ),
         # p("Click 'Generate Oncoprint' in the Oncoprint tab to create your visualization"),
          
          h4("💾 Step 4: Download Results"),
          tags$ul(
            tags$li(tags$b("PDF:"), " High-resolution figure"),
          ),
          # 
          # hr(),
          # 
          # h4("📊 Data Format Requirements"),
          # 
          # h5("Clinical Data"),
          # tags$pre(
          #   "Pt_ID\tAge\tSex\tDiagnosis_Type\n",
          #   "PBB---\t15\tMale\tColorectal Carcinoma"
          # ),
          # 
          # h5("Mutation Data"),
          # tags$pre(
          #   "Pt_ID\tGene\tType\tCellType\tDiagnosis_Type\n",
          #   "PBB---\tAPC\tTruncation\tGermline\tColorectal Carcinoma"
          # ),
          # 
          # h5("CNV Data"),
          # tags$pre(
          #   "Pt_ID\tGene\tEvent\tCellType\n",
          #   "PBC---\tSDHB\tLoss\tGermline"
          # ),
          # 
          # h5("Fusions Data"),
          # tags$pre(
          #   "Pt_ID\tFusion\tTier\tGene_5\tGene_3\n",
          #   "PBC---\tNCOA4::RET\tTier 1-2\tNCOA4\tRET"
          # )
        )
      )
    ),
    
    ## ========================================
    ## TAB 2: Data Preview
    ## ========================================
    tabPanel(
      "📋 Data Preview",
      br(),
      h3("Uploaded Data Summary"),
      
      fluidRow(
        column(3, uiOutput("data_box_clinical")),
        column(3, uiOutput("data_box_mutations")),
        column(3, uiOutput("data_box_cnv")),
        column(3, uiOutput("data_box_fusions"))
      ),
      
      hr(),
      
      h4("Clinical Data"),
      DT::dataTableOutput("preview_clinical"),
      
      hr(),
      
      h4("Mutation Data"),
      DT::dataTableOutput("preview_mutations"),
      
      hr(),
      
      h4("Fusion Data"),
      DT::dataTableOutput("preview_fusions"),
      
      hr(),
      
      h4("CNV Data"),
      DT::dataTableOutput("preview_cnv")
    ),
    
    ## ========================================
    ## TAB 3: Oncoprint ( parameters sidebar)
    ## ========================================
    tabPanel(
      "🎨 Oncoprint",
      
      sidebarLayout(
        ## SIDEBAR: Parameters & Action Buttons
        sidebarPanel(
          width = 3,
          
          h3("⚙️ Parameters"),
          
          # Diagnosis selection
          uiOutput("ui_diagnosis_select"),
          
          # Gene ordering section
          uiOutput("ui_gene_order_section"),
          
          # Gene family options
          checkboxInput("use_gene_families", 
                       "Group by gene families", 
                       value = FALSE),
          
          conditionalPanel(
            condition = "input.use_gene_families == true",
            checkboxInput("group_singletons", 
                         "Group small families as 'Other'", 
                         value = TRUE),
            numericInput("min_family_size",
                        "Minimum family size:",
                        value = 2, min = 1, max = 10)
          ),
          
          # Clustering strategy
          selectInput("clustering_strategy",
                     "Clustering Strategy:",
                     choices = c("None" = "NONE",
                               "File-based" = "FILE",
                               "Mutation-based" = "MUTATION",
                               "Combined (Mutation + CNV)" = "COMBINED",
                               "Gene-order priority" = "GENE_ORDER"),
                     selected = "COMBINED"),
          
          # Show mutation counts checkbox
          checkboxInput("show_mutation_counts",
                       "Show mutation counts on right",
                       value = TRUE),
          
          conditionalPanel(
            condition = "input.clustering_strategy == 'MUTATION' || input.clustering_strategy == 'COMBINED'",
            selectInput("cluster_method",
                       "Clustering Method:",
                       choices = c("Ward D2" = "ward.D2",
                                 "Complete" = "complete",
                                 "Average" = "average"),
                       selected = "ward.D2"),
            numericInput("num_clusters",
                        "Number of Clusters (NULL = auto):",
                        value = NA, min = 2, max = 20),
            checkboxInput("show_dendrogram",
                         "Show dendrogram",
                         value = TRUE),
            conditionalPanel(
              condition = "input.show_dendrogram == true",
              numericInput("dendrogram_height",
                          "Dendrogram height (mm):",
                          value = 5, min = 1, max = 50)
            )
          ),
          
          hr(),
          
          # Clinical Annotation Section
          h4("🏥 Clinical Annotations"),
          helpText("Select clinical variables to show as heatmap above oncoprint:"),
          uiOutput("ui_clinical_annotation_selector"),
          
          hr(),
          
          # Sizing strategy
          h4("📐 Size & Layout"),
          selectInput("sizing_strategy",
                     "Sizing Strategy:",
                     choices = c("Fixed Cell Size" = "FIXED_CELLS",
                               "Fixed PDF Size" = "FIXED_PDF",
                               "Balanced" = "BALANCED"),
                     selected = "FIXED_CELLS"),
          
          numericInput("cell_width", "Cell Width (mm):", value = 5, min = 2, max = 20),
          numericInput("cell_height", "Cell Height (mm):", value = 9, min = 3, max = 20),
          
          # Manual PDF size override for Fixed Cell Size (for large datasets)
          conditionalPanel(
            condition = "input.sizing_strategy == 'FIXED_CELLS'",
            checkboxInput("override_pdf_size", 
                         "Override PDF Size", 
                         value = FALSE),
            helpText("Check this to manually set PDF dimensions for large oncoprints."),
            conditionalPanel(
              condition = "input.override_pdf_size",
              numericInput("override_pdf_width", "PDF Width (inches):", 
                          value = 20, min = 10, max = 50),
              numericInput("override_pdf_height", "PDF Height (inches):", 
                          value = 24, min = 10, max = 50),
              helpText("💡 Tip: For 150+ patients, try 30-40 inches width")
            )
          ),
          
          conditionalPanel(
            condition = "input.sizing_strategy != 'FIXED_CELLS'",
            numericInput("target_pdf_width", "Target PDF Width (inches):", 
                        value = 9, min = 5, max = 20),
            numericInput("target_pdf_height", "Target PDF Height (inches):", 
                        value = 14, min = 5, max = 30)
          ),
          
          hr(),
          
          # Action buttons
          actionButton("btn_generate", 
                      "🎨 Generate Oncoprint", 
                      class = "btn-primary btn-lg btn-block",
                      icon = icon("play")),
          
          br(),
          
          downloadButton("btn_download_pdf", 
                        "📄 Download PDF", 
                        class = "btn-success btn-block"),
          
          # downloadButton("btn_download_data",
          #               "💾 Download Data",
          #               class = "btn-info btn-block")
        ),
        
        ## MAIN: Oncoprint Visualization
        mainPanel(
          width = 9,
          br(),
          uiOutput("status_message"),
          hr(),
          plotOutput("plot_oncoprint", 
                    width = "100%", 
                    height = "900px"),
          hr(),
          verbatimTextOutput("plot_dimensions")
        )
      )
    ),
    
    ## ========================================
    ## TAB 4: Statistics
    ## ========================================
    tabPanel(
      "📊 Statistics",
      br(),
      h3("Dataset Statistics"),
      
      fluidRow(
        column(6, plotOutput("plot_gene_freq", height = "400px")),
        column(6, plotOutput("plot_mutation_types", height = "400px"))
      ),
      
      hr(),
      
      h4("Summary Table"),
      DT::dataTableOutput("summary_table")
    )
  )
)


server <- function(input, output, session) {
  
  ## ========================================
  ## Reactive Values
  ## ========================================
  
  rv <- reactiveValues(
    clinical_data = NULL,
    mutation_data = NULL,
    fusion_data = NULL,
    cnv_data = NULL,
    cluster_data = NULL,
    
    # Processed data
    mutation_matrix = NULL,
    cnv_mat = NULL,
    heatmap_object = NULL,
    
    # Metadata
    somatic_map = NULL,
    germline_map = NULL,
    cnv_somatic_map = NULL,
    cnv_germline_map = NULL,
    type_colors = NULL,
    CNV_COLORS = NULL,
    filtered_patient_dx = NULL,
    
    # Gene ordering
    gene_order = NULL,
    
    # Status
    data_loaded = FALSE,
    plot_ready = FALSE
  )
  
  ## ========================================
  ## File Upload Handlers
  ## ========================================
  
  observeEvent(input$file_clinical, {
    req(input$file_clinical)
    tryCatch({
      rv$clinical_data <- read.csv(input$file_clinical$datapath, fileEncoding = "UTF-8")
      
      # Normalize column names
      if ("Diagnosis" %in% names(rv$clinical_data)) {
        rv$clinical_data <- rv$clinical_data %>% 
          select(-Diagnosis) %>% 
          rename(Diagnosis = Diagnosis_Type)
      } else if ("Diagnosis_Type" %in% names(rv$clinical_data)) {
        rv$clinical_data <- rv$clinical_data %>% rename(Diagnosis = Diagnosis_Type)
      }
      
      showNotification("✓ Clinical data loaded", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading clinical data:", e$message), type = "error")
    })
  })
  
  observeEvent(input$file_mutations, {
    req(input$file_mutations)
    tryCatch({
      rv$mutation_data <- read.csv(input$file_mutations$datapath, fileEncoding = "UTF-8")
      
      # Normalize column names
      if ("Diagnosis" %in% names(rv$mutation_data)) {
        rv$mutation_data <- rv$mutation_data %>% 
          select(-any_of("Diagnosis")) %>% 
          rename(Diagnosis = Diagnosis_Type)
      } else if ("Diagnosis_Type" %in% names(rv$mutation_data)) {
        rv$mutation_data <- rv$mutation_data %>% rename(Diagnosis = Diagnosis_Type)
      }
      
      # Fill missing gene families
      if ("Gene_Family" %in% names(rv$mutation_data)) {
        rv$mutation_data$Gene_Family[is.na(rv$mutation_data$Gene_Family) | 
                                     trimws(rv$mutation_data$Gene_Family) == ""] <- "Unassigned family"
      }
      
      showNotification("✓ Mutation data loaded", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading mutation data:", e$message), type = "error")
    })
  })
  
  observeEvent(input$file_fusions, {
    req(input$file_fusions)
    tryCatch({
      rv$fusion_data <- read.csv(input$file_fusions$datapath, fileEncoding = "UTF-8")
      
      if ("Diagnosis_Type" %in% names(rv$fusion_data)) {
        rv$fusion_data <- rv$fusion_data %>% 
          select(-any_of("Diagnosis")) %>% 
          rename(Diagnosis = Diagnosis_Type)
      }
      
      showNotification("✓ Fusion data loaded", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading fusion data:", e$message), type = "error")
    })
  })
  
  observeEvent(input$file_cnv, {
    req(input$file_cnv)
    tryCatch({
      rv$cnv_data <- fread(input$file_cnv$datapath)
      showNotification("✓ CNV data loaded", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading CNV data:", e$message), type = "error")
    })
  })
  
  observeEvent(input$file_clusters, {
    req(input$file_clusters)
    tryCatch({
      rv$cluster_data <- read.csv(input$file_clusters$datapath, stringsAsFactors = FALSE)
      showNotification("✓ Cluster assignments loaded", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading cluster data:", e$message), type = "error")
    })
  })
  
  ## ========================================
  ## Dynamic UI: Diagnosis Selection
  ## ========================================
  
  output$ui_diagnosis_select <- renderUI({
    req(rv$clinical_data)
    
    if (!("Diagnosis" %in% names(rv$clinical_data))) {
      return(
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          h4("Error: No Diagnosis Column"),
          p("Your clinical data is missing the 'Diagnosis' or 'Diagnosis_Type' column."),
          p("Please check your data format.")
        )
      )
    }
    
    diagnoses <- unique(rv$clinical_data$Diagnosis)
    diagnoses <- diagnoses[!is.na(diagnoses) & diagnoses != ""]
    
    if (length(diagnoses) == 0) {
      return(
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          h4("No Valid Diagnoses"),
          p("Your clinical data has no valid diagnosis values."),
          p("Please check your data.")
        )
      )
    }
    
    pickerInput(
      "target_diagnosis",
      "Select Diagnosis:",
      choices = diagnoses,
      selected = diagnoses[1],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE
      )
    )
  })
  
  ## ========================================
  ## Dynamic UI: Gene Ordering
  ## ========================================
  
  # Detect available genes from the actual mutation matrix
  available_genes <- reactive({
    # Require mutation matrix to exist (after first generation)
    if (is.null(rv$mutation_matrix)) {
      return(character(0))
    }
    
    # Get genes from the actual matrix that was created
    genes <- rownames(rv$mutation_matrix)
    genes <- genes[!is.na(genes) & genes != ""]
    genes <- sort(genes)
    
    return(genes)
  })
  
  # Initialize gene order when mutation matrix is created/updated
  observeEvent(rv$mutation_matrix, {
    # Only auto-update if gene ordering is NOT enabled
    if (is.null(input$enable_gene_ordering) || !input$enable_gene_ordering) {
      if (!is.null(rv$mutation_matrix)) {
        genes <- rownames(rv$mutation_matrix)
        genes <- genes[!is.na(genes) & genes != ""]
        rv$gene_order <- sort(genes)
      }
    }
  })
  
  # Render gene ordering UI
  output$ui_gene_order_section <- renderUI({
    # Explicitly access rv$mutation_matrix to establish reactive dependency
    mutation_matrix <- rv$mutation_matrix
    
    # Check if matrix exists
    if (is.null(mutation_matrix)) {
      return(
        div(
          helpText("💡 Generate an oncoprint first to enable gene ordering"),
          hr()
        )
      )
    }
    
    # Get genes from matrix
    genes <- rownames(mutation_matrix)
    genes <- genes[!is.na(genes) & genes != ""]
    genes <- sort(genes)
    
    if (length(genes) == 0) {
      return(
        div(
          helpText("⚠️ No genes found in matrix"),
          hr()
        )
      )
    }
    
    div(
      checkboxInput("enable_gene_ordering",
                   "📋 Custom Gene Order",
                   value = FALSE),
      helpText(paste("Genes in current oncoprint:", length(genes))),
      helpText("Note: If you change diagnosis,you need to generate oncoprint again before seeing the correct genes."),
      helpText("Note: Dendrogram and mutation counts will also follow custom order"),
      
      conditionalPanel(
        condition = "input.enable_gene_ordering == true",
        
        div(
          style = "margin-top: 10px; margin-bottom: 10px;",
          helpText(HTML("<b>Select a gene and use buttons to reorder:</b>"))
        ),
        
        # Gene list selector
        selectInput("custom_gene_order",
                   NULL,
                   choices = genes,
                   selected = NULL,
                   multiple = FALSE,
                   size = 10,
                   selectize = FALSE,
                   width = "100%"),
        
        # Movement buttons
        div(
          style = "display: flex; gap: 5px; margin-bottom: 10px; flex-wrap: wrap;",
          actionButton("btn_move_top", "⬆️ Top", class = "btn-sm btn-default", style = "flex: 1;"),
          actionButton("btn_move_up", "↑ Up", class = "btn-sm btn-default", style = "flex: 1;")
        ),
        div(
          style = "display: flex; gap: 5px; margin-bottom: 10px; flex-wrap: wrap;",
          actionButton("btn_move_down", "↓ Down", class = "btn-sm btn-default", style = "flex: 1;"),
          actionButton("btn_move_bottom", "⬇️ Bottom", class = "btn-sm btn-default", style = "flex: 1;")
        ),
        
        # Utility buttons
        div(
          style = "display: flex; gap: 5px; flex-wrap: wrap;",
          actionButton("btn_sort_alpha", "🔤 A-Z Sort", class = "btn-sm btn-info", style = "flex: 1;")
        )
      ),
      
      hr()
    )
  })
  
  # Move gene to top
  observeEvent(input$btn_move_top, {
    req(input$custom_gene_order)
    req(rv$gene_order)
    
    selected <- input$custom_gene_order
    current_order <- rv$gene_order
    
    # Remove selected and add to beginning
    new_order <- c(selected, setdiff(current_order, selected))
    rv$gene_order <- new_order
    
    updateSelectInput(session, "custom_gene_order",
                     choices = new_order,
                     selected = selected)
  })
  
  # Move gene up one position
  observeEvent(input$btn_move_up, {
    req(input$custom_gene_order)
    req(rv$gene_order)
    
    selected <- input$custom_gene_order
    current_order <- rv$gene_order
    idx <- which(current_order == selected)
    
    if (idx > 1) {
      # Swap with previous
      new_order <- current_order
      new_order[idx] <- current_order[idx - 1]
      new_order[idx - 1] <- selected
      rv$gene_order <- new_order
      
      updateSelectInput(session, "custom_gene_order",
                       choices = new_order,
                       selected = selected)
    }
  })
  
  # Move gene down one position
  observeEvent(input$btn_move_down, {
    req(input$custom_gene_order)
    req(rv$gene_order)
    
    selected <- input$custom_gene_order
    current_order <- rv$gene_order
    idx <- which(current_order == selected)
    
    if (idx < length(current_order)) {
      # Swap with next
      new_order <- current_order
      new_order[idx] <- current_order[idx + 1]
      new_order[idx + 1] <- selected
      rv$gene_order <- new_order
      
      updateSelectInput(session, "custom_gene_order",
                       choices = new_order,
                       selected = selected)
    }
  })
  
  # Move gene to bottom
  observeEvent(input$btn_move_bottom, {
    req(input$custom_gene_order)
    req(rv$gene_order)
    
    selected <- input$custom_gene_order
    current_order <- rv$gene_order
    
    # Remove selected and add to end
    new_order <- c(setdiff(current_order, selected), selected)
    rv$gene_order <- new_order
    
    updateSelectInput(session, "custom_gene_order",
                     choices = new_order,
                     selected = selected)
  })
  
  # Sort alphabetically
  observeEvent(input$btn_sort_alpha, {
    req(rv$gene_order)
    
    new_order <- sort(rv$gene_order)
    rv$gene_order <- new_order
    
    if (!is.null(input$custom_gene_order)) {
      updateSelectInput(session, "custom_gene_order",
                       choices = new_order,
                       selected = input$custom_gene_order)
    } else {
      updateSelectInput(session, "custom_gene_order",
                       choices = new_order)
    }
  })
  
## ========================================
  ## Dynamic UI: Clinical Annotation Selector
  ## ========================================
  
  output$ui_clinical_annotation_selector <- renderUI({
    if (is.null(rv$clinical_data)) {
      return(helpText(" Generate an oncoprint first"))
    }
    
    # Get available clinical columns (exclude Pt_ID)
    available_cols <- setdiff(names(rv$clinical_data), "Pt_ID")
    
    if (length(available_cols) == 0) {
      return(helpText("No clinical columns available"))
    }
    
    # Default selections if columns exist
    default_select <- intersect(c("Age", "Sex", "Diagnosis"), available_cols)
    
    # Create checkbox group for selecting columns
    div(
      style = "max-height: 250px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 4px;",
      checkboxGroupInput(
        "clinical_annotation_cols",
        NULL,
        choices = available_cols,
        selected = default_select
      )
    )
  })
  
  ## ========================================
  ## Data Preview Outputs
  ## ========================================
  
  output$data_box_clinical <- renderUI({
    if (is.null(rv$clinical_data)) {
      div(class = "alert alert-warning", 
          icon("exclamation-triangle"),
          " No clinical data uploaded")
    } else {
      n_diagnoses <- length(unique(rv$clinical_data$Diagnosis[!is.na(rv$clinical_data$Diagnosis)]))
      div(
        class = "alert alert-success",
        h4(icon("check-circle"), " ", nrow(rv$clinical_data), " patients"),
        p(n_diagnoses, " diagnosis types")
      )
    }
  })
  
  output$data_box_mutations <- renderUI({
    if (is.null(rv$mutation_data)) {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"),
          " No mutation data uploaded")
    } else {
      n_genes <- length(unique(rv$mutation_data$Gene))
      div(
        class = "alert alert-success",
        h4(icon("check-circle"), " ", nrow(rv$mutation_data), " mutations"),
        p(n_genes, " unique genes")
      )
    }
  })
  
  output$data_box_cnv <- renderUI({
    if (is.null(rv$cnv_data) || nrow(rv$cnv_data) == 0) {
      div(class = "alert alert-info",
          icon("info-circle"),
          " No CNV data (optional)")
    } else {
      n_genes <- length(unique(rv$cnv_data$Gene))
      div(
        class = "alert alert-success",
        h4(icon("check-circle"), " ", nrow(rv$cnv_data), " CNV events"),
        p(n_genes, " genes affected")
      )
    }
  })
  
  output$data_box_fusions <- renderUI({
    if (is.null(rv$fusion_data) || nrow(rv$fusion_data) == 0) {
      div(class = "alert alert-info",
          icon("info-circle"),
          " No fusion data (optional)")
    } else {
      n_genes <- length(unique(rv$fusion_data$Gene_3))
      div(
        class = "alert alert-success",
        h4(icon("check-circle"), " ", nrow(rv$fusion_data), " fusions"),
        p(n_genes, " genes involved")
      )
    }
  })
  
  output$preview_clinical <- DT::renderDataTable({
    req(rv$clinical_data)
    DT::datatable(rv$clinical_data, 
                 options = list(pageLength = 10, scrollX = TRUE, scrollY = "400px"))
  })
  
  output$preview_mutations <- DT::renderDataTable({
    req(rv$mutation_data)
    DT::datatable(rv$mutation_data, 
                 options = list(pageLength = 10, scrollX = TRUE, scrollY = "400px"))
  })
  
  output$preview_fusions <- DT::renderDataTable({
    if (is.null(rv$fusion_data) || nrow(rv$fusion_data) == 0) {
      DT::datatable(data.frame(Message = "No fusion data uploaded"))
    } else {
      DT::datatable(rv$fusion_data, 
                   options = list(pageLength = 10, scrollX = TRUE, scrollY = "400px"))
    }
  })
  
  output$preview_cnv <- DT::renderDataTable({
    if (is.null(rv$cnv_data)) {
      DT::datatable(data.frame(Message = "No CNV data uploaded"))
    } else {
      DT::datatable(rv$cnv_data, 
                   options = list(pageLength = 10, scrollX = TRUE, scrollY = "400px"))
    }
  })
  
  ## ========================================
  ## Main Processing Function
  ## ========================================
  
  observeEvent(input$btn_generate, {
    
    # Comprehensive validation
    if (is.null(rv$clinical_data)) {
      showNotification("Please upload clinical data first!", 
                      type = "error", duration = 5)
      return()
    }
    
    if (is.null(rv$mutation_data)) {
      showNotification("Please upload mutation data first!", 
                      type = "error", duration = 5)
      return()
    }
    
    if (is.null(input$target_diagnosis)) {
      showNotification("Please wait for diagnosis options to load, then select a diagnosis!", 
                      type = "warning", duration = 5)
      return()
    }
    
    if (length(input$target_diagnosis) == 0) {
      showNotification("Please select at least one diagnosis!", 
                      type = "error", duration = 5)
      return()
    }
    
    # Show progress
    withProgress(message = 'Generating Oncoprint...', value = 0, {
      
      tryCatch({
        
        incProgress(0.1, detail = "Processing data...")
        
        ## Merge fusion data 
        data <- rv$mutation_data
        
        if (!is.null(rv$fusion_data)) {
          fusions_keep <- rv$fusion_data %>%
            filter(Intergenic == "N", Tier %in% c("Tier 1-2", "Tier 1", "Tier 2", "Tier 3"))
          
          fusion_rows <- fusions_keep %>%
            transmute(
              Pt_ID,
              Gene = Gene_3,
              Type = "Fusion",
              CellType = "Somatic",
              Patient_Type = "Somatic",
              Gene_Family = "3'partner Fusion",
              Diagnosis
            )
          
          data <- bind_rows(data, fusion_rows)
        }
        
        incProgress(0.2, detail = "Filtering to selected diagnosis...")
        
        ## Filter to target diagnosis
        if (is.null(input$target_diagnosis) || length(input$target_diagnosis) == 0) {
          showNotification("Please select at least one diagnosis!", 
                          type = "error", duration = 5)
          return()
        }
        
        pts_filtered <- rv$clinical_data %>% 
          filter(Diagnosis %in% input$target_diagnosis)
        
        if (nrow(pts_filtered) == 0) {
          showNotification(
            paste("No patients found for diagnosis:", paste(input$target_diagnosis, collapse = ", ")),
            type = "error", duration = 5
          )
          return()
        }
        
        data_filtered <- data %>% 
          filter(Pt_ID %in% pts_filtered$Pt_ID)
        
        if (nrow(data_filtered) == 0) {
          showNotification("No mutations found for selected diagnosis!", 
                          type = "error", duration = 5)
          return()
        }
        
        incProgress(0.3, detail = "Building mutation matrix...")
        
        ## Build mutation objects using original function
        mut_obj <- build_mutation_objects(
          data_filtered,
          pts_filtered,
          use_gene_families = input$use_gene_families,
          group_singletons = input$group_singletons,
          min_family_size = input$min_family_size
        )
        
        # Store in reactive values
        rv$mutation_matrix <- mut_obj$mutation_matrix
        rv$somatic_map <- mut_obj$somatic_map
        rv$germline_map <- mut_obj$germline_map
        rv$type_colors <- mut_obj$type_colors
        rv$filtered_patient_dx <- mut_obj$filtered_patient_dx
        
        incProgress(0.5, detail = "Processing CNV data...")
        
        ## Process CNV data if available
        if (!is.null(rv$cnv_data) && nrow(rv$cnv_data) > 0) {
          CNV_COLORS <- c(
            'Gain' = '#B3D9FF',           # Light sky blue (pastel - distinct from mutations)
            'Loss' = '#FFB3D9',           # Light pink (pastel - distinct from mutations)
            'LOH' = '#FFD9B3',            # Light peach (pastel - distinct from mutations)
            #'Not specified' = '#E6D4C4',  # Light tan (pastel - distinct from mutations)
            'Chromothripsis' = '#D4B3E6'  # Light purple (pastel - distinct from mutations)
          )
          
          rv$CNV_COLORS <- CNV_COLORS
          
          # Process CNV data - filter by patient IDs instead of diagnosis
          ge <- rv$cnv_data
          
          # Ensure required columns exist
          if (!("Pt_ID" %in% names(ge)) || !("Gene" %in% names(ge)) || !("Event" %in% names(ge))) {
            showNotification(
              "CNV data missing required columns (Pt_ID, Gene, Event). Skipping CNV visualization.",
              type = "warning", duration = 5
            )
            ge <- data.frame()  # Empty dataframe
          } else {
            ge <- ge %>%
              filter(!is.na(Pt_ID), Pt_ID %in% pts_filtered$Pt_ID) %>%
              mutate(Event = trimws(Event)) %>%
              filter(Event %in% names(CNV_COLORS))
          }
          
          if (nrow(ge) > 0) {
            ge_somatic <- ge %>% filter(is.na(CellType) | CellType == "Somatic")
            ge_germ <- ge %>% filter(!is.na(CellType) & CellType == "Germline")
            
            cnv_som_tbl <- ge_somatic %>%
              group_by(Pt_ID, Gene) %>%
              summarise(EventList = list(sort(unique(Event))), .groups = "drop")
            
            cnv_germ_tbl <- ge_germ %>%
              group_by(Pt_ID, Gene) %>%
              summarise(EventList = list(sort(unique(Event))), .groups = "drop")
            
            make_event_map <- function(tbl) {
              if (is.null(tbl) || nrow(tbl) == 0) return(list())
              keys <- paste0(tbl$Pt_ID, "|", tbl$Gene)
              keep <- !is.na(keys)
              setNames(tbl$EventList[keep], keys[keep])
            }
            
            rv$cnv_somatic_map <- make_event_map(cnv_som_tbl)
            rv$cnv_germline_map <- make_event_map(cnv_germ_tbl)
          } else {
            # No CNV data available or valid
            rv$cnv_somatic_map <- list()
            rv$cnv_germline_map <- list()
          }
        } else {
          # No CNV data uploaded
          rv$cnv_somatic_map <- list()
          rv$cnv_germline_map <- list()
          rv$CNV_COLORS <- NULL
        }
        
        # Calculate appropriate cell sizes based on sizing strategy
        cell_sizes <- calculate_cell_sizes(
          sizing_strategy = input$sizing_strategy,
          cell_width = input$cell_width,
          cell_height = input$cell_height,
          target_pdf_width = input$target_pdf_width,
          target_pdf_height = input$target_pdf_height,
          n_patients = ncol(rv$mutation_matrix),
          n_genes = nrow(rv$mutation_matrix)
        )
        
        incProgress(0.7, detail = "Creating heatmap...")
        
        ## Apply custom gene order if enabled
        if (!is.null(input$enable_gene_ordering) && input$enable_gene_ordering && !is.null(rv$gene_order)) {
          # Get current gene order from matrix
          current_genes <- rownames(rv$mutation_matrix)
          
          # Filter custom order to only include genes that are in the matrix
          valid_gene_order <- rv$gene_order[rv$gene_order %in% current_genes]
          
          # Add any genes from matrix that aren't in custom order (shouldn't happen, but safety check)
          missing_genes <- setdiff(current_genes, valid_gene_order)
          if (length(missing_genes) > 0) {
            valid_gene_order <- c(valid_gene_order, missing_genes)
          }
          
          # Reorder mutation matrix
          rv$mutation_matrix <- rv$mutation_matrix[valid_gene_order, , drop = FALSE]
          
          # Reorder somatic and germline maps (check they exist and are matrices)
          if (!is.null(rv$somatic_map) && is.matrix(rv$somatic_map) && nrow(rv$somatic_map) == length(current_genes)) {
            rv$somatic_map <- rv$somatic_map[valid_gene_order, , drop = FALSE]
          }
          if (!is.null(rv$germline_map) && is.matrix(rv$germline_map) && nrow(rv$germline_map) == length(current_genes)) {
            rv$germline_map <- rv$germline_map[valid_gene_order, , drop = FALSE]
          }
          
          # Reorder CNV maps if they exist and are matrices
          if (!is.null(rv$cnv_somatic_map) && is.matrix(rv$cnv_somatic_map) && nrow(rv$cnv_somatic_map) == length(current_genes)) {
            rv$cnv_somatic_map <- rv$cnv_somatic_map[valid_gene_order, , drop = FALSE]
          }
          if (!is.null(rv$cnv_germline_map) && is.matrix(rv$cnv_germline_map) && nrow(rv$cnv_germline_map) == length(current_genes)) {
            rv$cnv_germline_map <- rv$cnv_germline_map[valid_gene_order, , drop = FALSE]
          }
          
          message("Applied custom gene order: ", paste(head(valid_gene_order, 5), collapse = ", "), "...")
        }
        
        ## Create heatmap (using simplified version for app)
        rv$heatmap_object <- create_heatmap_object(
          mutation_matrix = rv$mutation_matrix,
          somatic_map = rv$somatic_map,
          germline_map = rv$germline_map,
          cnv_somatic_map = rv$cnv_somatic_map,
          cnv_germline_map = rv$cnv_germline_map,
          type_colors = rv$type_colors,
          CNV_COLORS = rv$CNV_COLORS,
          cell_width = cell_sizes$cell_width,      # Uses calculated sizes
          cell_height = cell_sizes$cell_height,    # Based on strategy
          use_gene_families = input$use_gene_families,
          gene_family_frequency = mut_obj$gene_family_frequency,
          data = data_filtered,
          # NEW: Pass clustering parameters
          clustering_strategy = input$clustering_strategy,
          show_dendrogram = input$show_dendrogram,
          dendrogram_height = input$dendrogram_height,
          cluster_method = input$cluster_method,
          num_clusters = input$num_clusters,
          cluster_data = rv$cluster_data,
          # NEW: Pass clinical annotation parameters
          clinical_data = rv$clinical_data,
          clinical_annotation_cols = input$clinical_annotation_cols,
          # NEW: Pass show mutation counts parameter
          show_mutation_counts = input$show_mutation_counts
        )
        
        # Store values needed for PDF recreation
        rv$gene_family_frequency <- mut_obj$gene_family_frequency
        rv$data_filtered <- data_filtered
        
        incProgress(1, detail = "Done!")
        
        rv$plot_ready <- TRUE
        
        # Switch to visualization tab
        updateTabsetPanel(session, "main_tabs", selected = "🎨 Oncoprint")
        
        showNotification("✓ Oncoprint generated successfully!", 
                        type = "message", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), 
                        type = "error", duration = 10)
        print(e)
      })
    })
  })
  
  ## ========================================
  ## Plotting Outputs
  ## ========================================
  
  output$status_message <- renderUI({
    if (!rv$plot_ready) {
      div(
        class = "alert alert-info",
        h4("Ready to Generate"),
        p("Configure your parameters and click 'Generate Oncoprint' to create the visualization.")
      )
    } else {
      div(
        class = "alert alert-success",
        h4("✓ Oncoprint Generated"),
        p("Visualization ready. Use the download button to save as PDF.")
      )
    }
  })
  
  output$plot_oncoprint <- renderPlot({
    req(rv$plot_ready)
    req(rv$heatmap_object)
    
    # rv$heatmap_object is now a list with heatmap and legends
    if (is.list(rv$heatmap_object) && "heatmap" %in% names(rv$heatmap_object)) {
      draw(
        rv$heatmap_object$heatmap,
        annotation_legend_list = list(rv$heatmap_object$legends),
        annotation_legend_side = "bottom",
        padding = unit(c(2, 2, 2, 2), "mm")
      )
    } else {
      # Fallback for old format 
      draw(rv$heatmap_object)
    }
  })
  
  output$plot_dimensions <- renderPrint({
    req(rv$plot_ready)
    req(rv$mutation_matrix)
    
    cat("Oncoprint Dimensions:\n")
    cat("  Genes:", nrow(rv$mutation_matrix), "\n")
    cat("  Samples:", ncol(rv$mutation_matrix), "\n")
    cat("  Cell size:", input$cell_width, "mm x", input$cell_height, "mm\n")
    cat("  Total mutations:", sum(!is.na(rv$mutation_matrix)), "\n")
  })
  
  ## ========================================
  ## Statistics Plots
  ## ========================================
  
  output$plot_gene_freq <- renderPlot({
    if (is.null(rv$mutation_matrix) || !rv$plot_ready) {
      # Show placeholder
      plot.new()
      text(0.5, 0.5, "Generate an oncoprint first to see gene frequency statistics", 
           cex = 1.2, col = "gray50")
      return()
    }
    
    tryCatch({
      gene_freq <- sort(rowSums(!is.na(rv$mutation_matrix)), decreasing = TRUE)
      
      if (length(gene_freq) == 0) {
        plot.new()
        text(0.5, 0.5, "No gene data available", cex = 1.2, col = "gray50")
        return()
      }
      
      par(mar = c(4, 8, 3, 1))
      barplot(
        head(gene_freq, 20),
        horiz = TRUE,
        las = 1,
        col = "#3288BD",
        main = "Top 20 Most Frequently Altered Genes (Mutations & Fusions)",
        xlab = "Number of Samples",
        cex.names = 0.8
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
    })
  })
  
  output$plot_mutation_types <- renderPlot({
    if (is.null(rv$mutation_matrix) || !rv$plot_ready) {
      plot.new()
      text(0.5, 0.5, "Generate an oncoprint first to see alteration type distribution", 
           cex = 1.2, col = "gray50")
      return()
    }
    
    tryCatch({
      type_counts <- table(rv$mutation_matrix[!is.na(rv$mutation_matrix)])
      
      if (length(type_counts) == 0) {
        plot.new()
        text(0.5, 0.5, "No alteration data available", cex = 1.2, col = "gray50")
        return()
      }
      
      par(mar = c(4, 4, 3, 1))
      barplot(
        sort(type_counts, decreasing = TRUE),
        col = rv$type_colors[names(sort(type_counts, decreasing = TRUE))],
        main = "Distribution of Alteration Types (Mutations & Fusions)",
        ylab = "Count",
        las = 2,
        cex.names = 0.8
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
    })
  })
  
  output$plot_sample_burden <- renderPlot({
    if (is.null(rv$mutation_matrix) || !rv$plot_ready) {
      plot.new()
      text(0.5, 0.5, "Generate an oncoprint first to see mutation burden distribution", 
           cex = 1.2, col = "gray50")
      return()
    }
    
    tryCatch({
      sample_burden <- colSums(!is.na(rv$mutation_matrix))
      
      if (length(sample_burden) == 0) {
        plot.new()
        text(0.5, 0.5, "No sample data available", cex = 1.2, col = "gray50")
        return()
      }
      
      # Create integer breaks for discrete mutation counts
      min_mut <- floor(min(sample_burden))
      max_mut <- ceiling(max(sample_burden))
      breaks_seq <- seq(min_mut - 0.5, max_mut + 0.5, by = 1)
      
      par(mar = c(4, 4, 3, 1))
      hist(
        sample_burden,
        breaks = breaks_seq,
        col = "#E31A1C",
        main = "Mutation Burden Distribution",
        xlab = "Number of Mutations per Sample",
        ylab = "Frequency",
        xaxt = "n"  # Suppress default x-axis
      )
      
      # Add custom integer x-axis
      axis(1, at = min_mut:max_mut, labels = min_mut:max_mut)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
    })
  })
  
  output$plot_diagnosis_dist <- renderPlot({
    if (is.null(rv$filtered_patient_dx) || !rv$plot_ready) {
      plot.new()
      text(0.5, 0.5, "Generate an oncoprint first to see diagnosis distribution", 
           cex = 1.2, col = "gray50")
      return()
    }
    
    tryCatch({
      diag_counts <- table(rv$filtered_patient_dx$Diagnosis)
      
      if (length(diag_counts) == 0) {
        plot.new()
        text(0.5, 0.5, "No diagnosis data available", cex = 1.2, col = "gray50")
        return()
      }
      
      par(mar = c(4, 4, 3, 1))
      pie(
        diag_counts,
        labels = paste0(names(diag_counts), "\n(n=", diag_counts, ")"),
        col = rainbow(length(diag_counts)),
        main = "Sample Distribution by Diagnosis"
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")
    })
  })
  
  output$summary_table <- DT::renderDataTable({
    if (is.null(rv$mutation_matrix) || !rv$plot_ready) {
      return(DT::datatable(
        data.frame(
          Message = "Please generate an oncoprint first to see summary statistics"
        ),
        options = list(dom = 't')
      ))
    }
    
    tryCatch({
      summary_df <- data.frame(
        Metric = c(
          "Total Samples",
          "Total Genes",
          "Total Mutations",
          "Avg Mutations per Sample",
          "Avg Mutations per Gene",
          "Most Mutated Gene",
          "Sample with Most Mutations"
        ),
        Value = c(
          ncol(rv$mutation_matrix),
          nrow(rv$mutation_matrix),
          sum(!is.na(rv$mutation_matrix)),
          round(mean(colSums(!is.na(rv$mutation_matrix))), 2),
          round(mean(rowSums(!is.na(rv$mutation_matrix))), 2),
          rownames(rv$mutation_matrix)[which.max(rowSums(!is.na(rv$mutation_matrix)))],
          colnames(rv$mutation_matrix)[which.max(colSums(!is.na(rv$mutation_matrix)))]
        )
      )
      
      DT::datatable(summary_df, options = list(dom = 't'))
    }, error = function(e) {
      DT::datatable(
        data.frame(Error = paste("Error generating summary:", e$message)),
        options = list(dom = 't')
      )
    })
  })
  
  ## ========================================
  ## Download Handlers
  ## ========================================
  
  output$btn_download_pdf <- downloadHandler(
    filename = function() {
      diagnosis_str <- gsub("[^A-Za-z0-9]+", "_", paste(input$target_diagnosis, collapse = "_"))
      paste0("oncoprint_", diagnosis_str, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Check requirements
      if (is.null(rv$mutation_matrix)) {
        stop("No mutation matrix available. Please generate the oncoprint first.")
      }
      if (is.null(rv$somatic_map)) {
        stop("No somatic map available. Please generate the oncoprint first.")
      }
      if (is.null(rv$germline_map)) {
        stop("No germline map available. Please generate the oncoprint first.")
      }
      if (is.null(rv$type_colors)) {
        stop("No type colors available. Please generate the oncoprint first.")
      }
      
      message("Starting PDF generation...")
      message("Matrix dimensions: ", nrow(rv$mutation_matrix), " genes x ", ncol(rv$mutation_matrix), " patients")
      
      tryCatch({
        # Calculate cell sizes for PDF
        message("Calculating cell sizes...")
        cell_sizes <- calculate_cell_sizes(
          sizing_strategy = input$sizing_strategy,
          cell_width = input$cell_width,
          cell_height = input$cell_height,
          target_pdf_width = input$target_pdf_width,
          target_pdf_height = input$target_pdf_height,
          n_patients = ncol(rv$mutation_matrix),
          n_genes = nrow(rv$mutation_matrix)
        )
        message("Cell sizes: ", cell_sizes$cell_width, " x ", cell_sizes$cell_height, " mm")
        
        # Calculate PDF dimensions based on strategy
        if (input$sizing_strategy == "FIXED_CELLS") {
          # Check if manual override is enabled
          if (input$override_pdf_size) {
            # Use manually specified PDF dimensions
            pdf_width <- input$override_pdf_width
            pdf_height <- input$override_pdf_height
            message("Using manual PDF size override: ", pdf_width, " x ", pdf_height, " inches")
          } else {
            # PDF size adapts to cell sizes - add extra space for legends and annotations
            pdf_width <- ncol(rv$mutation_matrix) * cell_sizes$cell_width / 25.4 + 3
            pdf_height <- nrow(rv$mutation_matrix) * cell_sizes$cell_height / 25.4 + 7
            message("Auto-calculated PDF size: ", pdf_width, " x ", pdf_height, " inches")
          }
        } else {
          # For FIXED_PDF and BALANCED, use target dimensions
          pdf_width <- input$target_pdf_width
          pdf_height <- input$target_pdf_height
        }
        message("PDF dimensions: ", pdf_width, " x ", pdf_height, " inches")
        
        # Handle NULL values with defaults
        gene_family_freq <- if (!is.null(rv$gene_family_frequency)) rv$gene_family_frequency else NULL
        data_filt <- if (!is.null(rv$data_filtered)) rv$data_filtered else NULL
        
        # RECREATE heatmap object with PDF-specific cell sizes
        message("Creating heatmap object for PDF...")
        pdf_heatmap <- create_heatmap_object(
          mutation_matrix = rv$mutation_matrix,
          somatic_map = rv$somatic_map,
          germline_map = rv$germline_map,
          cnv_somatic_map = rv$cnv_somatic_map,
          cnv_germline_map = rv$cnv_germline_map,
          type_colors = rv$type_colors,
          CNV_COLORS = rv$CNV_COLORS,
          cell_width = cell_sizes$cell_width,      # Use PDF cell sizes
          cell_height = cell_sizes$cell_height,    # Use PDF cell sizes
          use_gene_families = input$use_gene_families,
          gene_family_frequency = gene_family_freq,
          data = data_filt,
          clustering_strategy = input$clustering_strategy,
          show_dendrogram = input$show_dendrogram,
          dendrogram_height = input$dendrogram_height,
          cluster_method = input$cluster_method,
          num_clusters = if (!is.na(input$num_clusters)) input$num_clusters else NULL,
          cluster_data = rv$cluster_data,
          clinical_data = rv$clinical_data,
          clinical_annotation_cols = input$clinical_annotation_cols,
          show_mutation_counts = input$show_mutation_counts
        )
        message("Heatmap object created successfully")
        
        # Open PDF device with calculated dimensions
        message("Opening PDF device...")
        pdf(file, width = pdf_width, height = pdf_height)
        
        # Draw newly created heatmap with legends
        message("Drawing heatmap to PDF...")
        if (is.list(pdf_heatmap) && "heatmap" %in% names(pdf_heatmap)) {
          draw(
            pdf_heatmap$heatmap,
            annotation_legend_list = list(pdf_heatmap$legends),
            annotation_legend_side = "bottom",
            padding = unit(c(2, 2, 2, 2), "mm")
          )
        } else {
          draw(pdf_heatmap)
        }
        
        message("Closing PDF device...")
        dev.off()
        message("PDF generation completed successfully!")
        
      }, error = function(e) {
        # Close any open graphics devices
        if (length(dev.list()) > 0) {
          message("Closing graphics device due to error...")
          dev.off()
        }
        # Log the full error
        message("=== PDF GENERATION ERROR ===")
        message("Error message: ", e$message)
        message("Error call: ", deparse(e$call))
        message("===========================")
        
        # Re-throw with more info
        stop(paste0(
          "PDF generation failed: ", e$message, 
          "\n\nPlease check the R console for details.",
          "\nIf the error persists, try:",
          "\n1. Regenerate the oncoprint",
          "\n2. Try a different sizing strategy",
          "\n3. Check that all data files are loaded correctly"
        ))
      })
    }
  )
  
  #   output$btn_download_data <- downloadHandler(
  #   filename = function() {
  #     paste0("mutation_matrix_", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     req(rv$mutation_matrix)
  #     write.csv(rv$mutation_matrix, file, row.names = TRUE)
  #   }
  # )
}

## ========================================
## Helper Functions
## ========================================

# Build mutation objects (from  original script)
build_mutation_objects <- function(data, pts, 
                                  use_gene_families = TRUE,
                                  group_singletons = TRUE,
                                  min_family_size = 2) {
  
  # Build somatic and germline tables
  somatic_tbl <- data %>%
    filter(CellType == "Somatic") %>%
    group_by(Pt_ID, Gene) %>%
    summarise(TypeList = list(Type), .groups = "drop")
  
  germline_tbl <- data %>%
    filter(CellType == "Germline") %>%
    group_by(Pt_ID, Gene) %>%
    summarise(TypeList = list(Type), .groups = "drop")
  
  # Create mutation matrix
  unique_genes <- unique(data$Gene)
  unique_patients <- unique(pts$Pt_ID)
  mutation_matrix <- matrix(
    NA, nrow = length(unique_genes), ncol = length(unique_patients),
    dimnames = list(unique_genes, unique_patients)
  )
  
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    mutation_matrix[row$Gene, row$Pt_ID] <- row$Type
  }
  
  # Create lookup maps
  if (nrow(somatic_tbl) > 0) {
    somatic_map <- setNames(
      somatic_tbl$TypeList,
      paste0(somatic_tbl$Pt_ID, "|", somatic_tbl$Gene)
    )
  } else {
    somatic_map <- list()
  }
  
  if (nrow(germline_tbl) > 0) {
    germline_map <- setNames(
      germline_tbl$TypeList,
      paste0(germline_tbl$Pt_ID, "|", germline_tbl$Gene)
    )
  } else {
    germline_map <- list()
  }
  
  # Color scheme
  # MUTTYPE_COLOURS <- c(
  #   "Missense" = "#E31A1C",
  #   "Truncation" = "#1F78B4",
  #   "Inframe" = "#33A02C",
  #   "Fusion" = "#6A3D9A",
  #   "Synonymous" = "#FF7F00",
  #   "Loss/Mutation" = "#CAB2D6",
  #   "Not Specified" = '#A65628',
  #   "NA" = "#FFFFFF"
  # )
  
  MUTTYPE_COLOURS <- c(
    "Missense" = "#E31A1C",        # Red
    "Truncation" = "#1F78B4",      # Blue
    "Inframe" = "#33A02C",         # Green
    "Fusion" = "#6A3D9A",          # Purple
    "Synonymous" = "#FF7F00",      # Orange
    "Splice Site" = "#E6D4C4",     # Medium-dark red --NEW
    "Promoter" = "#006D6F",        # Medium-dark orange --NEW
    "Start Site" = "#7A1F3D",      # Medium-dark green --NEW    
    "Loss/Mutation" = "#CAB2D6",
    "Not Specified" = '#A65628',
    "NA" = "#FFFFFF"
  )
  
  all_types <- intersect(names(MUTTYPE_COLOURS), unique(data$Type))
  type_colors <- MUTTYPE_COLOURS[all_types]
  
  # Diagnosis data
  pts$Diagnosis <- as.factor(pts$Diagnosis)
  filtered_patient_dx <- pts %>%
    select(Pt_ID, Diagnosis) %>%
    distinct(Pt_ID, Diagnosis) %>%
    arrange(Diagnosis) %>%
    filter(Pt_ID %in% colnames(mutation_matrix))
  
  # Gene family processing
  if (use_gene_families && "Gene_Family" %in% names(data)) {
    if (group_singletons) {
      gene_families_with_few_members <- data %>% 
        group_by(Gene_Family) %>%
        filter(n_distinct(Gene) < min_family_size) %>% 
        pull(Gene_Family) %>% 
        unique()
      
      other_genes <- data %>% 
        filter(Gene_Family %in% gene_families_with_few_members) %>%
        pull(Gene) %>% 
        unique()
      
      data$Gene_Family[data$Gene %in% other_genes] <- "Other"
    }
    
    gene_frequency <- rowSums(!is.na(mutation_matrix))
    mutation_matrix <- mutation_matrix[order(gene_frequency, decreasing = TRUE), ]
    gene_families <- data$Gene_Family[match(rownames(mutation_matrix), data$Gene)]
    gene_family_frequency <- table(gene_families) |> 
      sort(decreasing = TRUE) |> 
      names()
    
    if ("Other" %in% gene_family_frequency) {
      gene_family_frequency <- c(
        gene_family_frequency[gene_family_frequency != "Other"], 
        "Other"
      )
    }
  } else {
    gene_frequency <- rowSums(!is.na(mutation_matrix))
    mutation_matrix <- mutation_matrix[order(gene_frequency, decreasing = TRUE), ]
    gene_family_frequency <- NULL
  }
  
  list(
    data = data,
    somatic_tbl = somatic_tbl,
    germline_tbl = germline_tbl,
    mutation_matrix = mutation_matrix,
    somatic_map = somatic_map,
    germline_map = germline_map,
    type_colors = type_colors,
    filtered_patient_dx = filtered_patient_dx,
    gene_family_frequency = gene_family_frequency,
    use_gene_families = use_gene_families
  )
}

# Create heatmap object
# Calculate cell sizes based on sizing strategy
calculate_cell_sizes <- function(sizing_strategy, cell_width, cell_height,
                                 target_pdf_width, target_pdf_height,
                                 n_patients, n_genes) {
  
  # Margins and legend space 
  margin_width <- 6  # inches for row names, legends, etc.
  margin_height <- 4 # inches for column names, legends, etc.
  
  if (sizing_strategy == "FIXED_CELLS") {
    # Use user-specified cell sizes
    return(list(
      cell_width = cell_width,
      cell_height = cell_height
    ))
    
  } else if (sizing_strategy == "FIXED_PDF") {
    # Calculate cell sizes to fit target PDF dimensions
    available_width <- (target_pdf_width - margin_width) * 25.4  # inches to mm
    available_height <- (target_pdf_height - margin_height) * 25.4
    
    calc_cell_width <- max(2, available_width / n_patients)
    calc_cell_height <- max(3, available_height / n_genes)
    
    return(list(
      cell_width = calc_cell_width,
      cell_height = calc_cell_height
    ))
    
  } else if (sizing_strategy == "BALANCED") {
    # Balance between PDF size and cell legibility
    available_width <- (target_pdf_width - margin_width) * 25.4
    available_height <- (target_pdf_height - margin_height) * 25.4
    
    # Calculate what would fit
    calc_cell_width <- available_width / n_patients
    calc_cell_height <- available_height / n_genes
    
    # But enforce minimum readable sizes
    min_cell_width <- 2  # mm
    min_cell_height <- 3 # mm
    max_cell_width <- 10 # mm
    max_cell_height <- 12 # mm
    
    balanced_width <- max(min_cell_width, min(max_cell_width, calc_cell_width))
    balanced_height <- max(min_cell_height, min(max_cell_height, calc_cell_height))
    
    return(list(
      cell_width = balanced_width,
      cell_height = balanced_height
    ))
  }
  
  # Default fallback
  return(list(cell_width = cell_width, cell_height = cell_height))
}

# ============================================================================
# CREATE CLINICAL ANNOTATION HEATMAP
# ============================================================================

create_clinical_annotation <- function(clinical_data, patient_order, selected_cols) {
  
  if (is.null(clinical_data) || is.null(selected_cols) || length(selected_cols) == 0) {
    return(NULL)
  }
  
  # Prepare annotation parameters and color list
  anno_params <- list()
  col_list <- list()
  
  for (col in selected_cols) {
    if (!(col %in% names(clinical_data))) {
      next
    }
    
    # Create a named vector matching patient_order
    # This ensures we have a value for each patient, even if NA
    clinical_vec <- setNames(clinical_data[[col]], clinical_data$Pt_ID)
    values <- clinical_vec[patient_order]
    names(values) <- NULL
    
    # Determine if numeric or categorical
    if (is.numeric(values)) {
      # Numeric annotation (e.g., Age) --white to green
      anno_params[[col]] <- values
      col_list[[col]] <- colorRamp2(
        c(min(values, na.rm = TRUE), max(values, na.rm = TRUE)),
        c("#ffffff", "#27ae60")  # White → Green
      )
      
    } else {
      # Categorical annotation (e.g., Sex, Diagnosis)
      values <- as.character(values)
      
      # Replace NA with "NA" string for visualization
      values[is.na(values)] <- "NA"
      
      # Get unique categories including "NA"
      n_categories <- length(unique(values))
      
      # Assign colors based on number of categories
      if (n_categories <= 2) {
        categories <- sort(unique(values), decreasing = TRUE)  
        colors <- c("#3498db", "#e74c3c")  # Blue, Red
      } else if (n_categories <= 8) {
        categories <- sort(unique(values))
        colors <- brewer.pal(max(3, min(n_categories, 8)), "Set2")[1:n_categories]
      } else {
        categories <- sort(unique(values))
        colors <- rainbow(n_categories)
      }
      
      # If "NA" is present, make it gray
      if ("NA" %in% categories) {
        na_idx <- which(categories == "NA")
        colors[na_idx] <- "#CCCCCC"  # Gray for NA
      }
      
      # Create named color vector with exact same categories
      col_mapping <- setNames(colors[1:n_categories], categories)
      
      # Create factor with all categories (including NA)
      anno_params[[col]] <- factor(values, levels = categories)
      
      # Add color mapping to col_list (ComplexHeatmap format)
      col_list[[col]] <- col_mapping
    }
  }
  
  # Create HeatmapAnnotation 
  if (length(anno_params) == 0) {
    return(NULL)
  }
  
  # Build annotation using do.call
  # Pass col_list as 'col' parameter if it has any mappings
  params_to_pass <- c(
    anno_params,
    list(
      which = "column",
      show_legend = TRUE,
      annotation_name_side = "left",
      annotation_name_gp = gpar(fontsize = 9, fontface = "bold"),
      simple_anno_size = unit(4, "mm"),
      gap = unit(1, "mm"),
      border = TRUE,
      gp = gpar(col = "black", lwd = 0.5) # cell level column borders
    )
  )
  
  # Add color list categorical variables
  if (length(col_list) > 0) {
    params_to_pass$col <- col_list
  }
  
  ha <- do.call(HeatmapAnnotation, params_to_pass)
  
  return(ha)
}



create_heatmap_object <- function(mutation_matrix, somatic_map, germline_map,
                                  cnv_somatic_map = NULL, cnv_germline_map = NULL,
                                  type_colors, CNV_COLORS = NULL,
                                  cell_width = 5, cell_height = 9,
                                  use_gene_families = FALSE,
                                  gene_family_frequency = NULL,
                                  data = NULL,
                                  # NEW: Clustering parameters
                                  clustering_strategy = "NONE",
                                  show_dendrogram = FALSE,
                                  dendrogram_height = 20,
                                  cluster_method = "ward.D2",
                                  num_clusters = NULL,
                                  cluster_data = NULL,
                                  # NEW: Clinical annotation parameters
                                  clinical_data = NULL,
                                  clinical_annotation_cols = NULL,
                                  # NEW: Show mutation counts parameter
                                  show_mutation_counts = TRUE) {
  
  BG_COL <- "#C0C0C0"
  
  # Initialize clustering variables
  use_clustering <- clustering_strategy != "NONE"
  col_dend <- NULL
  column_split <- NULL
  cluster_columns <- FALSE
  column_dend_height <- unit(0, "mm")
  show_column_dend <- FALSE
  column_order <- NULL
  
  # Perform clustering if requested
  if (use_clustering && clustering_strategy == "FILE") {
    # Use cluster assignments from uploaded file
    if (!is.null(cluster_data)) {
      # Match cluster assignments to samples in mutation matrix
      sample_names <- colnames(mutation_matrix)
      
      # Try to match by Sample_Name or Pt_ID
      cluster_col <- if ("Sample_Name" %in% names(cluster_data)) {
        "Sample_Name"
      } else if ("Pt_ID" %in% names(cluster_data)) {
        "Pt_ID"
      } else {
        names(cluster_data)[1]  # Use first column as fallback
      }
      
      # Create cluster assignments vector for ALL samples
      cluster_assignments <- cluster_data$Cluster[match(sample_names, cluster_data[[cluster_col]])]
      
      # For samples without cluster assignments, assign "Unassigned"
      cluster_assignments[is.na(cluster_assignments)] <- "Unassigned"
      
      # Create factor with all samples
      cluster_factor <- factor(cluster_assignments)
      
      # Set clustering parameters
      column_split <- cluster_factor
      show_column_dend <- FALSE
    }
  } else if (use_clustering && clustering_strategy == "GENE_ORDER") {
    # Order samples from left to right based on the first altered gene
    # in the CURRENT displayed gene order (top-to-bottom on the oncoprint).
    gene_priority <- rownames(mutation_matrix)
    sample_names <- colnames(mutation_matrix)
    
    first_hit_rank <- sapply(sample_names, function(sample_name) {
      altered_genes <- which(!is.na(mutation_matrix[, sample_name]))
      if (length(altered_genes) == 0) {
        Inf
      } else {
        min(altered_genes)
      }
    })
    
    alteration_burden <- sapply(sample_names, function(sample_name) {
      sum(!is.na(mutation_matrix[, sample_name]))
    })
    
    # Samples are ordered by the first gene they hit in the selected gene order.
    # Ties are broken by higher alteration burden first, then by sample name.
    ordered_samples <- sample_names[order(first_hit_rank, -alteration_burden, sample_names)]
    column_order <- ordered_samples
    show_column_dend <- FALSE
    message("Applied gene-order-based patient ordering using current displayed gene order")
  } else if (use_clustering && clustering_strategy %in% c("MUTATION", "COMBINED")) {
    
    # Create binary matrix for clustering
    if (clustering_strategy == "MUTATION") {
      binary_mat <- matrix(
        as.integer(!is.na(mutation_matrix)), 
        nrow = nrow(mutation_matrix), 
        ncol = ncol(mutation_matrix),
        dimnames = dimnames(mutation_matrix)
      )
    } else if (clustering_strategy == "COMBINED") {
      # Mutation binary
      mutation_binary <- matrix(
        as.integer(!is.na(mutation_matrix)), 
        nrow = nrow(mutation_matrix), 
        ncol = ncol(mutation_matrix),
        dimnames = dimnames(mutation_matrix)
      )
      
      # CNV binary
      if (!is.null(cnv_somatic_map) && length(cnv_somatic_map) > 0) {
        cnv_binary <- matrix(0, nrow = nrow(mutation_matrix), ncol = ncol(mutation_matrix),
                            dimnames = dimnames(mutation_matrix))
        
        for (g in rownames(mutation_matrix)) {
          for (p in colnames(mutation_matrix)) {
            key <- paste0(p, "|", g)
            has_som_cnv <- length(unlist(cnv_somatic_map[[key]])) > 0
            has_germ_cnv <- if (!is.null(cnv_germline_map)) {
              length(unlist(cnv_germline_map[[key]])) > 0
            } else {
              FALSE
            }
            cnv_binary[g, p] <- as.integer(has_som_cnv || has_germ_cnv)
          }
        }
        
        # Stack mutation and CNV matrices
        binary_mat <- rbind(mutation_binary, cnv_binary)
      } else {
        binary_mat <- mutation_binary
      }
    }
    
    # Perform hierarchical clustering
    col_dist <- dist(t(binary_mat), method = "binary")
    col_hcl  <- hclust(col_dist, method = cluster_method)
    col_dend <- as.dendrogram(col_hcl)
    
    # Determine number of clusters
    if (is.null(num_clusters) || is.na(num_clusters)) {
      num_clusters <- min(5, max(2, ceiling(ncol(binary_mat) / 10)))
    }
    
    # Cut tree to create clusters
    cluster_assignments <- cutree(col_hcl, k = num_clusters)
    
    # Set clustering parameters
    if (show_dendrogram) {
      cluster_columns <- col_dend
      column_split <- num_clusters
      column_dend_height <- unit(dendrogram_height, "mm")
      show_column_dend <- TRUE
    } else {
      cluster_columns <- FALSE
      cluster_factor <- factor(paste0("C", cluster_assignments), 
                              levels = paste0("C", sort(unique(cluster_assignments))))
      column_split <- cluster_factor
      show_column_dend <- FALSE
    }
  }
  
  # ========================================================================
  # REORDER CLUSTERS: Move empty clusters to the right
  # ========================================================================
  # If we have a column_split factor , reorder it so that
  # clusters with no mutations appear on the furthest right
  
  if (!is.null(column_split) && is.factor(column_split)) {
    
    # Calculate total mutations per cluster
    cluster_levels <- levels(column_split)
    mutation_counts <- sapply(cluster_levels, function(cluster_name) {
      # Get columns belonging to this cluster
      cluster_cols <- which(column_split == cluster_name)
      
      # Count total non-NA cells in these columns
      sum(!is.na(mutation_matrix[, cluster_cols, drop = FALSE]))
    })
    
    # Create ordering: clusters with mutations first (by mutation count descending),
    # then clusters with no mutations
    clusters_with_mutations <- cluster_levels[mutation_counts > 0]
    empty_clusters <- cluster_levels[mutation_counts == 0]
    
    # Sort non-empty clusters by mutation count (descending)
    if (length(clusters_with_mutations) > 0) {
      sorted_nonempty <- clusters_with_mutations[order(mutation_counts[mutation_counts > 0], decreasing = TRUE)]
    } else {
      sorted_nonempty <- character(0)
    }
    
    # New order: non-empty clusters first, empty clusters last
    new_order <- c(sorted_nonempty, empty_clusters)
    
    # Reorder the factor levels
    column_split <- factor(column_split, levels = new_order)
    
    # Optional: Print info for debugging
    if (length(empty_clusters) > 0) {
      message(sprintf("Reordered clusters: %d non-empty clusters, %d empty clusters moved to right", 
                      length(sorted_nonempty), length(empty_clusters)))
    }
  }
  # ========================================================================
  
  # Calculate frequencies - include both mutations and CNVs
  mutation_frequencies <- t(
    apply(
      mutation_matrix,
      1,
      function(x) table(factor(x, levels = names(type_colors)))
    )
  )
  
  # Add CNV frequencies if CNV data exists
  if (!is.null(CNV_COLORS) && !is.null(cnv_somatic_map)) {
    # Create CNV frequency matrix
    cnv_freq <- matrix(0, nrow = nrow(mutation_matrix), ncol = length(CNV_COLORS),
                      dimnames = list(rownames(mutation_matrix), names(CNV_COLORS)))
    
    for (g in rownames(mutation_matrix)) {
      for (p in colnames(mutation_matrix)) {
        key <- paste0(p, "|", g)
        som_cnv <- unlist(cnv_somatic_map[[key]])
        germ_cnv <- if (!is.null(cnv_germline_map)) unlist(cnv_germline_map[[key]]) else character(0)
        all_cnv <- c(som_cnv, germ_cnv)
        
        for (cnv_type in names(CNV_COLORS)) {
          cnv_freq[g, cnv_type] <- cnv_freq[g, cnv_type] + sum(all_cnv == cnv_type)
        }
      }
    }
    
    # Combine mutation and CNV frequencies
    combined_frequencies <- cbind(mutation_frequencies, cnv_freq)
    combined_colors <- c(type_colors, CNV_COLORS)
  } else {
    combined_frequencies <- mutation_frequencies
    combined_colors <- type_colors
  }
  
  
  # ============================================================================
  # Create clinical annotation if requested
  # ============================================================================
  
  clinical_annot <- NULL
  if (!is.null(clinical_data) && !is.null(clinical_annotation_cols) && length(clinical_annotation_cols) > 0) {
    patient_order <- if (!is.null(column_order)) column_order else colnames(mutation_matrix)
    clinical_annot <- create_clinical_annotation(clinical_data, patient_order, clinical_annotation_cols)
  }
  ht <- Heatmap(
    mutation_matrix,
    name = "Alteration Type",
    col = type_colors,
    na_col = BG_COL,
    rect_gp = gpar(col = NA, fill = NA),
    show_column_names = TRUE,
    column_names_gp = gpar(fontsize = 8, fontface = "bold"),
    column_names_rot = 90,
    column_names_centered = TRUE,
    
    width = ncol(mutation_matrix) * unit(cell_width, "mm"),
    height = nrow(mutation_matrix) * unit(cell_height, "mm"),
    
    # Clustering parameters
    cluster_columns = cluster_columns,
    cluster_rows = FALSE,
    column_split = column_split,
    column_order = column_order,
    column_dend_height = column_dend_height,
    show_column_dend = show_column_dend,
    column_title = NULL,
    
    
    row_split = if (use_gene_families && !is.null(gene_family_frequency) && !is.null(data)) {
      factor(
        data$Gene_Family[match(rownames(mutation_matrix), data$Gene)],
        levels = gene_family_frequency
      )
    } else {
      NULL
    },
    row_title_rot = 0,  # Horizontal gene family labels
    row_title_gp = gpar(fontsize = 10, fontface = "bold"),
    
    row_names_gp = gpar(fontsize = 9, fontface = "bold"),
    row_names_side = "left",
    
    layer_fun = function(j, i, x, y, w, h, fill) {
      for (k in seq_along(i)) {
        gene <- rownames(mutation_matrix)[i[k]]
        patient <- colnames(mutation_matrix)[j[k]]
        key <- paste0(patient, "|", gene)
        
        # Collect ALL variants (SNVs and CNVs) with their germline status
        variants <- list()
        variants_germline <- c()
        
        # Add somatic SNVs
        som_types <- unlist(somatic_map[[key]])
        if (length(som_types) > 0) {
          for (vtype in som_types) {
            variants <- c(variants, list(vtype))
            variants_germline <- c(variants_germline, FALSE)
          }
        }
        
        # Add germline SNVs
        germ_types <- unlist(germline_map[[key]])
        if (length(germ_types) > 0) {
          for (vtype in germ_types) {
            variants <- c(variants, list(vtype))
            variants_germline <- c(variants_germline, TRUE)
          }
        }
        
        # Add somatic CNVs
        som_cnv <- if (!is.null(cnv_somatic_map)) unlist(cnv_somatic_map[[key]]) else character(0)
        if (length(som_cnv) > 0) {
          for (cnv_type in som_cnv) {
            variants <- c(variants, list(cnv_type))
            variants_germline <- c(variants_germline, FALSE)
          }
        }
        
        # Add germline CNVs
        germ_cnv <- if (!is.null(cnv_germline_map)) unlist(cnv_germline_map[[key]]) else character(0)
        if (length(germ_cnv) > 0) {
          for (cnv_type in germ_cnv) {
            variants <- c(variants, list(cnv_type))
            variants_germline <- c(variants_germline, TRUE)
          }
        }
        
        pt_to_in <- 1/72
        inset_bg <- 0.1 * pt_to_in
        inset_w <- convertWidth(unit(inset_bg, "inches"), "npc", valueOnly = FALSE)
        inset_h <- convertHeight(unit(inset_bg, "inches"), "npc", valueOnly = FALSE)
        
        # Draw background
        grid.rect(
          x = x[k], y = y[k],
          width = w[k] - 2*inset_w,
          height = h[k] - 2*inset_h,
          gp = gpar(fill = BG_COL, col = NA)
        )
        
        # Draw variants as split cells
        n_variants <- length(variants)
        
        if (n_variants > 0) {
          inset_pt <- 0.2 * pt_to_in
          inset_w2 <- convertWidth(unit(inset_pt, "inches"), "npc", valueOnly = FALSE)
          inset_h2 <- convertHeight(unit(inset_pt, "inches"), "npc", valueOnly = FALSE)
          inner_w <- w[k] - 2*inset_w2
          inner_h <- h[k] - 2*inset_h2
          cx <- x[k]
          cy <- y[k]
          
          # Calculate section height for each variant
          section_h <- inner_h / n_variants
          
          # Draw each variant section
          for (v in seq_len(n_variants)) {
            variant_type <- variants[[v]]
            is_germline <- variants_germline[v]
            
            # Calculate y position for this section (from top to bottom)
            section_y <- cy + inner_h/2 - section_h/2 - (v - 1) * section_h
            
            # Get color from combined colors
            variant_color <- if (!is.null(CNV_COLORS) && variant_type %in% names(CNV_COLORS)) {
              CNV_COLORS[variant_type]
            } else {
              type_colors[variant_type]
            }
            
            # Draw the colored section
            grid.rect(
              x = cx,
              y = section_y,
              width = inner_w,
              height = section_h,
              gp = gpar(fill = variant_color, col = NA)
            )
            
            # Draw border around germline variants
            if (is_germline) {
              grid.rect(
                x = cx,
                y = section_y,
                width = inner_w,
                height = section_h,
                gp = gpar(fill = NA, col = "black", lwd = 2.8)
              )
            }
            
            # Draw thin separator between sections (except for last one)
            if (v < n_variants) {
              sep_y <- section_y - section_h/2
              grid.lines(
                x = c(cx - inner_w/2, cx + inner_w/2),
                y = c(sep_y, sep_y),
                gp = gpar(col = "black", lwd = 1)
              )
            }
          }
        }
        
        # Draw cell border
        grid.rect(x[k], y[k], w[k], h[k], 
                 gp = gpar(fill = NA, col = "black", lwd = 1))
      }
    },
    
    # Combine clinical and cluster annotations
    top_annotation = {
      cluster_annot <- NULL
      
      # Create cluster annotation if file-based clustering
      if (clustering_strategy == "FILE" && !is.null(column_split)) {
        unique_clusters <- levels(column_split)
        n_clusters <- length(unique_clusters)
        cluster_colors <- setNames(
          rainbow(n_clusters, s = 0.7, v = 0.9),
          unique_clusters
        )
        
        cluster_annot <- HeatmapAnnotation(
          Cluster = column_split,
          col = list(Cluster = cluster_colors),
          annotation_name_side = "left",
          annotation_name_gp = gpar(fontsize = 10, fontface = "bold"),
          show_legend = TRUE,
          simple_anno_size = unit(5, "mm")
        )
      }
      
      # Combine annotations if both exist
      if (!is.null(clinical_annot) && !is.null(cluster_annot)) {
        # Stack clinical on top, then cluster
        c(clinical_annot, cluster_annot)
      } else if (!is.null(clinical_annot)) {
        clinical_annot
      } else if (!is.null(cluster_annot)) {
        cluster_annot
      } else {
        NULL
      }
    },
    
    # Right annotation - show mutation counts if requested
    right_annotation = if (show_mutation_counts) {
      rowAnnotation(
        Count = anno_barplot(
          combined_frequencies,
          border = FALSE,
          gp = gpar(fill = combined_colors[colnames(combined_frequencies)], 
                   col = combined_colors[colnames(combined_frequencies)])
        ),
        annotation_name_side = "top"
      )
    } else {
      NULL
    },
    
    # Don't show automatic legend 
    show_heatmap_legend = FALSE
  )
  
  # # Create single combined legend for both SNV and CNV alterations
  # combined_colors <- type_colors
  # if (!is.null(CNV_COLORS)) {
  #   combined_colors <- c(type_colors, CNV_COLORS)
  # }
  # 
  # # Single comprehensive legend
  # single_legend <- Legend(
  #   title = "Alterations",
  #   at = names(combined_colors),
  #   labels = names(combined_colors),
  #   legend_gp = gpar(fill = combined_colors),
  #   title_gp = gpar(fontsize = 11, fontface = "bold"),
  #   labels_gp = gpar(fontsize = 9),
  #   ncol = 2,  # Two columns for better space usage
  #   column_gap = unit(5, "mm")
  # )
  
  # Create single combined legend for both SNV and CNV alterations
  combined_colors <- type_colors
  
  if (!is.null(CNV_COLORS)) {
    # Find which CNV types are actually present in the DISPLAYED genes
    present_cnv_types <- c()
    
    # Get the genes that are actually shown in the heatmap
    displayed_genes <- rownames(mutation_matrix)
    
    # Check somatic CNV events (only for displayed genes)
    if (!is.null(cnv_somatic_map) && length(cnv_somatic_map) > 0) {
      # Create regex pattern to match keys with displayed genes
      # Keys are "Pt_ID|Gene", so we match "|Gene" at the end
      gene_pattern <- paste0("\\|(", paste(displayed_genes, collapse="|"), ")$")
      
      # Filter keys to only those with displayed genes
      displayed_keys <- grep(gene_pattern, names(cnv_somatic_map), value = TRUE)
      
      # Extract events only from displayed genes
      if (length(displayed_keys) > 0) {
        somatic_events <- unique(unlist(cnv_somatic_map[displayed_keys]))
        present_cnv_types <- c(present_cnv_types, somatic_events)
      }
    }
    
    # Check germline CNV events (only for displayed genes)
    if (!is.null(cnv_germline_map) && length(cnv_germline_map) > 0) {
      # Create regex pattern to match keys with displayed genes
      gene_pattern <- paste0("\\|(", paste(displayed_genes, collapse="|"), ")$")
      
      # Filter keys to only those with displayed genes
      displayed_keys <- grep(gene_pattern, names(cnv_germline_map), value = TRUE)
      
      # Extract events only from displayed genes
      if (length(displayed_keys) > 0) {
        germline_events <- unique(unlist(cnv_germline_map[displayed_keys]))
        present_cnv_types <- c(present_cnv_types, germline_events)
      }
    }
    
    # Get unique CNV types that are actually present in displayed genes
    present_cnv_types <- unique(present_cnv_types)
    
    # filter CNV_COLORS to only include types present in data
    if (length(present_cnv_types) > 0) {
      cnv_colors_filtered <- CNV_COLORS[names(CNV_COLORS) %in% present_cnv_types]
      combined_colors <- c(type_colors, cnv_colors_filtered)
    } else {
      # No CNV data present, just use mutation type colors
      combined_colors <- type_colors
    }
  } else {
    # No CNV_COLORS defined
    combined_colors <- type_colors
  }
  
  # Create separate legends for mutations (left) and CNV (right)
  mutation_legend <- Legend(
    title = "Mutations",
    at = names(type_colors),
    labels = names(type_colors),
    legend_gp = gpar(fill = type_colors),
    title_gp = gpar(fontsize = 11, fontface = "bold"),
    labels_gp = gpar(fontsize = 10),
    ncol = 1  # Single column for mutations
  )
  
  # Only create CNV legend if CNV data present
  if (!is.null(CNV_COLORS) && length(present_cnv_types) > 0) {
    cnv_colors_filtered <- CNV_COLORS[names(CNV_COLORS) %in% present_cnv_types]
    
    cnv_legend <- Legend(
      title = "CNV",
      at = names(cnv_colors_filtered),
      labels = names(cnv_colors_filtered),
      legend_gp = gpar(fill = cnv_colors_filtered),
      title_gp = gpar(fontsize = 11, fontface = "bold"),
      labels_gp = gpar(fontsize = 10),
      ncol = 1  # Single column for CNV
    )
    
    # Combine legends horizontally (side by side)
    # Mutations on LEFT, CNV on RIGHT
    combined_legend <- packLegend(
      mutation_legend, 
      cnv_legend,
      direction = "horizontal",
      column_gap = unit(8, "mm")  # Space between the two columns
    )
  } else {
    # No CNV data, just show mutation legend bazinga
    combined_legend <- mutation_legend
  }
  
  return(list(
    heatmap = ht,
    legends = combined_legend
  ))
}

## ========================================
## Launch App
## ========================================

shinyApp(ui, server)
