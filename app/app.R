library(shiny)
library(bslib)
library(magrittr)
library(DT)
options(shiny.maxRequestSize = 30 * 1024^2) # max 30 MB

# Load helper functions
source("helpers.R")

# filter regions for CNV analysis
filter_regions <- c("IKBKG", "CFHR", "FCGR3A", "FCGR3B", "FANCD2", "IGHG2")

filter_starts <- c("196743431", "74779061", "74780136", "5977567", "5978575", "23544939", "18176751", # nolint
                  "74774011", "74780759", "74785161", "74787963", "74789018", "44229512") # nolint

# UI - main layout with sidebar
ui <- page_sidebar(

  # app title in the navbar
  title = "Exom Analýza",

  # light background
  bg = "#f0f0f0",

  # custom HTML/CSS section
  tags$head(

    # set browser tab title
    tags$script(HTML("document.title = 'Exom Analýza';")),

    # custom styles (colors, spacing, etc.)
    tags$style(HTML("
      .navbar.navbar-static-top {
        background: #007BC2;
        background: linear-gradient(90deg, rgba(0, 123, 194, 1) 0%, rgba(255, 255, 255, 1) 25%); # nolint
      }
      .navbar-brand {
        color: #ffffff !important;
        font-weight: 700 !important;
        font-size: 26px !importnat;
      }
      .navbar.navbar-static-top {
        background: #007BC2;
        background: linear-gradient(90deg,rgba(0, 123, 194, 1) 0%, rgba(255, 255, 255, 1) 25%); # nolint
      }
      .navbar-brand {
        color: #ffffff !important;
        font-weight: 700 !important;
      }
      .btn-file {
        font-size: 16px;
        width: 100%;
      }
      .btn-file:hover {
        font-size: 16px;
      }
      .input-group,
      .input-group-prepend {
        width: 100% !important;
        padding-top: 0px !important;
        margin-top: 0px;
      }
      .gender-row {
        display: flex;
        align-items: center;
        gap: 0 !important; 
        margin-bottom: 5px;
        padding: 0;
      }
      .gender-label {
        width: 120px;
        text-align: right;
        padding-right: 10px;
      }
      .gender-select .form-group {
        margin: 0;
        width: 80px;
      }
      .card-body.bslib-gap-spacing {
        gap: 0 !important;
      }
      hr {
        margin-top: 5px;
        margin-bottom: 5px;
        border: 1px solid #ccc;
      }
    "))
  ),

  # upload .txt files - button + placeholder
  sidebar = sidebar(
    fileInput("file", NULL, multiple = TRUE, accept = ".txt", buttonLabel = "Vybrat soubory", # nolint
              placeholder = "Nevybrán žádný soubor", width = "100%"),

    # download buttons
    downloadButton("downloadCoveragemean", "Cov Mean ALL", class = "btn-lg btn-primary"), # nolint
    downloadButton("downloadCNVMmean", "CNV M Mean", class = "btn-lg btn-primary"), # nolint
    downloadButton("downloadCNVZmean", "CNV Z Mean", class = "btn-lg btn-primary"), # nolint
    #tags$hr(),
    #downloadButton("downloadCoverageproc", "Cov Procenta ALL", class = "btn-lg btn-primary"), # nolint
    #downloadButton("downloadCNVMproc", "CNV M Procenta", class = "btn-lg btn-primary"), # nolint
    #downloadButton("downloadCNVZproc", "CNV Z Procenta", class = "btn-lg btn-primary"), # nolint

    # horizontal divider
    tags$hr(),

    # link to OMIM database
    tags$a(
      href = "https://www.omim.org", target = "_blank",
      style = "font-weight: bold; font-size: 16px; display: block; margin-top: 10px;", # nolint
      icon("database"), "OMIM databáze"
    )
  ),

  # card with pannels - regions and gender, coverage mean all, cnv muži mean, cnv ženy mean # nolint
  card(
    tabsetPanel(
      id = "results_tabs",
      type = "tabs",
      tabPanel("Výběr regionů",
        tabPanel("Výběr regionů",
          # regions - filter regions + selecting
          h4("Filtr oblastí hg38", style = "margin-top: 30px; font-weight: bold;"), # nolint
          uiOutput("regions_selector"),
          # position start - filter start
          h4("Filtr hodnot start", style = "margin-top: 20px; font-weight: bold;"), # nolint
          uiOutput("position_starts"),
          # position end - filter end
          h4("Filtr hodnot stop", style = "margin-top: 20px; font-weight: bold;"), # nolint
          uiOutput("position_ends"),
          # horizontal divider
          tags$hr(),
          # chromosome X - with/without/only
          h4("Chromozom X", style = "margin-top: 20px; font-weight: bold;"), # nolint
          radioButtons(
            inputId = "chr_filter",
            label = "Výběr chromozomů pro analýzu",
            choices = c(
              "Všechny chromozomy" = "all",
              "Bez chromozomu X" = "noX",
              "Pouze chromozom X" = "onlyX"
            ),
            selected = "all"
          ),
          tags$hr(),
          # choosing gender male/female
          h4("Výběr pohlaví", style = "margin-top: 20px; font-weight: bold;"),
          p("U každého vzorku vyberte pohlaví žena/muž:"),
          uiOutput("gender_input"),
          uiOutput("action_button")
        )
      ),
      # Coverage Mean ALL
      tabPanel("Coverage Mean ALL",
        p("Průměrné (nenormalizované) pokrytí všech vzorků pro každý gen"),
        DT::dataTableOutput("coverage_table")
      ),
      # CNV male mean
      tabPanel("CNV Muži Mean",
        p("Normalizované pokrytí mužských vzorků – detekce CNV odchylek větších než 0,25"), # nolint
        DT::dataTableOutput("cnv_m")
      ),
      # CNV female mean
      tabPanel("CNV Ženy Mean",
        p("Normalizované pokrytí ženských vzorků – detekce CNV odchylek větších než 0,25"), # nolint
        DT::dataTableOutput("cnv_z")
      ),
      # Coverage procento ALL
      #tabPanel("Coverage procento ALL", p("Procentuální pokrytí všech vzorků pro každý gen"), DT::dataTableOutput("coverage_table_proc")),
      # CNV male procento
      #tabPanel("CNV Muži procento", p("Procentuální pokrytí mužských vzorků"), DT::dataTableOutput("cnv_m_proc")),
      # CNV female procento
      #tabPanel("CNV Ženy procento", p("Procentuální pokrytí ženských vzorků"), DT::dataTableOutput("cnv_z_proc"))
    )
  )
)

######################################################################################################################## # nolint

# Server logic
server <- function(input, output, session) {

  # extract sample IDs from filenames without suffix
  sample_id <- reactive({
    req(input$file)
    gsub(".coveragefin\\.txt$", "", input$file$name)
  })

  # dynamiccaly generate a gender selector for each sample
  output$gender_input <- renderUI({
    ids <- sample_id()
    lapply(ids, function(id) {
      div(class = "gender-row",
        div(class = "gender-label", strong(id)),
        div(class = "gender-select",
          selectInput(
            inputId = paste0("pohlavi", id),
            label = NULL,
            choices = c("Muž" = "M", "Žena" = "Z"),
            width = "80px",
            selectize = TRUE
          ),
        )
      )
    })
  })

  # reactive values
  final_data <- reactiveVal()
  final_data_proc <- reactiveVal()
  pohlavi_data <- reactiveVal()
  cnv_m_data <- reactiveVal()
  cnv_z_data <- reactiveVal()
  cnv_m_data_proc <- reactiveVal()
  cnv_z_data_proc <- reactiveVal()
  submit_status <- reactiveVal("ready")
  regions_data <- reactiveVal(NULL)

  regions <- reactive({
    selected_regions <- Filter(function(x) x != "", input$regions)
    if (length(selected_regions) == 0) {
      return(NULL)
    } else {
      return(selected_regions)
    }
  })

  starts <- reactive({
    selected_starts <- Filter(function(x) x != "", input$starts)
    if (length(selected_starts) == 0) {
      return(NULL)
    } else {
      return(selected_starts)
    }
  })

  # show a green submit button 'Zpracovat' after file upload
  output$action_button <- renderUI({
    #req(input$file)
    actionButton(
      "submit",
      label = "Zpracovat",
      icon = icon("check"),
      class = if (submit_status() == "processing") "btn btn-primary" else "btn btn-success", # nolint
      style = "margin-top: 5px; width: 200px; font-size: 20px; padding: 10px;"
    )
  })

  # regions selector - allows to select regions to be excluded from CNV analysis
  output$regions_selector <- renderUI({
    #if (is.null(input$file) || is.null(regions_data())) {
    if (is.null(input$file)) {
      # blank space before files are loaded
      selectInput(
        "regions",
        label = "Následující oblasti budou vyloučeny:",
        choices = "",
        #selected = filter_regions,
        width = "25%",
        multiple = TRUE
      )
    } else {
      # after files are loaded
      all_genes <- unique(c(filter_regions, regions_data()))
      tagList(
        selectInput(
          "regions",
          label = "Následující oblasti budou vyloučeny:",
          choices = all_genes,
          selected = filter_regions,
          width = "100%",
          multiple = TRUE
        ),
        helpText("Oblasti vybrané v tomto seznamu budou z CNV analýzy vyřazeny. Pokud výběr necháte prázdný, budou zahrnuty všechny oblasti.") # nolint
      )
    }
  })

  #position start selector - allows to select regions to be excluded from CNV analysis # nolint
  output$position_starts <- renderUI({
    if (is.null(input$file)) {
      # blank space before files are loaded
      selectInput(
        "starts",
        label = "Filtruj podle počáteční pozice:",
        choices = "",
        #selected = filter_starts,
        width = "25%",
        multiple = TRUE
      )
    } else {
      # after files are loaded
      tagList(
        selectizeInput(
          "starts",
          label = "Filtruj podle počáteční pozice:",
          choices = filter_starts,
          selected = filter_starts,
          multiple = TRUE,
          options = list(create = TRUE),
          width = "100%"
        ),
        helpText("Oblasti vybrané v tomto seznamu budou z CNV analýzy vyřazeny. Pokud výběr necháte prázdný, budou zahrnuty všechny oblasti.") # nolint
      )
    }
  })

  # after upload - extract unique gene names from the 4th column of each file
  observeEvent(input$file, {
    withProgress(message = "Načítám seznam oblastí...", value = 0, {
      incProgress(0.2, detail = "Čtení souborů...")

      dfs <- lapply(input$file$datapath, function(path) {
        read.delim(path, check.names = FALSE)
      })
      gene_names <- unique(unlist(lapply(dfs, function(df) {
        df[[4]]
      })))
      cat("---gene_names---\n")
      print(head(gene_names, 5))

      incProgress(0.9, detail = "Dokončuji...")
      regions_data(gene_names)
    })
  })

  #position end selector - allows to select regions to be excluded from CNV analysis # nolint
  output$position_ends <- renderUI({
    if (is.null(input$file)) {
      # blank space before files are loaded
      selectInput(
        "ends",
        label = "Filtruj podle koncové pozice:",
        choices = "",
        #selected = filter_starts,
        width = "25%",
        multiple = TRUE
      )
    } else {
      # after files are loaded
      tagList(
        selectizeInput(
          "ends",
          label = "Filtruj podle koncové pozice:",
          choices = "",
          selected = "",
          multiple = TRUE,
          options = list(create = TRUE),
          width = "100%"
        ),
        helpText("Oblasti vybrané v tomto seznamu budou z CNV analýzy vyřazeny. Pokud výběr necháte prázdný, budou zahrnuty všechny oblasti.") # nolint
      )
    }
  })

  # when the submit button is clicked, process the data
  observeEvent(input$submit, {
    req(input$file)
    submit_status("processing")

    withProgress(message = "Zpracování CNV...", value = 0, {
      submit_status("processing")

      # Step 1: load coverage data from all files
      incProgress(0.1, detail = "Načítání souborů...")

      file_list <- input$file$datapath
      filenames <- input$file$name
      ids <- sample_id()

      # get gender selections from input
      pohlavi <- sapply(ids, function(id) input[[paste0("pohlavi", id)]])
      pohlavi_df <- data.frame(ID = ids, Gender = pohlavi)
      pohlavi_data(pohlavi_df)

      # Step 2: extract MEAN and PERCENTAGE columns from each input file
      incProgress(0.3, detail = "Generování coverage dat...")

      # MEAN
      # extract 5th column for each file - It loops over all uploaded files and extracts the 5th column (mean coverage).  # nolint
      # It then labels each column with the gender and sample ID and returns a list of selected columns.  # nolint
      selected_cols_list <- lapply(seq_along(file_list), function(i) {
        tryCatch({
          df <- read.delim(file_list[i], check.names = FALSE)
          df$chr <- gsub('"', '', df$chr)
          selected <- df[, 5, drop = FALSE]
          base_name <- tools::file_path_sans_ext(gsub(".coveragefin\\.txt$", "", filenames[i])) # nolint
          gender <- input[[paste0("pohlavi", ids[i])]]
          colnames(selected) <- paste0(gender, "_", base_name)
          return(selected)
          cat("---gender---\n")
          print(gender)
        }, error = function(e) {
          showNotification(paste("Chyba u souboru:", filenames[i]), type = "error") # nolint
          return(NULL)
        })
      })

      # PERCENTAGE
      # extract 6th column for each file - It processes each uploaded file to extract the 6th column (percentage coverage),  # nolint
      # labels it with gender and sample ID, and returns a list of these columns.  # nolint
      selected_cols_list_proc <- lapply(seq_along(file_list), function(i) {
        tryCatch({
          df <- read.delim(file_list[i], check.names = FALSE)
          selected <- df[, 6, drop = FALSE]
          base_name <- tools::file_path_sans_ext(gsub(".coveragefin\\.txt$", "", filenames[i])) # nolint
          gender <- input[[paste0("pohlavi", ids[i])]]
          colnames(selected) <- paste0(gender, "_", base_name)
          return(selected)
        }, error = function(e) {
          showNotification(paste("Chyba u souboru:", filenames[i]), type = "error") # nolint
          return(NULL)
        })
      })

      # combine data
      result <- do.call(cbind, selected_cols_list)
      result_proc <- do.call(cbind, selected_cols_list_proc)
      prvni_trisloupce <- read.delim(file_list[1], check.names = FALSE)[, 1:4] # nolint
      combined <- cbind(prvni_trisloupce, result)
      #cat("---combined---\n")
      #print(head(combined), 5)

      combined_proc <- cbind(prvni_trisloupce, result_proc)

      colnames(combined) <- trimws(gsub(".COV-mean", "", colnames(combined), fixed = TRUE)) # nolint
      colnames(combined_proc) <- trimws(gsub(".COV-procento", "", colnames(combined_proc), fixed = TRUE)) # nolint

      # filter out regions, start and stop positions that are in the selected regions
      selected_names <- regions()
      if (!is.null(regions()) && length(regions()) > 0) {
        pattern <- paste0("\\b(", paste(selected_names, collapse = "|"), ")[A-Za-z0-9_]*\\b") # nolint
        combined <- combined[!grepl(pattern, combined$name), ]
      }

      if (!is.null(input$starts) && length(input$starts) > 0) {
        combined <- combined[!combined$start %in% input$starts, ] # nolint
      }

      if (!is.null(input$ends) && length(input$ends) > 0) { # nolint
        combined <- combined[!combined$stop %in% input$ends, ]
      }

      # filter out regions, start and stop positions in percent coverage data
      if (!is.null(input$regions_proc) && length(input$regions_proc) > 0) {
        pattern <- paste0("\\b(", paste(input$regions_proc, collapse = "|"), ")[A-Za-z0-9_]*\\b") # nolint
        combined_proc <- combined_proc[!grepl(pattern, combined_proc$name), ]
      }

      if (!is.null(input$starts_proc) && length(input$starts_proc) > 0) {
        combined_proc <- combined_proc[!combined_proc$start %in% input$starts_proc, ] # nolint
      }

      if (!is.null(input$ends_proc) && length(input$ends_proc) > 0) {
        combined_proc <- combined_proc[!combined_proc$stop %in% input$ends_proc, ] # nolint
      }

      # filter with chromosome X selection
      chr_selected <- input$chr_filter
      if (chr_selected == "noX") {
        combined <- combined[combined$chr != "X", ]
        combined_proc <- combined_proc[combined_proc$chr != "X", ]
      } else if (chr_selected == "onlyX") {
        combined <- combined[combined$chr == "X", ]
        combined_proc <- combined_proc[combined_proc$chr == "X", ]
      } else if (chr_selected == "all") {
        combined <- combined
        combined_proc <- combined_proc
      }

      final_data(combined)
      final_data_proc(combined_proc)
      #cat("---final_data---\n")
      #print(head(final_data(), 5))

      # Step 3: CNV detection
      incProgress(0.6, detail = "Normalizace CNV M...")

      # CNV logic
      coverage <- final_data()
      coverage_proc <- final_data_proc()
      pohlavi <- pohlavi_data()
      m <- colnames(coverage)[grepl("^M_", colnames(coverage))]
      z <- colnames(coverage)[grepl("^Z_", colnames(coverage))]
      m_p <- colnames(coverage_proc)[grepl("^M_", colnames(coverage_proc))]
      z_p <- colnames(coverage_proc)[grepl("^Z_", colnames(coverage_proc))]
      #cat("---m---\n")
      #print(head(m), 10)
      #cat("---z---\n")
      #print(head(z), 10)

      # load OMIM gene annotations
      omimgeny <- load_omim_file()

      # CNV analysis
      # First, it normalizes the coverage values for male/female-specific columns using normalize_coverage(). # nolint
      # It adds a unique row_id to help with tracking rows after transformations. # nolint
      # Then it combines the genomic position (chr, start, stop, name, row_id) with the normalized values. # nolint
      # It checks which normalized values exceed the CNV threshold of ±0.25.
      # Only rows with at least one such value are selected for further analysis. # nolint
      # These filtered rows are then annotated with phenotype data from the OMIM reference using annotate_with_omim(). # nolint
      # Finally, the result is stored in the cnv_m_data() or cnv_z_data() reactive variable for use in the UI. # nolint

      # CNV for males
      if (length(m) > 0) {
        normalized_m <- normalize_coverage(coverage[, m, drop = FALSE])
        coverage$row_id <- seq.int(nrow(coverage))
        coverage_m_final <- cbind(
          coverage[, c("chr", "start", "stop", "name", "row_id")],
          normalized_m
        )
        coverage_cols <- coverage_m_final[, -c(1:5), drop = FALSE]
        m_values <- abs(coverage_cols) > 0.25
        greater_m <- coverage_m_final[rowSums(m_values, na.rm = TRUE) > 0, ]
        greater_m <- annotate_with_omim(greater_m, omimgeny)
        cnv_m_data(greater_m)

        # save percentage data for males - 
        coverage_proc$row_id <- seq_len(nrow(coverage_proc))
        cnv_m_data_proc(
          cbind(
            coverage_proc[, c("chr", "start", "stop", "name", "row_id")],
            coverage_proc[, m_p, drop = FALSE]
          )
        )
      }

      # CNV for females
      incProgress(0.8, detail = "Normalizace CNV Z...")
      if (length(z) > 0) {
        normalized_z <- normalize_coverage(coverage[, z, drop = FALSE])
        coverage$row_id <- seq.int(nrow(coverage))
        coverage_z_final <- cbind(
          coverage[, c("chr", "start", "stop", "name", "row_id")],
          normalized_z
        )
        coverage_cols <- coverage_z_final[, -c(1:5), drop = FALSE]
        z_values <- abs(coverage_cols) > 0.25
        greater_z <- coverage_z_final[rowSums(z_values, na.rm = TRUE) > 0, ]
        greater_z <- annotate_with_omim(greater_z, omimgeny)
        cnv_z_data(greater_z)

        # save percentage data for females
        cnv_z_data_proc(cbind(coverage_proc[, c(1:5)], coverage_proc[, z_p, drop = FALSE])) # nolint
        cnv_z_data_proc(
          cbind(
            coverage_proc[, c("chr", "start", "stop", "name", "row_id")],
            coverage_proc[, z_p, drop = FALSE]
          )
        )
      }
      incProgress(1, detail = "Hotovo")
    })

    # mark processing as done
    submit_status("ready")
  })

  # Tables
  # the main coverage table - Renders the combined mean coverage data in a scrollable DataTable  # nolint
  # with pagination and horizontal/vertical scrolling.
  output$coverage_table <- DT::renderDataTable({
    req(final_data())
    df <- final_data()
    validate(need(nrow(df) > 0, "Žádná data pro pokrytí"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE
      )
    )
  })

  # CNV results for males
  # Shows normalized coverage deviations for male samples, with annotation from OMIM. # nolint
  output$cnv_m <- DT::renderDataTable({
    req(cnv_m_data())
    df <- cnv_m_data()
    validate(need(nrow(df) > 0, "Žádná data pro CNV M"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE,
        columnDefs = list(
          list(
            targets = which(colnames(df) == "OMIM"),
            width = "800px"
          )
        )
      ),
      escape = FALSE
    ) %>%
    DT::formatStyle(
      "OMIM",
      `white-space` = "normal"
    )
  })

  # CNV results for females
  # Shows normalized coverage deviations for female samples, with annotation from OMIM. # nolint
  output$cnv_z <- DT::renderDataTable({
    req(cnv_z_data())
    df <- cnv_z_data()
    validate(need(nrow(df) > 0, "Žádná data pro CNV Z"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE,
        columnDefs = list(
          list(
            targets = which(colnames(df) == "OMIM"),
            width = "800px"
          )
        )
      ),
      escape = FALSE
    ) %>%
    DT::formatStyle(
      "OMIM",
      `white-space` = "normal"
    )
  })

  # coverage data in percentages
  # This table displays percent coverage values before normalization.
  output$coverage_table_proc <- DT::renderDataTable({
    req(final_data_proc())
    df <- final_data_proc()
    validate(need(nrow(df) > 0, "Žádná data pro procenta pokrytí"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE
      )
    )
  })

  # raw percentage values for CNV M
  # Displays coverage percentage for male samples without normalization.
  output$cnv_m_proc <- DT::renderDataTable({
    req(cnv_m_data_proc())
    df <- cnv_m_data_proc()
    validate(need(nrow(df) > 0, "Žádná data pro procenta CNV M"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE
      )
    )
  })

  # raw percentage values for CNV Z
  # Displays coverage percentage for female samples without normalization.
  output$cnv_z_proc <- DT::renderDataTable({
    req(cnv_z_data_proc())
    df <- cnv_z_data_proc()
    validate(need(nrow(df) > 0, "Žádná data pro procenta CNV Z"))
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE
      )
    )
  })

  # downloads the complete mean coverage table
  output$downloadCoveragemean <- downloadHandler(
    filename = function() { "coveragemeanALL.csv" },
    content = function(file) {
      write.csv2(final_data(), file, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8") # nolint
    }
  )

  # downloads normalized CNV results for males
  output$downloadCNVMmean <- downloadHandler(
    filename = function() { "CNV_M_mean.csv" },
    content = function(file) {
      write.csv2(cnv_m_data(), file, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8") # nolint
    }
  )

  # downloads normalized CNV results for females
  output$downloadCNVZmean <- downloadHandler(
    filename = function() { "CNV_Z_mean.csv" },
    content = function(file) {
      write.csv2(cnv_z_data(), file, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8") # nolint
    }
  )

  # downloads the full percentage coverage table
  output$downloadCoverageproc <- downloadHandler(
    filename = function() { "coverageprocentoALL.csv" },
    content = function(file) {
      write.csv2(final_data_proc(), file, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8") # nolint
    }
  )

  # downloads percentage CNV data for males
  output$downloadCNVMproc <- downloadHandler(
    filename = function() { "CNV_M_procento.csv" },
    content = function(file) {
      write.csv2(cnv_m_data_proc(), file, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8") # nolint
    }
  )

  # downloads percentage CNV data for females
  output$downloadCNVZproc <- downloadHandler(
    filename = function() { "CNV_Z_procento.csv" },
    content = function(file) {
      write.csv2(cnv_z_data_proc(), file, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8") # nolint
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
