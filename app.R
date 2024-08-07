# Computation of exemplar-wise vowel-space metrics separately for each relevant subgroup (e.g. speaker or speaker*condition), after Audibert, Fougeron, Gendrot and Adda-Decker, 2015
# 
# The following metrics are computed, as well as complementary information from the LDA (predicted vowel category, posterior probability belonging to the predicted category)
# DistCentroid
# VDispersion
# ContrastLoss
#
# As an option, vowel-space and category-wise centroid values used in the computation of metrics DistCentroid and VDispersion and/or complementary information from the LDA (predicted vowel category, posterior probability belonging to the predicted category) can be exported.
# Computed metrics and selected complementary information as added to the input data as new columns
#
# Adaptation as a Shiny app of the code used for the computation of metrics in the following ICPhS paper:
# Hermes, A., Audibert, N., & Bourbon, A. (2023). Age-related vowel variation in French. In 20th International Congress of Phonetic Sciences (ICPhS 2023), Prague, Czech Republic
#
# Author: Nicolas Audibert, Laboratoire de Phonétique et Phonologie, CNRS & Sorbonne Nouvelle
# Last modified August 2024

library(shiny)
library(shinyFiles)
library(shinythemes)
library(shinyjs)
library(rhandsontable)
library(tidyverse)
library(readxl)
library(writexl)
library(MASS)

defaultTheme = "spacelab"
maxUploadSizeMB = Inf
htmlAppDescriptionFile = "metrics_computation_app.html"

# Utility functions
# Convert Hertz to Bark (after Traunmüller, 1990)
hertzToBark <- function(hertzValue) {
  return(26.81/(1+1960/hertzValue) - 0.53)
}
# Get the distance between two points defined by their coordinates
distanceBetweenCoordinatesVectors <- function(point1_coord, point2_coord, distanceType = "euclidean") {
  dataMat <- t(matrix(c(point1_coord, point2_coord), ncol = 2))
  return(as.numeric(stats::dist(dataMat, method = distanceType)))
}
# Get LDA predictions and posterior probability to belong to either the reference class or the predicted class
getLDApredictionsWithProbabilities <- function(itemIdVect, classVect, predictorsLabelsVect, predictorsValuesVect) {
  df <- tibble(
    itemId = itemIdVect,
    classCol = classVect,
    predictorsLabelsCol = predictorsLabelsVect,
    predictorsValuesCol = predictorsValuesVect
  )
  predictorsLabels <- df %>% 
    distinct(predictorsLabelsCol) %>% 
    pull(predictorsLabelsCol)
  df <- df %>% 
    pivot_wider(names_from = predictorsLabelsCol, values_from = predictorsValuesCol)
  itemIds <- df %>% pull(itemId)
  ref <- df %>% pull(classCol)
  lda.model <- lda(reformulate(predictorsLabels, response = "classCol"), data = df, CV = TRUE)
  lda.model.predicted <- tibble(itemId = itemIds, classCol = ref, ldaPredictedCategory = lda.model$class %>% as.character())
  lda.model.posterior <- lda.model$posterior %>% 
    as_tibble() %>% 
    bind_cols(lda.model.predicted) %>% 
    pivot_longer(cols = c(-itemId, -classCol, -ldaPredictedCategory), names_to = "posteriorClassLabel", values_to = "posteriorProbability")
  lda.model.posterior.ref <- lda.model.posterior %>% 
    filter(posteriorClassLabel==classCol) %>% 
    mutate(ContrastLoss = 1 - posteriorProbability) %>% 
    dplyr::select(itemId, ContrastLoss)
  lda.model.posterior.predicted <- lda.model.posterior %>% 
    filter(posteriorClassLabel==ldaPredictedCategory) %>% 
    dplyr::select(itemId, posteriorProbability) %>% 
    rename(ldaProbPred = posteriorProbability)
  ldaResults <- lda.model.predicted %>% 
    left_join(lda.model.posterior.ref, by = "itemId") %>% 
    left_join(lda.model.posterior.predicted, by = "itemId")
  return(ldaResults)
}

# Shiny app
# Define UI
ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme(defaultTheme),
  titlePanel("Exemplar-wise vowel space metrics computation"),
  htmlOutput("app_description"),
  # Select input file
  fileInput("file", "Select input file",
            multiple = FALSE,
            accept = c(".csv", ".xlsx", ".tsv", ".txt")),
  
  # User-defined parameters
  fluidRow(
    column(width=3, selectInput("group_columns", "Grouping variable(s):", choices = NULL, multiple = TRUE)),
    column(width=3, selectInput("vowel_column", "Vowel category variable:", choices = NULL))
  ),
  fluidRow(
    column(
      width=12,
      h5("Select the variables that contain the formant values (or MFCC or other relevant parameters for metrics computation). You can either enter a regular expression below and click on the Select button, or directly tick the variables to be selected in the table that groups all variables not already selected as grouping variables or to identify vowel categories.")
    )
  ),
  fluidRow(
    column(width=7, textInput("formants_cols_regex", "Formants columns selection regex", width = "100%")),
    column(width=3, selectInput("formants_cols_regex_type", "Regex type", choices = c("starts with", "ends with", "regex"))),
    column(width=2, actionButton("formants_cols_regex_button", "Select"))
  ),
  rHandsontableOutput("formants_cols_table", height = 120),
  fluidRow(
    column(width=3, checkboxInput("compute_distances_in_bark", "Convert values from Hertz to Bark", value = FALSE)),
    column(width=3, selectInput("distance_type", "Distance type:", choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"))),
    column(width=3, checkboxInput("include_centroid_values_in_exported_data", "Include centroid values in exported data", value = FALSE)),
    column(width=3, checkboxInput("include_lda_predicted_category_with_probability_in_exported_data", "Include LDA predicted category with probability in exported data", value = TRUE))
  ),
  actionButton("compute_button", "Compute metrics"),
  downloadButton("downloadData", label = "Download results")
)

# Define server logic
server <- function(input, output, session) {
  # Set the file upload limit
  base::options(shiny.maxRequestSize = maxUploadSizeMB * 1024^2)
  
  # Hide action buttons
  shinyjs::hide("compute_button")
  shinyjs::hide("downloadData")
  shinyjs::hide("formants_cols_regex_button")
  
  # HTML displayed on top of the page (contents loaded from file)
  output$app_description <- renderUI({
    displayedHTML = readLines(htmlAppDescriptionFile)
    HTML(paste(displayedHTML))
  })
  
  processedData <- reactiveVal() # to store the processed data
  
  # Reactive value for input dataset
  inputDataset <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    processedData(NULL)
    shinyjs::hide("compute_button")
    shinyjs::hide("downloadData")
    # Read file depending on extension
    if (ext %in% c("tsv", "txt")) {
      dataset <- read_tsv(input$file$datapath, show_col_types = FALSE)
    } else if (ext == "csv") {
      dataset <- read_csv(input$file$datapath, show_col_types = FALSE)
    } else if (ext == "xlsx") {
      dataset <- readxl::read_xlsx(input$file$datapath, sheet = 1)
    } else {
      dataset <- NULL
      stop("Unsupported file type")
    }
    return(dataset)
  })
  
  observeEvent(input$file, {
    updateSelectInput(
      session,
      "group_columns",
      "Grouping variable(s):",
      choices = names(inputDataset()),
      selected = ""
    )
    updateSelectInput(
      session,
      "vowel_column",
      "Vowel category variable:",
      choices = names(inputDataset()),
      selected = ""
    )
  })
  
  formantsCols <- reactiveVal()
  formantsColsDF <- reactiveVal()
  groupCols <- reactiveVal()
  vowelCol <- reactiveVal()
  
  observeEvent(input$formants_cols_table, {
  # formantsCols <- reactive({
    formantsColsDF(hot_to_r(input$formants_cols_table))
    selectedColnames <- formantsColsDF() %>% 
      filter(Selected) %>% 
      pull(Colname)
    if(length(selectedColnames) > 0)
      shinyjs::show("compute_button")
    formantsCols(selectedColnames)
  })
  
  # Get the Excel column letter ID from the row index
  colIndex2ExcelLettersID <- function(colIndex) {
    letters = ''
    while (colIndex > 0) {
      r = (colIndex - 1) %% 26  # remainder
      letters = paste0(intToUtf8(r + utf8ToInt('A')), letters) # ascii
      colIndex = (colIndex - 1) %/% 26 # quotient
    }
    return(letters)
  }
  colIndex2ExcelLettersID <- Vectorize(colIndex2ExcelLettersID)
  
  updateFormantsColsTable <- function() {
    if(!is.null(input$group_columns) && !is.null(input$vowel_column) && all(input$group_columns!="") && input$vowel_column!="") {
      formantsColsDF(
        tibble(
          Colname = names(inputDataset())
        ) %>% 
          mutate(
            ColNum = row_number(),
            ColLetter = colIndex2ExcelLettersID(ColNum),
            Selected = FALSE
          ) %>% 
          filter(!Colname %in% c(input$group_columns, input$vowel_column)) %>% 
          dplyr::select(ColNum, ColLetter, Colname, Selected)
      )
      output$formants_cols_table <- renderRHandsontable(
        rhandsontable(formantsColsDF(), selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
          hot_col(col = 1, readOnly = TRUE)  %>%
          hot_col(col = 2, readOnly = TRUE)  %>%
          hot_col(col = 3, readOnly = TRUE)  %>%
          hot_col(col = 4, type = "checkbox") %>%
          hot_cols(columnSorting = TRUE)
      )
    } else {
      output$formants_cols_table <- renderRHandsontable(NULL)
    }
  }
  
  observeEvent(input$group_columns, {
    updateFormantsColsTable()
  })
  observeEvent(input$vowel_column, {
    updateFormantsColsTable()
  })
  
  # Regex selection of formants columns
  observeEvent(input$formants_cols_regex, {
    if(input$formants_cols_regex!="")
      shinyjs::show("formants_cols_regex_button")
    else
      shinyjs::hide("formants_cols_regex_button")
  })
  
  observeEvent(input$formants_cols_regex_button, {
    if(input$formants_cols_regex!="") {
      formantsColsDF(formantsColsDF() %>% 
        mutate(
          # Reset columns selection
          Selected = FALSE,
          # ...and select columns matching the regex instead
          Selected = case_when(
            input$formants_cols_regex_type == "starts with" ~ if_else(startsWith(Colname, input$formants_cols_regex), TRUE, FALSE),
            input$formants_cols_regex_type == "ends with" ~ if_else(endsWith(Colname, input$formants_cols_regex), TRUE, FALSE),
            input$formants_cols_regex_type == "regex" ~ if_else(str_detect(Colname, input$formants_cols_regex), TRUE, FALSE),
            TRUE ~ FALSE # Default to not selected
          )
        )
      )
      output$formants_cols_table <- renderRHandsontable(
        rhandsontable(formantsColsDF(), selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
          hot_col(col = 1, readOnly = TRUE)  %>%
          hot_col(col = 2, readOnly = TRUE)  %>%
          hot_col(col = 3, readOnly = TRUE)  %>%
          hot_col(col = 4, type = "checkbox") %>%
          hot_cols(columnSorting = TRUE)
      )
    }
  })
  
  # Compute metrics and create output file
  observeEvent(input$compute_button, {
    withProgress(message = "Computing metrics...", value =0 , {
      # Extract user-defined parameters
      groupCols(input$group_columns)
      vowelCol(input$vowel_column)
      computeDistancesInBark <- input$compute_distances_in_bark
      includeCentroidValuesInExportedData <- input$include_centroid_values_in_exported_data
      includeLDApredictedCategoryWithProbabilityInExportedData <- input$include_lda_predicted_category_with_probability_in_exported_data
      distanceType <- input$distance_type 
      
      # Add a temporary item id column (removed before exporting results)
      dataset <- inputDataset() %>% 
        mutate(tmpItemId = row_number())
      # Convert formant values from Hz to Bark if requested
      if(computeDistancesInBark) {
        dataset <- dataset %>% 
          mutate(across(all_of(formantsCols()), hertzToBark))
      }
      
      # Compute centroids
      # Vowel category-wise centroids
      centroids_categ <- dataset %>% 
        group_by(across(all_of(c(groupCols(), vowelCol())))) %>% 
        summarise(
          across(all_of(formantsCols()), mean),
          nExemplars = n(),
          .groups = "drop"
        )
      # Vowel space centroids (centroid of vowel category centroids)
      centroids_global <- centroids_categ %>% 
        group_by(across(all_of(groupCols()))) %>% 
        summarise(
          across(all_of(formantsCols()), mean),
          nVowelCategories = n(),
          .groups = "drop"
        )
      
      # Convert dataframes from large to long format for data pairing and distances computation
      centroids_categ_long <- centroids_categ %>% 
        dplyr::select(-nExemplars) %>% 
        pivot_longer(cols = all_of(formantsCols()), names_to = "formantNumber", values_to = "vowelCategoryCentroid")
      centroids_global_long <- centroids_global %>% 
        dplyr::select(-nVowelCategories) %>% 
        pivot_longer(cols = all_of(formantsCols()), names_to = "formantNumber", values_to = "vowelSpaceCentroid")
      dataset_long <- dataset %>% 
        pivot_longer(cols = all_of(formantsCols()), names_to = "formantNumber", values_to = "formantValue") %>% 
        left_join(centroids_categ_long, by = c(groupCols(), vowelCol(), "formantNumber")) %>% 
        left_join(centroids_global_long, by = c(groupCols(), "formantNumber"))
      
      # Compute distances. The resulting data frame has the same number of rows as the original (large) data frame.
      distancesCentroids <- dataset_long %>% 
        group_by(tmpItemId) %>% 
        summarise(
          VDispersion = distanceBetweenCoordinatesVectors(formantValue, vowelCategoryCentroid, distanceType = distanceType),
          DistCentroid = distanceBetweenCoordinatesVectors(formantValue, vowelSpaceCentroid, distanceType = distanceType),
          .groups = "drop"
        )
      # Add distances to the original data frame
      dataset <- dataset %>% 
        left_join(distancesCentroids, by = "tmpItemId")
      # Add centroid values to the original data frame if requested
      if(includeCentroidValuesInExportedData) {
        dataset <- dataset %>% 
          left_join(
            centroids_categ %>%
              rename_with(~str_c("vowelCategMean", .), all_of(formantsCols())),
            by = c(groupCols(), vowelCol())
          ) %>%
          left_join(
            centroids_global %>%
              rename_with(~str_c("vowelSpaceMean", .), all_of(formantsCols())),
            by = groupCols()
          )
      }
      
      # Get LDA results. The resulting data frame has the same number of rows as the original (large) data frame.
      # Since LDA operates on all rows belonging to the same group and returns rowwise results, use nest_by to get a (temporary) distinct data frame for each group.
      ldaResults <- dataset_long %>% 
        nest_by(across(all_of(groupCols())), .key = "tmpDataCol") %>% 
        mutate(ldaResults = list(getLDApredictionsWithProbabilities(tmpDataCol %>% pull(tmpItemId), tmpDataCol %>% pull(all_of(vowelCol())), tmpDataCol %>% pull(formantNumber), tmpDataCol %>% pull(formantValue)))) %>% 
        dplyr::select(-tmpDataCol) %>% 
        unnest(cols = ldaResults) %>% 
        ungroup()

      # Discard predicted category and probability of belonging to the predicted category if not requested
      if(!includeLDApredictedCategoryWithProbabilityInExportedData) {
        ldaResults <- ldaResults %>% 
          dplyr::select(-ldaPredictedCategory, -ldaProbPred)
      }
      # Add LDA results to the original data frame
      dataset <- dataset %>% 
        left_join(ldaResults %>% dplyr::select(-all_of(groupCols()), -classCol), by = c("tmpItemId" = "itemId"))
      
      # Drop temporary item id col
      dataset <- dataset %>% 
        dplyr::select(-tmpItemId)
      
      incProgress(.5, message = "Exporting data")
      processedData(list(data = dataset, categCentroids = centroids_categ, globalCentroids = centroids_global))
      shinyjs::show("downloadData")
      incProgress(1, message = "Complete")
    })
  })
    
  # DOWNLOAD BUTTON 
  output$downloadData <- downloadHandler(
    filename = function() { 
      str_c(
        str_replace(
          input$file$name,
          str_c("\\.", tools::file_ext(input$file$name), "$"),
          ""
        ),
        "_with_metrics.xlsx"
      )
      # tempfile(fileext = ".xlsx")
    },
    content = function(file) {
      write_xlsx(
        processedData(),
        path = file,
        format_headers = F
      )
    })
}

# Run app
shinyApp(ui, server)
