library(tidyverse)
library(readxl)
library(writexl)
library(MASS)

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
# Code used for the computation of metrics in the following ICPhS paper:
# Hermes, A., Audibert, N., & Bourbon, A. (2023). Age-related vowel variation in French. In 20th International Congress of Phonetic Sciences (ICPhS 2023), Prague, Czech Republic
#
# Author: Nicolas Audibert, Laboratoire de Phonétique et Phonologie, CNRS & Sorbonne Nouvelle
# Last modified January 2023

# User-defined parameters
fileNameIn = "acoustic_analysis/acoustic_analysis_aging_ICPhS_final_selection_MFCC.xlsx"
fileNameOut = "acoustic_analysis/acoustic_analysis_aging_ICPhS_final_selection_MFCC_with_metrics.xlsx"
groupColumns = "spkCode" # Must be the name(s) of one or several of the columns in input data file fileNameIn
vowelColumn = "label" # Must be the name of one of the columns in input data file fileNameIn
computeDistancesInBark <- FALSE
distanceType <- "euclidean" # Possible values: "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
includeCentroidValuesInExportedData <- FALSE
includeLDApredictedCategoryWithProbabilityInExportedData <- TRUE

# Which formant dimensions (or other, e.g. MFCC) are considered in distances computation
formantsCols <- str_c("MFCC", 1:12) # Use MFCC coefficients 1 to 12

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
    dplyr::select(itemId, posteriorProbability) %>% 
    rename(ContrastLoss = posteriorProbability)
  lda.model.posterior.predicted <- lda.model.posterior %>% 
    filter(posteriorClassLabel==ldaPredictedCategory) %>% 
    dplyr::select(itemId, posteriorProbability) %>% 
    rename(ldaProbPred = posteriorProbability)
  ldaResults <- lda.model.predicted %>% 
    left_join(lda.model.posterior.ref, by = "itemId") %>% 
    left_join(lda.model.posterior.predicted, by = "itemId")
  return(ldaResults)
}

# Read input data
# dataset <- read_tsv(fileNameIn, show_col_types = F) # If fileNameIn is a tsv file
dataset <- read_xlsx(fileNameIn, sheet = 1) # If fileNameIn is an xlsx file

# Add a temporary item id column (removed before exporting results)
dataset <- dataset %>% 
  mutate(tmpItemId = row_number())
# Convert formant values from Hz to Bark if requested
if(computeDistancesInBark) {
  dataset <- dataset %>% 
    mutate(across(all_of(formantsCols), hertzToBark))
}

# Compute centroids
# Vowel category-wise centroids
centroids_categ <- dataset %>% 
  group_by(across(all_of(c(groupColumns, vowelColumn)))) %>% 
  summarise(
    across(all_of(formantsCols), mean),
    nExemplars = n(),
    .groups = "drop"
  )
# Vowel space centroids (centroid of vowel category centroids)
centroids_global <- centroids_categ %>% 
  group_by(across(all_of(groupColumns))) %>% 
  summarise(
    across(all_of(formantsCols), mean),
    nVowelCategories = n(),
    .groups = "drop"
  )

# Convert dataframes from large to long format for data pairing and distances computation
centroids_categ_long <- centroids_categ %>% 
  dplyr::select(-nExemplars) %>% 
  pivot_longer(cols = all_of(formantsCols), names_to = "formantNumber", values_to = "vowelCategoryCentroid")
centroids_global_long <- centroids_global %>% 
  dplyr::select(-nVowelCategories) %>% 
  pivot_longer(cols = all_of(formantsCols), names_to = "formantNumber", values_to = "vowelSpaceCentroid")
dataset_long <- dataset %>% 
  pivot_longer(cols = all_of(formantsCols), names_to = "formantNumber", values_to = "formantValue") %>% 
  left_join(centroids_categ_long, by = c(groupColumns, vowelColumn, "formantNumber")) %>% 
  left_join(centroids_global_long, by = c(groupColumns, "formantNumber"))

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
              rename_with(~str_c("vowelCategMean", .), all_of(formantsCols)),
            by = c(groupColumns, vowelColumn)
  ) %>%
  left_join(
    centroids_global %>%
      rename_with(~str_c("vowelSpaceMean", .), all_of(formantsCols)),
    by = groupColumns
  )
}

# Get LDA results. The resulting data frame has the same number of rows as the original (large) data frame.
# Since LDA operates on all rows belonging to the same group and returns rowwise results, use nest_by to get a (temporary) distinct data frame for each group.
ldaResults <- dataset_long %>% 
  nest_by(across(all_of(groupColumns)), .key = "tmpDataCol") %>% 
  mutate(ldaResults = list(getLDApredictionsWithProbabilities(tmpDataCol %>% pull(tmpItemId), tmpDataCol %>% pull(all_of(vowelColumn)), tmpDataCol %>% pull(formantNumber), tmpDataCol %>% pull(formantValue)))) %>% 
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
  left_join(ldaResults %>% dplyr::select(-all_of(groupColumns), -classCol), by = c("tmpItemId" = "itemId"))

# Drop temporary item id col
dataset <- dataset %>% 
  dplyr::select(-tmpItemId)

# Export data
write_xlsx(
  list(data = dataset, categCentroids = centroids_categ, globalCentroids = centroids_global),
  path = fileNameOut,
  format_headers = F
)
