# Exemplar-wise vowel space metrics computation
R implementation made available as a Shiny app of the vowel-space metrics DistCentroid, VDispersion and ContrastLoss (Audibert et al., 2015).
Those vowel-space are presented in the following articles (please cite if you use this app):
- Audibert, N., Fougeron, C., Gendrot, C., & Adda-Decker, M. (2015). Duration-vs. style-dependent vowel variation: A multiparametric investigation. In 18th International Congress of Phonetic Sciences (ICPhS 2015), Glasgow, United Kingdom, paper 0753. (https://www.internationalphoneticassociation.org/icphs-proceedings/ICPhS2015/Papers/ICPHS0753.pdf)
- Hermes, A., Audibert, N., & Bourbon, A. (2023). Age-related vowel variation in French. In 20th International Congress of Phonetic Sciences (ICPhS 2023), Prague, Czech Republic. pp. 2045-2049. (https://guarant.cz/icphs2023/705.pdf)

An online version of the app can be found at https://shiny.laboratoirephonetiquephonologie.fr/vowel_space_metrics_computation/
Due to limited server capacity, the size of the data files processed with the online version cannot exceed 10MB. You can use the local version downloaded from this repository with R and RStudio on your computer to process larger files.

The R code of the non-interactive version used in the ICPhS 2023 paper (R script compute_exemplar_wise_vowel_space_metrics.R) is also hosted on this repository.
