# Support Ticket Analyzing - Bachelor Thesis

## Introduction

This project is a part of a bachelor thesis focused on analyzing support tickets to gain insights and improve customer service processes. The analysis involves preprocessing historical data, performing topic modeling, clustering, and generating various visualizations and reports.

## Project Structure

### Preprocessing Code Files

1. **`preproc_selection.Rmd`**: This R Markdown file select the important data
2. **`preproc_historical.Rmd`**: This R Markdown file preprocesses historical data, including filtering and transforming variables, and merging datasets.
3. **`preproc_topic_modeling.Rmd`**: This R Markdown file prepares the data for topic modeling, including text tokenization and the creation of a document-feature matrix.
4. **`preproc_clustering.Rmd`**: This R Markdown file preprocesses data for clustering analysis, including one-hot encoding and TF-IDF vectorization.
5. **`preproc_text_processing.ipynb`**: This Jupyter Notebook preprocesses vectorized text data using Python, including text cleaning, TF-IDF vectorization, and saving the processed data to CSV files.
6. **`preproc_clustering_alternatives.Rmd`**: This R Markdown file are alternatives preprocessing methods for clustering analysis, including one-hot encoding and TF-IDF vectorization. Finally they were not used.

### Analysis Code File

1. **`analysis.ipynb`**: This Jupyter Notebook performs the main analysis on the preprocessed data, including generating descriptive statistics, visualizations, and profiling reports for different clusters.
2. **`analysis_clusters.ipynb`**: This Jupyter Notebook performs the main analysis on the different clusters of the data, for a simple overview.

### Script for historical data

1. **`special_html_curler.ipynb`**: This Jupyter Notebook was used to get to the historical data info

### Data Privacy

Due to data privacy concerns, the data used in this project is not available in this repository.

## Conclusion

This project provides a comprehensive analysis of support tickets to help identify trends, improve response times, and enhance overall customer service efficiency. The preprocessing steps, clustering, and topic modeling contribute to a deeper understanding of the support tickets and the factors influencing their resolution.
