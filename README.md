# Project Documentation

This project contains the source code, data, and documentation related to the analysis and modeling of the Zentralbahn dataset.

## Project Structure

- **final_documentatoin.Rmd**  
  Contains all relevant source code written in R, combined with comprehensive documentation of the analysis and modeling process. This is the final consolidated document.

- **rnd_snippets/**  
  A folder with R Markdown snippets (`.Rmd` files) that were used during the exploratory phase to test and develop different models. All final versions of these analyses are integrated into `final_documentatoin.Rmd`.

- **screenshots/**  
  Contains screenshots used for documentation purposes.

- **Scripts/**  
  R scripts used to create the dataset and to generate initial models.

- **data/**  
  This folder contains:  
  - `zentralbahn_final.csv`: The main dataset used for the analysis.  
  - Raw data files.  
  - Intermediate CSV files generated during data processing.

## Note

The paths to `zentralbahn_final.csv` in the scripts and R Markdown snippets may not be up to date. Users need to verify and update the file paths accordingly before running any scripts.

Nevertheless, there should be no need to run the individual scripts or snippets, as all relevant and final code is contained within `final_documentatoin.Rmd`.
