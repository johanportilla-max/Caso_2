# CLAUDE.md - AI Assistant Guide for Caso_2 Repository

## Project Overview

**Project Name**: Caso_2 - Credit Risk Evaluation with Machine Learning
**Project Type**: R/RStudio Data Science Project
**Primary Focus**: Application of supervised machine learning algorithms (K-Nearest Neighbors and Logistic Regression) for credit risk assessment using Lending Club loan data
**License**: Apache License 2.0

### Team Members
- Joan Sebastian Aguirre Aldana (2419550)
- Alejandro Barros Rayo (2415837)
- Diego Fernando Muñoz Portela (2415620)
- Johan Camilo Portilla Aguirre (2422468)

## Repository Structure

```
Caso_2/
├── .git/                          # Git repository metadata
├── .gitignore                     # R-specific files to ignore
├── .gitattributes                 # Git LFS configuration
├── Caso_2.Rproj                   # RStudio project configuration
├── LICENSE                        # Apache 2.0 license
│
├── Data Files
│   ├── LC_loans_granting_model_dataset.csv  # Main dataset (Git LFS, 145MB)
│   ├── desc_por_estado.csv        # Descriptive statistics by status
│   └── desc_var.csv               # Variable descriptions
│
├── R Scripts (Individual Work)
│   ├── Camilo_final.R             # Camilo's analysis code
│   ├── DIEGOFINAL_.R              # Diego's analysis code
│   ├── diego.R                    # Diego's exploratory work
│   ├── Pretamos_final.R           # Final loan analysis
│   ├── Pretamos_ALE.R             # Alejandro's loan analysis
│   ├── Periodos.R                 # Time period analysis
│   ├── pruebas.R                  # Testing/experimentation
│   └── sirve.R                    # Utility script
│
├── R Markdown Reports
│   ├── Caso-2.Rmd                 # Main comprehensive report
│   ├── Rmarkdown_final.Rmd        # Final polished report
│   ├── Caso 2.Rmd                 # Alternative report version
│   ├── DIEGOBB.Rmd                # Diego's report
│   ├── .jj.Rmd                    # Work-in-progress
│   └── prueba.Rmd                 # Test/experimental report
│
├── Generated Outputs
│   ├── Caso-2.html                # Main HTML report (19.8MB)
│   ├── Rmarkdown_final.html       # Final HTML report (6.2MB)
│   ├── DIEGOBB.html               # Diego's HTML report (6MB)
│   ├── .jj.html                   # Work-in-progress HTML
│   ├── animacion_fico.gif         # FICO score animation (334KB)
│   ├── animacion_ingreso.gif      # Income animation (310KB)
│   └── grafico_umbral_logit_animado.gif  # Logistic threshold animation
│
├── Supporting Files
│   ├── Sebastian-styles.css       # Custom CSS styling for reports
│   ├── DIEGOBB_files/             # Supporting files for Diego's report
│   ├── Rmarkdown_final_files/     # Supporting files for final report
│   └── rsconnect/                 # RStudio Connect deployment files
│
└── Temporary Files (Gitignored)
    ├── .RDataTmp, .RDataTmp1      # Temporary R data files
    └── .Rproj.user/               # RStudio user settings
```

## Technology Stack

### Core Languages & Tools
- **R** (Primary language for statistical analysis)
- **R Markdown** (For reproducible reports)
- **RStudio** (IDE - see Caso_2.Rproj configuration)
- **Git LFS** (For large dataset management)

### Essential R Libraries

#### Data Manipulation & Processing
```r
library(dplyr)          # Data transformation
library(tidyr)          # Data tidying
library(readr)          # CSV reading
library(tidyverse)      # Comprehensive data science toolkit
library(lubridate)      # Date/time handling
library(scales)         # Scale functions for visualization
```

#### Machine Learning & Statistical Modeling
```r
library(class)          # K-Nearest Neighbors
library(caret)          # Machine learning framework
library(pROC)           # ROC curve analysis
library(broom)          # Model output tidying
```

#### Visualization
```r
library(ggplot2)        # Core plotting (via tidyverse)
library(ggthemes)       # Additional themes
library(ggcorrplot)     # Correlation matrices
library(gridExtra)      # Multiple plot arrangement
library(grid)           # Low-level graphics
library(ggplotify)      # Convert plots to grobs
library(plotly)         # Interactive plots
library(gganimate)      # Animated visualizations
```

#### Reporting & Tables
```r
library(knitr)          # Dynamic report generation
library(kableExtra)     # Enhanced tables
library(DT)             # Interactive DataTables
library(htmltools)      # HTML generation
```

## Data Overview

### Primary Dataset
**File**: `LC_loans_granting_model_dataset.csv` (stored in Git LFS)
- **Size**: 145,282,794 bytes (~145 MB)
- **Source**: Lending Club loan granting data
- **Format**: CSV with large row count (requires `guess_max = 20000` for proper type inference)

### Key Variables (Spanish naming convention)
```r
# Original → Spanish renaming
revenue         → ingreso                    # Income
dti_n           → relacion_deuda_ingreso    # Debt-to-income ratio
loan_amnt       → monto_prestamo            # Loan amount
fico_n          → puntaje_fico              # FICO score
Default         → estado_pago               # Payment status (0=Paga, 1=No_paga)
purpose         → proposito                 # Loan purpose
issue_d         → fecha_emision             # Issue date
```

### Data Preprocessing Pipeline
1. **Variable Selection & Renaming**: Spanish names for clarity
2. **Date Parsing**: `parse_date_time(issue_d, orders = "b-Y", locale = "en_US")`
3. **Categorical Grouping**:
   - `proposito_agrupado`: Groups loan purposes into 4 categories
     - Consolidacion: debt_consolidation, credit_card
     - Casa_Vehiculo: home_improvement, major_purchase, car, house
     - Negocio_Estudio: small_business, educational
     - Otros: Everything else
4. **Filtering**:
   - Income (`ingreso`) <= 250,000
   - Debt-to-income ratio (`relacion_deuda_ingreso`) <= 50
5. **Missing Data**: `drop_na()` removes incomplete records
6. **Balanced Sampling**: 5,000 "Paga" + 5,000 "No_paga" (seed = 28)
7. **Train/Test Split**: 75% training, 25% testing (seed = 28)

## Machine Learning Workflow

### Model Types
1. **K-Nearest Neighbors (KNN)**
   - Uses `class::knn()` function
   - Hyperparameter tuning: k from 1 to 100
   - Requires feature scaling (center + scale)

2. **Logistic Regression**
   - Binary classification for default prediction
   - Threshold optimization for classification

### Standard Workflow
```r
# 1. Data Preparation
vars_input <- c("ingreso", "relacion_deuda_ingreso", "monto_prestamo", "puntaje_fico")
train_input <- train[, vars_input]
test_input  <- test[, vars_input]
train_output <- train$estado_pago
test_output  <- test$estado_pago

# 2. Scaling (KNN requires normalized features)
scaler <- preProcess(train_input, method = c("center", "scale"))
train_input_scaled <- predict(scaler, train_input)
test_input_scaled  <- predict(scaler, test_input)

# 3. Model Training & Evaluation
# KNN example with hyperparameter search
k_vals <- 1:100
for (n in k_vals) {
  # Train and evaluate model
}

# 4. Performance Metrics
# - Accuracy, Precision, Recall
# - ROC curves (pROC package)
# - Confusion matrices
```

### Reproducibility
- **Random Seed**: Always `set.seed(28)` for reproducible results
- **Balanced Sampling**: Equal representation of both classes

## Visualization Standards

### Custom Theme
Files use a custom ggplot2 theme (`TS()`) defined in R Markdown chunks:
- **Font Families**:
  - Titles: "Playfair Display"
  - Text: "Source Serif Pro"
- **Color Palette**:
  - Primary: #FF0000 (red) for titles
  - Secondary: #CC0000 (dark red) for subtitles
  - Accent: #990000 (crimson) for axis titles
  - Text: #2c2c2c, #333333, #666666 (grays)

### Generated Visualizations
1. **Static Plots**: ggplot2 with custom themes
2. **Animated GIFs**:
   - `animacion_fico.gif`: FICO score distributions over time
   - `animacion_ingreso.gif`: Income distributions over time
   - `grafico_umbral_logit_animado.gif`: Logistic regression threshold animation
3. **Interactive Plots**: plotly for exploratory analysis

### Output Specifications
R Markdown outputs configured as:
```yaml
output:
  prettydoc::html_pretty:
    theme: cayman
    highlight: tango
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 8
    fig_height: 5
    self_contained: true
    css: Sebastian-styles.css
```

## Development Workflow

### RStudio Configuration (Caso_2.Rproj)
- **Encoding**: UTF-8
- **Tab Spacing**: 2 spaces (not tabs)
- **Code Indexing**: Enabled
- **R Markdown**: Sweave for knitting, pdfLaTeX for PDF generation

### Git Workflow
- **Version Control**: Active Git repository
- **LFS**: Used for large CSV dataset
- **Commit Style**: Mix of English and Spanish messages
- **Collaboration**: Multiple contributors with individual files

### File Organization Patterns
1. **Individual Work**: Contributors have separate R/Rmd files (e.g., `Camilo_final.R`, `DIEGOBB.Rmd`)
2. **Integration**: Combined work in `Caso-2.Rmd` and `Rmarkdown_final.Rmd`
3. **Experimentation**: Files like `prueba.Rmd`, `pruebas.R` for testing
4. **Outputs**: HTML files and GIFs generated from R Markdown

### Gitignore Strategy
Standard R exclusions:
```
.Rproj.user
.Rhistory
.RData
.Ruserdata
```

## Key Conventions for AI Assistants

### 1. Language Conventions
- **Code Comments**: Mix of Spanish and English
- **Variable Names**: **Spanish** (e.g., `ingreso`, `relacion_deuda_ingreso`, `estado_pago`)
- **Function Names**: English (standard R conventions)
- **Documentation**: Primarily Spanish (academic context)

### 2. Data Analysis Conventions
- **Always set seed**: Use `set.seed(28)` for reproducibility
- **Balanced sampling**: Ensure equal representation of classes
- **Feature scaling**: Required for KNN, use `caret::preProcess()`
- **Data validation**: Check distributions before and after filtering

### 3. Code Style
- **Tidyverse preference**: Use `%>%` pipe operator extensively
- **Indentation**: 2 spaces (RStudio default)
- **Assignment**: Use `<-` not `=` for assignment
- **Naming**: snake_case for variables and functions

### 4. R Markdown Best Practices
```r
# Standard chunk options
knitr::opts_chunk$set(
  echo = TRUE,          # Show code
  warning = FALSE,      # Suppress warnings
  message = FALSE,      # Suppress messages
  fig.align = "center", # Center figures
  out.width = "85%",    # Figure width
  comment = ""          # No comment prefix
)
```

### 5. Model Development Guidelines
- **Train/Test Split**: Use 75/25 ratio
- **Cross-validation**: Consider for hyperparameter tuning
- **Performance Metrics**: Calculate accuracy, precision, recall, ROC-AUC
- **Threshold Optimization**: For logistic regression classification
- **Feature Selection**: Focus on 4 core features (income, DTI, loan amount, FICO)

### 6. Visualization Guidelines
- **Use custom theme**: Apply `TS()` for consistency
- **Informative titles**: Spanish titles with clear context
- **Color consistency**: Follow established palette
- **Save animations**: Export as GIF for reports
- **Interactive elements**: Use plotly for exploratory analysis

### 7. Working with Large Data
- **Git LFS**: Dataset is stored in LFS, use `git lfs pull` to download
- **Efficient reading**: Use `guess_max = 20000` with `read_csv()`
- **Memory management**: Sample data when appropriate (e.g., 10,000 total records)
- **Caching**: R Markdown caching for expensive computations

### 8. Report Generation
- **Knit regularly**: Test R Markdown output frequently
- **Self-contained HTML**: Keep `self_contained: true` for portability
- **CSS customization**: Use `Sebastian-styles.css` for styling
- **Figure management**: Organize in `*_files/` directories
- **TOC structure**: Use hierarchical sections with `toc_depth: 4`

### 9. Collaboration Patterns
- **Individual exploration**: Work in personal files first
- **Integration**: Merge findings into main report files
- **Clear attribution**: Maintain author information in YAML headers
- **Version control**: Commit logical units of work with descriptive messages

### 10. Common Pitfalls to Avoid
- ❌ Don't mix English/Spanish variable names inconsistently
- ❌ Don't forget to set random seed before sampling/splitting
- ❌ Don't skip feature scaling for KNN models
- ❌ Don't commit large data files without Git LFS
- ❌ Don't modify `.gitignore` to include R workspace files
- ❌ Don't use absolute file paths (breaks reproducibility)
- ❌ Don't forget to load required libraries in each script
- ❌ Don't neglect data validation after filtering

## Useful Commands

### RStudio/R Environment
```r
# Install required packages
install.packages(c("tidyverse", "class", "caret", "pROC", "ggthemes",
                   "lubridate", "kableExtra", "knitr", "ggcorrplot",
                   "gridExtra", "plotly", "gganimate", "DT", "prettydoc"))

# Load project
rstudioapi::openProject("Caso_2.Rproj")

# Knit R Markdown
rmarkdown::render("Caso-2.Rmd")
rmarkdown::render("Rmarkdown_final.Rmd")
```

### Git Operations
```bash
# Pull dataset from LFS
git lfs pull

# Check LFS status
git lfs ls-files

# Standard git workflow
git status
git add .
git commit -m "message"
git push -u origin branch-name
```

### Project Setup
```bash
# Clone repository
git clone <repository-url>
cd Caso_2

# Pull LFS files
git lfs install
git lfs pull

# Open in RStudio
open Caso_2.Rproj
```

## Project Goals & Context

This project aims to:
1. **Apply supervised learning** techniques to real-world credit risk data
2. **Compare algorithms**: KNN vs. Logistic Regression performance
3. **Optimize thresholds**: Find optimal classification cutoffs
4. **Visualize insights**: Create informative, publication-ready visualizations
5. **Generate reports**: Produce comprehensive HTML reports with analysis

**Academic Context**: This appears to be a university data science project (case study 2) focusing on practical application of machine learning methods to financial data.

## Support & Troubleshooting

### Common Issues
1. **Dataset not loading**: Ensure Git LFS is installed and `git lfs pull` has been run
2. **Package errors**: Install missing packages with `install.packages()`
3. **Encoding issues**: Verify UTF-8 encoding in RStudio settings
4. **Knitting errors**: Check that all chunk dependencies are satisfied
5. **Memory errors**: Reduce sample size or use data.table for efficiency

### Environment Information
- **Working Directory**: `/home/user/Caso_2`
- **Git Status**: Active repository on branch with claude/ prefix
- **Platform**: Linux 4.4.0

## AI Assistant Recommendations

When working with this repository:

1. **Understand the context**: This is a collaborative academic project with Spanish documentation
2. **Respect naming conventions**: Keep Spanish variable names, add English comments for clarity
3. **Maintain reproducibility**: Always use seed 28, document preprocessing steps
4. **Test incrementally**: Run code chunks individually before full knitting
5. **Preserve formatting**: Keep the established ggplot2 theme and CSS styling
6. **Version control**: Commit logical changes with clear messages
7. **Collaborate effectively**: Don't overwrite individual contributor files without discussion
8. **Document changes**: Update this CLAUDE.md when adding new conventions or structures

## Last Updated
Generated: 2025-11-17
Repository Branch: `claude/claude-md-mi2izi47v1i8a84x-017rQUTyxfvqfyucB99EWj3T`
