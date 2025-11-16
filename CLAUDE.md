# CLAUDE.md - AI Assistant Guide for Caso_2 Repository

## Project Overview

**Project Name:** Caso_2 - Credit Risk Analysis and Loan Default Prediction
**License:** Apache License 2.0
**Language:** R (Statistical Computing)
**Project Type:** Academic/Research Data Science Project

### Purpose
This repository contains a comprehensive statistical analysis comparing supervised classification models (K-Nearest Neighbors and Logistic Regression) for predicting loan default risk using Lending Club loan data.

**Main Objective:** Evaluate and compare machine learning classification algorithms for credit risk assessment in personal loans.

### Team Members
- Aguirre Aldana Joan Sebastian (2419550)
- Barros Rayo Alejandro (2415837)
- Muñoz Portela Diego Fernando (2415620)
- Portilla Aguirre Johan Camilo (2422468)

---

## Repository Structure

```
Caso_2/
├── Data Files
│   ├── LC_loans_granting_model_dataset.csv    # Main dataset (Git LFS)
│   ├── desc_por_estado.csv                    # Descriptive stats by payment status
│   └── desc_var.csv                           # Variable descriptions
│
├── Analysis Scripts (R)
│   ├── Pretamos_final.R                       # Main analysis: KNN & Logit models
│   ├── Camilo_final.R                         # Extended analysis (34KB)
│   ├── DIEGOFINAL_.R                          # Additional model implementations
│   ├── diego.R / diegopruevas.R              # Exploratory scripts
│   ├── Pretamos_ALE.R                         # ALE (Accumulated Local Effects) analysis
│   ├── Periodos.R                             # Temporal analysis
│   ├── pruebas.R                              # Testing/experimentation
│   └── sirve.R                                # Utility functions
│
├── R Markdown Reports
│   ├── Rmarkdown_final.Rmd                    # Primary comprehensive report (117KB)
│   ├── Caso-2.Rmd                             # Alternative full report (155KB)
│   ├── DIEGOBB.Rmd                            # Specialized analysis section
│   ├── prueba.Rmd                             # Draft/testing report (208KB)
│   └── algo.Rmd                               # Experimental analysis
│
├── Generated Outputs
│   ├── Rmarkdown_final.html                   # Rendered HTML report
│   ├── Caso-2.html                            # Alternative HTML output
│   ├── DIEGOBB.html                           # Section HTML output
│   ├── animacion_fico.gif                     # FICO score animation
│   ├── animacion_ingreso.gif                  # Income distribution animation
│   └── grafico_umbral_logit_animado.gif       # Logit threshold animation
│
├── Styling & Configuration
│   ├── Sebastian-styles.css                   # Custom CSS for R Markdown
│   ├── Caso_2.Rproj                          # RStudio project configuration
│   └── LICENSE                                # Apache 2.0 license
│
└── RStudio/Publishing Artifacts
    ├── rsconnect/                             # RPubs publishing metadata
    ├── DIEGOBB_files/                         # Figure outputs
    └── Rmarkdown_final_files/                 # Figure outputs
```

---

## Codebase Architecture

### 1. Data Pipeline

**Source Dataset:** `LC_loans_granting_model_dataset.csv`
- Stored in Git LFS (Large File Storage) - 134MB
- Contains Lending Club loan information
- Key variables: revenue, dti_n, loan_amnt, fico_n, experience_c, emp_length, Default

**Data Preprocessing Workflow:**
```r
# Standard preprocessing pattern found across scripts
1. Load data from CSV
2. Select relevant variables
3. Transform emp_length (string → numeric)
4. Rename columns to Spanish equivalents
5. Convert Default to factor (Paga/No_paga)
6. Remove NA values
7. Create balanced sample (5000 per class)
8. Split into train/test (75/25)
```

### 2. Modeling Approach

**Two Primary Models:**

#### KNN (K-Nearest Neighbors)
- Implementation: `class::knn()` and `caret::train()`
- Cross-validation: 5-fold CV
- Hyperparameter tuning: k = 1 to 50
- Preprocessing: Center and scale
- Metric: ROC AUC

#### Logistic Regression (Logit)
- Implementation: `glm(family = binomial())`
- Threshold optimization: Youden index
- Probability predictions: type = "response"
- Performance metric: ROC AUC

### 3. Evaluation Framework

**Standard Evaluation Metrics:**
- Confusion Matrix (via `caret::confusionMatrix`)
- ROC Curve and AUC (via `pROC::roc`)
- Sensitivity and Specificity
- Optimal threshold determination

---

## Key Conventions for AI Assistants

### Naming Conventions

#### Variable Names (Spanish)
```r
# Standard variable mappings
revenue          → ingreso
dti_n            → relacion_deuda_ingreso
loan_amnt        → monto_prestamo
fico_n           → puntaje_fico
experience_c     → experiencia_lc
emp_length       → años_empleo
Default          → estado_pago
```

#### Target Variable Encoding
```r
estado_pago (factor):
  - "Paga"     = 0 (paid back the loan)
  - "No_paga"  = 1 (defaulted)
```

#### File Naming Patterns
- **Analysis scripts:** `[Name]_final.R` or `[Name].R`
- **Reports:** `[Name].Rmd` → generates `[Name].html`
- **Test files:** `prueba*.R` or `*pruevas.R`
- **Outputs:** `animacion_*.gif`, `desc_*.csv`

### Code Style Guidelines

#### 1. Package Loading
```r
# Standard packages used across project
library(readr)       # Data import
library(tidyverse)   # Data manipulation
library(class)       # KNN implementation
library(caret)       # ML framework
library(pROC)        # ROC analysis
library(ggthemes)    # Visualization themes
library(kableExtra)  # Table formatting
library(plotly)      # Interactive plots
library(gganimate)   # Animated visualizations
```

#### 2. Visualization Theme
**Custom theme pattern:**
```r
tema <- theme_minimal() +
  theme(
    text = element_text(family = "Segoe UI", color = "#2d3748"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.major = element_line(color = "#f3f2f1"),
    panel.grid.minor = element_blank(),
    # ... additional styling
  )
```

#### 3. Random Seed Convention
```r
set.seed(28)  # Consistently used across all scripts
```

#### 4. R Markdown Configuration
```yaml
output:
  html_document:
    theme: lumen / cayman
    toc: true
    toc_float: true
    code_folding: show
    number_sections: true
```

### Data Handling Patterns

#### Balanced Sampling Strategy
```r
# Create balanced dataset (equal class representation)
lending_muestra <- lending_base %>%
  group_by(estado_pago) %>%
  sample_n(5000, replace = FALSE) %>%
  ungroup()
```

#### Train/Test Split
```r
# 75/25 split with stratification
set.seed(28)
indice <- createDataPartition(
  y = lending_muestra$estado_pago,
  p = 0.75,
  list = FALSE
)
train <- lending_muestra[indice, ]
test  <- lending_muestra[-indice, ]
```

---

## Development Workflows

### For Analysis Modifications

**When modifying existing analyses:**

1. **Understand the model pipeline:**
   ```r
   Data Load → Preprocessing → Train/Test Split → Model Training → Evaluation
   ```

2. **Maintain consistency:**
   - Keep Spanish variable names
   - Use `set.seed(28)` before any random operations
   - Preserve the 75/25 train/test ratio
   - Use the same evaluation metrics (ROC AUC, Confusion Matrix)

3. **Follow existing patterns:**
   - Use `caret::train()` for model training with cross-validation
   - Apply `preProcess = c("center", "scale")` for KNN
   - Calculate optimal thresholds using Youden index for Logit

### For Visualization Tasks

**Creating new visualizations:**

1. **Use established themes:**
   - Apply the custom `tema` or `TS()` theme function
   - Color palette: primary colors are reds (#FF0000, #CC0000, #990000)
   - Font families: "Playfair Display" (titles), "Source Serif Pro" (body)

2. **For animated plots:**
   - Use `gganimate` package
   - Save as GIF: `animacion_[description].gif`
   - Typical dimensions: default ggplot2 sizing

3. **For HTML reports:**
   - Include interactive plots with `plotly`
   - Format tables with `kableExtra::kable()`
   - Use custom CSS from `Sebastian-styles.css` when applicable

### For R Markdown Reports

**When creating or modifying reports:**

1. **Chunk options:**
   ```r
   knitr::opts_chunk$set(
     echo = TRUE,
     warning = FALSE,
     message = FALSE,
     fig.align = "center",
     out.width = "85%"
   )
   ```

2. **Report structure:**
   - Title with team member attribution
   - Table of contents (floating)
   - Numbered sections
   - Code folding enabled
   - Professional academic tone (Spanish)

3. **Output formats:**
   - Primary: HTML (self-contained)
   - Themes: lumen, cayman, or prettydoc themes
   - Include custom CSS for advanced styling

---

## Git Workflow & Best Practices

### Branch Strategy

**Current setup:**
- **Main branch:** Stable, production-ready code
- **Feature branches:** Named `claude/claude-md-*` for AI-assisted development
- **Collaboration:** Multiple contributors working in parallel

**Important patterns from commit history:**
- Frequent merges from `main`
- Spanish commit messages common ("Hola", "FInal", "scarlet")
- Iterative development with many small commits

### Commit Guidelines

**When committing changes:**

1. **Test before committing:**
   - Ensure R scripts run without errors
   - Verify R Markdown knits successfully to HTML
   - Check that data paths are correct (relative, not absolute)

2. **Descriptive messages:**
   - Use Spanish or English consistently
   - Describe the analytical change, not just file names
   - Example: "Ajuste umbral óptimo en modelo logit" vs. "actualización"

3. **Branch management:**
   - Always push to designated feature branch
   - Format: `claude/claude-md-[session-id]`
   - Use `git push -u origin <branch-name>`

---

## Important File-Specific Notes

### Pretamos_final.R
**Primary analysis script** - Contains complete pipeline:
- Data loading and preprocessing
- KNN model (basic and caret implementations)
- Logistic regression with threshold optimization
- ROC curve generation
- **Use this as reference for standard workflow**

### Rmarkdown_final.Rmd / Caso-2.Rmd
**Comprehensive reports** - Very large files (117KB and 155KB):
- Full analysis documentation
- Inline code execution
- Custom CSS styling
- Professional academic formatting
- **These are the main deliverables**

### LC_loans_granting_model_dataset.csv
**Main dataset** - Git LFS tracked:
- Do NOT try to read directly in full
- Use `read_csv()` with proper path
- Dataset is ~134MB
- Always work with samples for development

---

## Common Tasks & Commands

### Running Analysis Scripts

```r
# In R console or RStudio
source("Pretamos_final.R")  # Runs complete KNN + Logit analysis

# For specific sections
source("Periodos.R")        # Temporal analysis
source("Pretamos_ALE.R")    # ALE plots
```

### Knitting R Markdown Reports

```r
# In R console
rmarkdown::render("Rmarkdown_final.Rmd")
rmarkdown::render("Caso-2.Rmd")

# In RStudio: Click "Knit" button
```

### Package Installation

```r
# Install all required packages
install.packages(c(
  "tidyverse", "readr", "dplyr", "tidyr",
  "class", "caret", "pROC",
  "ggplot2", "ggthemes", "ggcorrplot",
  "knitr", "kableExtra", "DT",
  "plotly", "gganimate",
  "prettydoc", "lubridate"
))
```

---

## Troubleshooting Guide

### Common Issues

#### 1. Dataset Not Found
**Error:** `cannot open file 'LC_loans_granting_model_dataset.csv'`

**Solution:**
```r
# Ensure working directory is project root
setwd("/path/to/Caso_2")

# Or use RStudio project (Caso_2.Rproj)
# which automatically sets correct working directory
```

#### 2. Git LFS Issues
**Error:** Dataset shows as pointer file

**Solution:**
```bash
# Install Git LFS
git lfs install

# Pull LFS objects
git lfs pull
```

#### 3. Missing Packages
**Error:** `there is no package called 'X'`

**Solution:**
```r
# Install missing package
install.packages("package-name")

# Or install all at once (see Package Installation section)
```

#### 4. R Markdown Knitting Fails
**Error:** Various knitting errors

**Solution:**
- Check all file paths are relative (not absolute)
- Ensure all required packages are installed
- Verify code chunks run independently in console
- Check for Spanish characters encoding (UTF-8)

---

## AI Assistant Specific Guidelines

### When Analyzing Code

1. **Language awareness:**
   - Comments may be in Spanish
   - Variable names are primarily Spanish
   - Documentation should mirror existing language choice

2. **Context understanding:**
   - This is an academic project (university assignment)
   - Multiple team members contributed different sections
   - Some redundancy/overlap is intentional (different approaches)

3. **File relationships:**
   - Multiple versions of similar analyses exist (`diego.R`, `DIEGOFINAL_.R`)
   - Different R Markdown files may contain overlapping content
   - Final versions typically have `_final` or `FINAL` in filename

### When Making Modifications

1. **Preserve academic integrity:**
   - Maintain attribution to original authors
   - Don't remove comments or documentation
   - Keep team member names in R Markdown headers

2. **Statistical rigor:**
   - Don't modify `set.seed(28)` - ensures reproducibility
   - Maintain cross-validation approach
   - Keep balanced sampling strategy unless explicitly changing methodology

3. **Visualization consistency:**
   - Use established color schemes (reds for primary)
   - Maintain Spanish labels on plots
   - Apply custom themes for consistency

### When Adding New Features

1. **Follow existing patterns:**
   - Create new `.R` file or add to existing
   - Use Spanish variable names
   - Include comments explaining methodology

2. **Documentation:**
   - Add code chunks to appropriate R Markdown file
   - Include interpretation of results in Spanish
   - Generate visualizations following existing style

3. **Testing:**
   - Verify code runs with `set.seed(28)`
   - Check output formats (HTML, GIF, CSV)
   - Ensure compatibility with existing workflow

---

## Quick Reference

### Essential Variables

| English | Spanish | Description |
|---------|---------|-------------|
| revenue | ingreso | Annual income |
| dti_n | relacion_deuda_ingreso | Debt-to-income ratio |
| loan_amnt | monto_prestamo | Loan amount |
| fico_n | puntaje_fico | FICO credit score |
| experience_c | experiencia_lc | Lending Club experience |
| emp_length | años_empleo | Years employed |
| Default | estado_pago | Payment status (Paga/No_paga) |

### Key Hyperparameters

| Model | Parameter | Value/Range |
|-------|-----------|-------------|
| KNN | k | 1-50 (tuned via CV) |
| KNN | CV folds | 5 |
| KNN | Metric | ROC AUC |
| Logit | Threshold | Youden optimal |
| All | Train/Test | 75% / 25% |
| All | Sample size | 10,000 (5k per class) |
| All | Random seed | 28 |

### Critical Paths

```
Data:     LC_loans_granting_model_dataset.csv
Main R:   Pretamos_final.R
Main Rmd: Rmarkdown_final.Rmd, Caso-2.Rmd
Outputs:  *.html, animacion_*.gif
Config:   Caso_2.Rproj
```

---

## Version Information

**R Project Settings:**
- RStudio Project Version: 1.0
- Encoding: UTF-8
- Spaces for tabs: Yes (2 spaces)
- Code indexing: Enabled

**Document Rendering:**
- R Markdown: Yes
- Output: HTML (primary)
- Weave: Sweave
- LaTeX: pdfLaTeX

---

## Additional Resources

### External Dependencies
- **Git LFS:** Required for dataset access
- **RStudio:** Recommended IDE (uses `.Rproj` file)
- **RPubs:** Some reports published online (see `rsconnect/`)

### Learning Resources
For understanding the methodology:
- KNN: Study `Pretamos_final.R` lines 83-124
- Logit: Study `Pretamos_final.R` lines 176-229
- Visualization: Examine custom theme definitions
- R Markdown: Review YAML headers in `.Rmd` files

---

## Contact & Maintenance

This is an academic project repository. For questions about:
- **Methodology:** Refer to R Markdown reports
- **Code issues:** Check similar patterns in other `.R` files
- **Data questions:** See preprocessing in `Pretamos_final.R`

**Last Updated:** 2025-11-16
**AI Assistant Guide Version:** 1.0.0

---

*This CLAUDE.md file is designed to help AI assistants understand and work effectively with the Caso_2 repository. It should be updated as the project evolves.*
