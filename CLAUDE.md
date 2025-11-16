# CLAUDE.md - AI Assistant Guide

## Project Overview

**Project Name**: Caso_2 - Credit Risk Classification Analysis
**Project Type**: R Statistical Analysis & Research Project
**License**: Apache License 2.0

### Purpose

This repository contains a comprehensive statistical analysis comparing K-Nearest Neighbors (KNN) and Logistic Regression models for predicting default risk in personal loans. The study uses the Lending Club dataset (2007-2018) to evaluate which supervised classification model performs better at identifying loan defaults.

### Authors

- Barros Rayo Alejandro (2415837)
- Muñoz Portela Diego Fernando (2415620)
- Portilla Aguirre Johan Camilo (2422468)
- Aguirre Aldana Joan Sebastian (2419550)

---

## Repository Structure

```
Caso_2/
├── .git/                           # Git version control
├── .gitignore                      # R project gitignore (RData, Rhistory, etc.)
├── .RDataTmp, .RDataTmp1          # Temporary R workspace files
├── Caso_2.Rproj                   # RStudio project file
├── LICENSE                         # Apache 2.0 license
│
├── Data/
│   ├── LC_loans_granting_model_dataset.csv  # Main dataset (Git LFS)
│   ├── desc_var.csv               # Variable descriptions
│   └── desc_por_estado.csv        # Descriptive stats by payment status
│
├── R Scripts/
│   ├── Camilo_final.R             # Main analysis script (999 lines)
│   ├── DIEGOFINAL_.R              # Analysis implementation
│   ├── Pretamos_final.R           # Loan analysis script
│   ├── Pretamos_ALE.R             # ALE plots implementation
│   ├── Periodos.R                 # Time period analysis
│   ├── diegopruevas.R, diego.R    # Development/testing scripts
│   ├── pruebas.R, sirve.R         # Experimental scripts
│   └── algo.Rmd                   # Exploratory R Markdown
│
├── Reports/ (R Markdown)
│   ├── Rmarkdown_final.Rmd        # Final comprehensive report (primary)
│   ├── Rmarkdown_final.html       # Generated HTML report
│   ├── Caso-2.Rmd                 # Alternative report version
│   ├── Caso-2.html                # Generated HTML
│   ├── DIEGOBB.Rmd                # Individual contributor report
│   ├── DIEGOBB.html               # Generated HTML
│   ├── Caso 2.Rmd                 # Original draft report
│   └── prueba.Rmd                 # Test R Markdown
│
├── Visualizations/
│   ├── animacion_fico.gif         # FICO score animation
│   ├── animacion_ingreso.gif      # Income distribution animation
│   ├── grafico_umbral_logit_animado.gif  # Threshold animation
│   └── Sebastian-styles.css       # Custom CSS styling
│
├── Generated Assets/
│   ├── Rmarkdown_final_files/     # Supporting files for final report
│   ├── DIEGOBB_files/             # Supporting files for Diego's report
│   └── rsconnect/                 # RStudio Connect/RPubs deployment
│
└── CLAUDE.md                       # This file
```

---

## Technology Stack

### Core Technologies

- **Language**: R (statistical computing)
- **IDE**: RStudio
- **Version Control**: Git with Git LFS (for large datasets)
- **Documentation**: R Markdown (.Rmd) → HTML reports

### R Package Dependencies

#### Data Manipulation
- `dplyr` - Data wrangling and transformation
- `tidyr` - Data tidying and reshaping
- `readr` - Fast CSV reading
- `tidyverse` - Meta-package (includes dplyr, ggplot2, tidyr, etc.)

#### Machine Learning
- `class` - KNN classification
- `caret` - Unified ML interface, cross-validation, tuning
- `pROC` - ROC curve analysis and AUC calculation

#### Visualization
- `ggplot2` - Grammar of graphics (via tidyverse)
- `ggthemes` - Extended ggplot2 themes
- `ggcorrplot` - Correlation matrix visualization
- `scales` - Scale functions for visualizations
- `gridExtra` - Arranging multiple plots
- `grid` - Low-level graphics
- `ggplotify` - Convert plots to grobs
- `fmsb` - Radar chart visualization

#### Time Series
- `lubridate` - Date/time manipulation

#### Tables & Reporting
- `kableExtra` - Enhanced table formatting
- `knitr` - Dynamic report generation
- `broom` - Tidy model outputs

---

## Dataset Information

### Primary Dataset

**File**: `LC_loans_granting_model_dataset.csv`
**Storage**: Git LFS (Large File Storage)
**Size**: ~138 MB (145,282,794 bytes)
**Source**: Lending Club loan granting dataset (2007-2018)

### Key Variables

The analysis focuses on these variables:

| Original Name | Renamed To | Description |
|--------------|------------|-------------|
| `revenue` | `ingreso` | Borrower's annual income |
| `dti_n` | `relacion_deuda_ingreso` | Debt-to-income ratio |
| `loan_amnt` | `monto_prestamo` | Loan amount requested |
| `fico_n` | `puntaje_fico` | FICO credit score |
| `Default` | `estado_pago` | Payment status (0=Paga, 1=No_paga) |
| `purpose` | `proposito_agrupado` | Loan purpose (categorized) |
| `issue_d` | `fecha_emision` | Loan issue date |

### Data Preprocessing

The analysis applies these preprocessing steps:

1. **Filtering**:
   - Income <= $250,000
   - DTI <= 50%

2. **Balanced Sampling**:
   - Random sample: 5,000 "Paga" + 5,000 "No_paga" = 10,000 observations
   - Seed: 28 (for reproducibility)

3. **Train/Test Split**:
   - Training: 75% (7,500 observations)
   - Testing: 25% (2,500 observations)
   - Random seed: 28

4. **Purpose Categorization**:
   - `Consolidacion`: debt_consolidation, credit_card
   - `Casa_Vehiculo`: home_improvement, major_purchase, car, house
   - `Negocio_Estudio`: small_business, educational
   - `Otros`: all other purposes

---

## Analysis Methodology

### Models Implemented

#### 1. K-Nearest Neighbors (KNN)

**Two Implementations**:

a. **Base Implementation** (`class` package):
   - Manual k optimization (k = 1 to 100)
   - Standardization using `caret::preProcess()`
   - Performance metric: Accuracy

b. **Advanced Implementation** (`caret` package):
   - 5-fold cross-validation
   - Hyperparameter tuning (k = 1 to 150)
   - Performance metric: ROC-AUC
   - Includes categorical variable (`proposito_agrupado`)
   - Automatic centering and scaling

#### 2. Logistic Regression (Logit)

- **Family**: Binomial
- **Link function**: Logit
- **Features**: All continuous + categorical predictors
- **Threshold optimization**: Youden's J statistic
- **Evaluation**: ROC-AUC, confusion matrix, multiple metrics

### Model Evaluation Metrics

Both models are evaluated using:

- **Accuracy**: Overall correct classification rate
- **Sensitivity (Recall)**: True positive rate (detecting defaults)
- **Specificity**: True negative rate (detecting good payers)
- **Precision (PPV)**: Positive predictive value
- **Balanced Accuracy**: Average of sensitivity and specificity
- **AUC**: Area under ROC curve
- **Kappa**: Agreement beyond chance
- **Confusion Matrix**: Classification breakdown

---

## Key Findings

### Model Performance Comparison

| Metric | KNN (caret) | Logit |
|--------|-------------|-------|
| Accuracy | 0.597 | 0.612 |
| Sensitivity | 0.659 | 0.644 |
| Specificity | 0.537 | 0.582 |
| Balanced Accuracy | 0.598 | 0.613 |
| AUC | 0.645 | 0.646 |

**Conclusion**: Logistic Regression slightly outperforms KNN in overall accuracy and balanced performance, though both models show similar predictive power (AUC ≈ 0.645).

---

## Development Workflow

### For AI Assistants Working on This Project

#### Setting Up the Environment

```r
# Install required packages
install.packages(c(
  "dplyr", "tidyr", "readr", "tidyverse",
  "class", "caret", "pROC",
  "ggthemes", "lubridate", "kableExtra", "knitr",
  "ggcorrplot", "scales", "gridExtra", "grid",
  "ggplotify", "broom", "fmsb"
))

# Open RStudio project
# File > Open Project > Caso_2.Rproj
```

#### Primary Files to Modify

1. **Main Analysis**: `Camilo_final.R` - Contains complete analysis pipeline
2. **Primary Report**: `Rmarkdown_final.Rmd` - Final comprehensive report
3. **Styling**: `Sebastian-styles.css` - Custom CSS for HTML reports

#### Rendering Reports

```r
# Render R Markdown to HTML
rmarkdown::render("Rmarkdown_final.Rmd")

# Or use RStudio: Click "Knit" button
```

#### Git Workflow

This project uses **Git LFS** for large files. When working with the dataset:

```bash
# Ensure Git LFS is installed
git lfs install

# Pull LFS objects
git lfs pull

# Check LFS files
git lfs ls-files

# Current branch pattern for Claude
git checkout claude/claude-md-mi0zq0uybrduw3ix-01Xv8ASbQANT5qNCbnBQhSzv
```

---

## Coding Conventions

### R Style Guidelines

1. **Naming Conventions**:
   - Variables: `snake_case` (e.g., `puntaje_fico`, `estado_pago`)
   - Functions: `camelCase` (e.g., `performa()`)
   - Constants: Spanish language for domain variables

2. **Data Pipeline**:
   - Use `%>%` (pipe operator) for data transformations
   - Prefer `dplyr` verbs: `select()`, `filter()`, `mutate()`, `summarise()`

3. **Reproducibility**:
   - **ALWAYS set seeds**: `set.seed(28)` before random operations
   - Document all filtering criteria
   - Keep preprocessing steps explicit and commented

4. **Visualization**:
   - Use consistent theme (`tema` object in scripts)
   - Color palette: Primary `#2b6cb0`, Accent `#1a5276`, Warning `#c0392b`
   - Always include titles, subtitles, and captions with data source

5. **Tables**:
   - Use `kableExtra` for publication-quality tables
   - Include captions and footnotes
   - Consistent formatting: striped, hover effects

### R Markdown Conventions

1. **YAML Header**:
   - Use `html_document` output
   - Enable table of contents (`toc: true`)
   - Use code folding for cleaner presentation

2. **Chunk Options**:
   - Name all chunks descriptively
   - Set appropriate `echo`, `warning`, `message` options

3. **Inline CSS**:
   - Custom styles embedded in `<style>` tags
   - Responsive design with max-width constraints
   - Professional color scheme (see CSS variables in reports)

---

## Common Tasks for AI Assistants

### Adding a New Analysis

1. Create new R script in root directory
2. Follow naming convention: `[Author]_[description].R`
3. Import standard libraries (see `Camilo_final.R` header)
4. Use same preprocessing pipeline for consistency
5. Set seed to 28 for reproducibility

### Modifying Existing Models

1. **Locate model code**:
   - KNN (class): Lines 90-117 in `Camilo_final.R`
   - KNN (caret): Lines 120-146
   - Logit: Lines 148-172

2. **Key parameters**:
   - KNN k-range: Modify `k_vals` or `tuneLength`
   - CV folds: Change `number` in `trainControl()`
   - Train/test split: Adjust in lines 74-81

3. **After modifications**:
   - Re-run confusion matrix generation
   - Update ROC curve plots
   - Regenerate comparison tables

### Creating Visualizations

All visualizations should:
- Use the `tema` theme object (lines 204-217 in `Camilo_final.R`)
- Include proper labels in Spanish
- Add source attribution: "Fuente: Elaboración propia con base en el dataset Lending Club (2007–2018)"
- Export as high-DPI images for reports

### Updating the Final Report

1. Edit `Rmarkdown_final.Rmd`
2. Maintain section structure:
   - Introduction & Methodology
   - Data Exploration
   - Model Training
   - Results Comparison
   - Conclusions
3. Knit to HTML to verify formatting
4. Check that all plots render correctly
5. Validate table formatting

---

## File Naming Patterns

### Patterns to Recognize

- `*_final.R` - Finalized analysis scripts
- `*_ALE.R` - Accumulated Local Effects analysis
- `*.Rmd` - R Markdown documents (source)
- `*.html` - Generated reports (DO NOT edit directly)
- `*_files/` - Supporting assets for HTML reports (auto-generated)
- `animacion_*.gif` - Animated visualizations
- `desc_*.csv` - Descriptive statistics exports
- `.RDataTmp*` - Temporary workspace (ignore/gitignore)

### Files to Ignore

- `.RDataTmp`, `.RDataTmp1` - Temporary R workspaces
- `.Rproj.user/` - RStudio internal files
- `.Rhistory` - Command history
- `*_files/` - Auto-generated HTML dependencies
- `rsconnect/` - Deployment configuration

---

## Testing & Validation

### Model Validation Checklist

When modifying models, verify:

- [ ] Confusion matrix totals equal test set size (2,500)
- [ ] AUC values are between 0.5 and 1.0
- [ ] Accuracy > No Information Rate
- [ ] Sensitivity + Specificity values are reasonable (0.5-0.7 range)
- [ ] Balanced Accuracy is the mean of Sensitivity and Specificity
- [ ] ROC curves are smooth and above diagonal
- [ ] All tables render without errors in HTML

### Reproducibility Check

```r
# Verify reproducibility
set.seed(28)
# Run analysis
result1 <- your_analysis()

set.seed(28)
# Run again
result2 <- your_analysis()

# Should be identical
identical(result1, result2)
```

---

## Output Expectations

### Generated Reports

Reports should include:

1. **Executive Summary**: Model comparison table
2. **Data Section**: Distribution plots, descriptive statistics
3. **Methodology**: Model specifications, hyperparameters
4. **Results**:
   - Confusion matrices for both models
   - ROC curves with AUC values
   - Performance metrics tables
   - Comparative visualizations (radar chart)
5. **Conclusions**: Model recommendations

### Visualization Requirements

All plots must have:
- Clear title (bold, centered)
- Subtitle with context
- Axis labels (Spanish, bold)
- Caption with data source
- Consistent color scheme
- Professional theme (minimal, clean)

---

## Troubleshooting

### Common Issues

#### Issue: Dataset not loading
```r
Error: 'LC_loans_granting_model_dataset.csv' not found
```
**Solution**: Ensure Git LFS is installed and run `git lfs pull`

#### Issue: Package not found
```r
Error: there is no package called 'X'
```
**Solution**: Install missing package with `install.packages("X")`

#### Issue: Memory errors with large dataset
```r
Error: cannot allocate vector of size X
```
**Solution**: Increase R memory limit or work with sample data

#### Issue: HTML report not rendering properly
**Solution**:
- Check that all code chunks execute without errors
- Verify CSS is properly closed (matching braces)
- Clear knitr cache: `knitr::clean_cache()`

#### Issue: Plots not displaying in report
**Solution**:
- Ensure chunk option `echo=TRUE` or `fig.show='asis'`
- Check plot object is explicitly printed
- Verify figure dimensions are reasonable

---

## Performance Considerations

### Dataset Size Management

- **Original dataset**: ~145 MB via Git LFS
- **Working subset**: 10,000 observations (balanced sample)
- **Memory usage**: Peak ~500 MB during analysis

### Optimization Tips

1. **For faster iterations**: Use smaller sample sizes during development
2. **For production runs**: Use full 10,000 sample with seed=28
3. **Parallel processing**: `caret` supports parallel backend with `doParallel`

```r
# Optional: Enable parallel processing for caret
library(doParallel)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Run caret::train()
stopCluster(cl)
```

---

## Publication & Deployment

### RPubs Deployment

The project uses RPubs for report publishing:

1. Render R Markdown document
2. Click "Publish" button in RStudio
3. Select RPubs destination
4. Configuration stored in `rsconnect/` directory

### HTML Styling

Custom CSS is embedded in R Markdown files for:
- Professional color scheme
- Responsive layout
- Sticky table of contents
- Print-friendly formatting

---

## Data Privacy & Ethics

### Dataset Considerations

- **Source**: Lending Club public dataset (2007-2018)
- **Sensitivity**: Contains loan and borrower information
- **Usage**: Educational and research purposes only
- **Compliance**: Follow Apache 2.0 license terms

### Best Practices

- Do not add personal identifiable information (PII) to repository
- Maintain balanced sampling to avoid discrimination
- Report model limitations and potential biases
- Acknowledge uncertainty in predictions

---

## Additional Resources

### R Documentation

- **caret**: https://topepo.github.io/caret/
- **tidyverse**: https://www.tidyverse.org/
- **pROC**: https://cran.r-project.org/package=pROC

### Statistical Methods

- **KNN**: Hastie, T., Tibshirani, R., & Friedman, J. (2009). The Elements of Statistical Learning
- **Logistic Regression**: Applied Logistic Regression (Hosmer & Lemeshow)
- **ROC Analysis**: Fawcett, T. (2006). An introduction to ROC analysis

### Git LFS

- **Documentation**: https://git-lfs.github.com/
- **Setup**: `git lfs install && git lfs track "*.csv"`

---

## Contact & Support

For questions about this codebase:

1. **Check existing code**: Most patterns are in `Camilo_final.R`
2. **Review R Markdown**: `Rmarkdown_final.Rmd` contains complete workflow
3. **Consult this guide**: CLAUDE.md (this file)

### Project Authors

- Alejandro Barros Rayo
- Diego Fernando Muñoz Portela
- Johan Camilo Portilla Aguirre
- Joan Sebastian Aguirre Aldana

**Institution**: [Universidad details not specified in code]
**Course**: Statistical Methods / Machine Learning
**Year**: 2018-2024 (based on dataset period)

---

## Version History

- **Initial Release**: Analysis of Lending Club dataset with KNN and Logit models
- **Current State**: Comprehensive comparison with multiple visualizations and reports

---

## Future Development Suggestions

### Potential Enhancements

1. **Model Expansion**:
   - Random Forest
   - Gradient Boosting (XGBoost)
   - Neural Networks
   - Support Vector Machines

2. **Feature Engineering**:
   - Interaction terms
   - Polynomial features
   - Time-based features from issue_d

3. **Advanced Validation**:
   - Nested cross-validation
   - Time-series split validation
   - Bootstrap confidence intervals

4. **Explainability**:
   - SHAP values
   - Partial Dependence Plots
   - Feature importance rankings

5. **Interactive Reporting**:
   - Shiny dashboard
   - Plotly interactive plots
   - Flexdashboard layout

---

## Summary for AI Assistants

### Quick Reference

**Language**: R
**Main Script**: `Camilo_final.R` (999 lines)
**Main Report**: `Rmarkdown_final.Rmd`
**Dataset**: `LC_loans_granting_model_dataset.csv` (Git LFS)
**Sample Size**: 10,000 (balanced)
**Train/Test**: 75/25 split
**Models**: KNN (k=optimal via CV), Logistic Regression
**Seed**: 28 (always!)
**Performance**: Both models ~60% accuracy, ~0.645 AUC

### When Making Changes

1. Set seed to 28
2. Use tidyverse syntax
3. Follow Spanish variable naming
4. Include proper visualizations with `tema`
5. Update comparison tables
6. Regenerate R Markdown reports
7. Validate confusion matrices sum correctly
8. Check AUC values are reasonable

### Key Principles

- **Reproducibility**: Always set seed(28)
- **Consistency**: Use established preprocessing pipeline
- **Documentation**: Comment complex transformations
- **Visualization**: Professional theme, Spanish labels, source attribution
- **Validation**: Cross-check metrics against confusion matrix

---

*Last Updated: 2025-11-16*
*Generated by: Claude AI Assistant*
