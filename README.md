# 🧬 Oncoprint Generator

An interactive R Shiny application for visualizing genomic alterations in cancer cohorts. Supports somatic and germline mutations, copy number variations (CNV), and gene fusions, with flexible clustering and customizable visualization parameters.

**Author:** Ermis I. Michail Delopoulos  
**Last updated:** April 2026

---

## Features

- Upload and integrate multiple data types: SNV mutations, CNVs, fusions, and clinical data
- Interactive oncoprint visualization powered by [ComplexHeatmap](https://bioconductor.org/packages/ComplexHeatmap/)
- Multiple clustering strategies: mutation-based, CNV-based, combined, gene-order priority, or file-based
- Gene family grouping with customizable family size thresholds
- Clinical annotation tracks (diagnosis type, age, sex, etc.)
- Germline vs somatic distinction with visual border encoding
- Downloadable PDF output
- Summary statistics tab with alteration frequencies

---

## Requirements

### R version
R >= 4.1.0 recommended

### R Packages

```r
install.packages(c(
  "shiny", "shinyWidgets", "shinyjs",
  "dplyr", "tidyr", "tibble", "stringr",
  "data.table", "DT", "RColorBrewer"
))

# Bioconductor packages
if (!require("BiocManager")) install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
```

---

## How to Run

### Option 1: From RStudio
Open `app.R` and click **Run App**.

### Option 2: From R console
```r
shiny::runApp("path/to/OncoPrint_app")
```

### Option 3: Launch script
Use the provided `launch_oncoprint.R` script:
```r
source("launch_oncoprint.R")
```

---

## Input File Formats

All input files are `.csv` format.

### 1. Clinical Data *(required)*
| Column | Description |
|--------|-------------|
| `Pt_ID` | Unique patient identifier |
| `Diagnosis_Type` | Cancer/diagnosis category |
| `Age` | Patient age (optional) |
| `Sex` | Patient sex (optional) |
| `Other columns for plotting` | Any other columns user want to plot (optional) |

### 2. Mutation SNV Data *(required)*
| Column | Description |
|--------|-------------|
| `Pt_ID` | Unique patient identifier |
| `Gene` | Gene symbol |
| `Type` | Mutation type (e.g. Truncation, Missense, Splice) |
| `CellType` | `Somatic` or `Germline` |

### 3. CNV Data *(optional)*
| Column | Description |
|--------|-------------|
| `Pt_ID` | Unique patient identifier |
| `Gene` | Gene symbol |
| `Event` | CNV event type (e.g. Gain, Loss, Amplification) |
| `CellType` | `Somatic` or `Germline` |

### 4. Fusion Data *(optional)*
| Column | Description |
|--------|-------------|
| `Pt_ID` | Unique patient identifier |
| `Fusion` | Fusion name (e.g. `NCOA4::RET`) |
| `Tier` | Fusion tier/confidence |
| `Gene_5` | 5' gene partner |
| `Gene_3` | 3' gene partner |

### 5. Cluster Assignments *(optional)*
| Column | Description |
|--------|-------------|
| `Pt_ID` | Unique patient identifier |
| `Cluster` | Pre-computed cluster label (e.g. methylation cluster) |

---

## Usage

1. **Upload Data** — Use the sidebar in the Instructions tab to upload your CSV files
2. **Preview Data** — Check the Data Preview tab to verify uploads
3. **Configure Parameters** — Go to the Oncoprint tab and set:
   - Diagnosis type filter
   - Gene ordering (frequency, custom, or gene family grouping)
   - Clustering strategy
   - Display options (mutation counts, annotations)
4. **Generate** — Click **Generate Oncoprint**
5. **Download** — Export as PDF

---

## Clustering Strategies

| Strategy | Description |
|----------|-------------|
| `None` | No column clustering |
| `File-based` | Uses uploaded cluster assignment file |
| `Mutation-based` | Hierarchical clustering on SNV matrix |
| `Combined` | Clustering on SNV + CNV combined matrix |
| `Gene-order priority` | Sorts patients by alteration in top-ranked genes |

---

## Output

- Interactive oncoprint rendered in-app
- Downloadable **PDF** with configurable dimensions
- Statistics tab with per-gene and per-patient alteration summaries

---

## Project Structure

```
OncoPrint_app/
├── app.R                  # Main Shiny application
├── launch_oncoprint.R     # Launch script
├── README.md              
```
