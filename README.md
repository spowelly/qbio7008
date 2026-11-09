# Lord Howe Island climate risk assessment

**QBIO7008 — First-Pass Climate Risk Assessment and Adaptation Options for Lord Howe Island**

This repository contains the R scripts, input-data organisation and analytical outputs supporting a first-pass assessment of climate-related risks to the Lord Howe Island community. The analysis examines historical temperature, rainfall and climate extremes; associations with the El Niño–Southern Oscillation (ENSO); future climate projections; and implications for climate risk and adaptation planning.

The repository is intended to make the quantitative analyses, tables and figures transparent and reproducible. The qualitative risk assessment also draws on published reports and historical event records, which should be consulted alongside the analysis.

## Repository structure

```text
.
├── data/
│   ├── cipsap/              # Historical climate observations and climate-extreme indices
│   ├── enso/                # ENSO index data
│   ├── qld_futuremodels/    # Queensland Future Climate model output (where available)
│   ├── severe_weather/      # Compiled historical severe-weather information
│   └── not_used/            # Supporting data not included in the final analysis
├── scripts/
│   ├── 1_dataImport.R
│   ├── 2_dataImport_future.R
│   ├── 3_annualClimate.R
│   ├── 4_monthlyClimate.R
│   ├── 5_changepoints.R
│   ├── 6_climateExtremes.R
│   ├── 7_enso.R
│   ├── 8_summaryTables.R
│   ├── 9_regionalContext.R
│   └── 
├── graphs/                  # Generated figures
├── tables/                  # Generated tables
├── .gitignore
└── README.md
```

The file extensions above are shown for clarity. Superseded draft scripts not part of the final workflow have been removed from the repository.

## Data sources and provenance
Source descriptions and intended usage are recorded in `datasources.xlsx`.

## Software and dependencies

Analyses are written in **R**. Use a current R version compatible with the packages called in the scripts. Packages used across the workflow include `dplyr`, `tidyr`, `ggplot2`, `lubridate`, `purrr`, `slider`, `patchwork`, `lmtest`, `sandwich`, `car`, `segmented`, `nlme`, `trend`, `mblm` and `gt`; individual scripts may require additional packages. Consult the actual `library()` and `pkg::function()` calls for the definitive list.

## Reproducing the analysis

1. **Obtain input data** from their original providers, respecting access and licensing conditions. Place files in the paths expected by the scripts under `data/` (notably `data/cipsap/`, and future models in `data/<future_model>. future_model should be adjusted to the relevant model, and filenames and extensions will have to be adjusted accordingly, primarily in 2_dataImport_future).
2. **Open the repository as an RStudio project** (if using RStudio) or set the R working directory to the **repository root**, not `scripts/`. Input paths such as `data/cipsap/...` and output paths such as `graphs/...` are relative to this root.
3. **Install the required packages**
4. **Run the scripts in the order below**
5. **Check the outputs**.

### Script order and purpose

| Order | Script | Main function |
|---:|---|---|
| 1 | `1_dataImport.R` | Import and clean historical monthly observations; create monthly, seasonal and annual datasets, anomalies and rolling variability measures. |
| 2 | `2_dataImport_future.R` | Import downscaled daily projections, aggregate to monthly values, apply mean-based bias corrections and create future ensemble datasets. |
| 3 | `3_annualClimate.R` | Analyse historical and projected annual temperature/rainfall trends and variability; produce annual figures and supporting diagnostics. |
| 4 | `4_monthlyClimate.R` | Analyse monthly patterns and monthly trends; produce historical/future climatology figures, anomaly heatmaps and monthly regression diagnostics. |
| 5 | `5_changepoints.R` | Test and compare linear, segmented and spline trends, including relevant model diagnostics. |
| 6 | `6_climateExtremes.R` | Analyse historical climate-extreme indices and prepare extreme-event trend figures. |
| 7 | `7_enso.R` | Examine ENSO relationships with historical seasonal climate anomalies. |
| 8 | `8_summaryTables.R` | Assemble formatted historical, extreme-index and projected-climate summary tables. |
| 9 | `9_regionalContext.R` | Compare Lord Howe Island climatology and warming with selected mainland, regional and global datasets. |

`1_dataImport.R` should precede the historical analysis scripts; `2_dataImport_future.R` should precede analyses that use projected data. `8_summaryTables.R` uses objects assembled in preceding steps. `scripts/Old/` is archival and should not be included in the reproducible run sequence.

### Expected principal R objects

- **Historical:** `monthly_climate`, `seasonal_climate`, `annual_climate` and associated climatology datasets.
- **Projected:** `future_climate_models`, `future_monthly_trends`, `future_monthly_trends_20yr`, `tas_ensemble_annual`, `rain_ensemble_annual` and `future_rain_seasonal`.

## Outputs

- `graphs/`: figures, supplementary figures and diagnostic plots generated by the scripts.
- `tables/`: analytical tables where scripts have been configured to write to this directory.

## Reproducibility, limitations and reuse

- **Scope:** This repository supports the quantitative climate analysis informing a *first-pass*, qualitative climate risk assessment. It does not constitute a fully quantitative risk model.
- **Input availability:** Some future projection files were provided directly by a research team and may not be deposited publicly. Independent replication of those analyses requires authorised access to the same model outputs.
- **Reuse:** The workflow may be adapted to other CIPSA stations and future projection datasets, but variable names, calibration periods, observational overlap, season definitions, units and site-specific assumptions must be revalidated.

### Citation and licensing
**No permission to redistribute externally supplied climate model data is implied by this README. Please consult original data sources as listed in data_sources.xlsx**

Note: This ReadMe was created with the assistance of AI.
