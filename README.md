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
│   └── Old/                # Superseded drafts; not part of the final workflow
├── graphs/                  # Generated figures
├── tables/                  # Generated tables
├── .gitignore
└── README.md
```

The file extensions above are shown for clarity. Input subdirectories and file availability may vary according to data access and local configuration. The repository may also contain RStudio project files and local R session files; these are not analytical inputs.

## Data sources and provenance

| Source | Purpose | Access / notes |
|---|---|---|
| **CIPSA (Climate Indices for Pacific Sectoral Applications)** | Homogenised monthly observations for Lord Howe Island, reference-location observations, and historical climate-extreme indices | [CIPSA](https://cipsap.org/). Historical climate analyses use observations principally over **1940–2024**; availability varies by variable and index. |
| **Queensland Future Climate projections** | Downscaled temperature and precipitation simulations for the grid cell near Lord Howe Island | Supplied directly by the Queensland Future Climate team; **not assumed to be publicly redistributable**. Projections analysed under **SSP3-7.0**, extending to 2100. |
| **Relative Niño 3.4 index** | ENSO–climate correlations | Bureau of Meteorology; original source and download date should be recorded with the input file. |
| **Regional and global temperature datasets** | Contextual comparison of Lord Howe Island warming | Bureau of Meteorology Australian regional series, Berkeley Earth and HadSST4; source URLs and versions should be recorded alongside downloaded files. |
| **Severe-weather and spatial information** | Context for hazards, historical impacts and qualitative risk assessment | Published technical reports, cyclone/event archives, local records and spatial datasets. Not all sources are loaded by the R scripts. |

Further source descriptions and intended usage are recorded in `datasources.xlsx` **if included with the project data**. For fully reproducible results, preserve each original filename, source URL or provider, download/access date, version, units, temporal coverage and any redistribution restrictions. Do not commit restricted source data without permission.

### Important analytical conventions

- Historical observed temperature and rainfall series are analysed primarily over **1940–2024**, subject to data completeness.
- Monthly climatological anomalies use the **1961–1990** reference period unless noted otherwise.
- A seasonal or annual observation is treated as missing if any constituent month for that variable is missing; variables are handled independently.
- Summer (DJF) assigns December to the following season-year.
- Future temperature is bias-adjusted additively against observed monthly means for **2015–2024**; future rainfall is bias-adjusted multiplicatively against observed monthly totals for **1981–2024**. Corrections are determined separately by simulation and calendar month.
- Future projections are scenario-based model estimates, **not forecasts**. Model spread, baseline choice, bias correction and the approximately 10 km model grid-cell representation introduce uncertainty, particularly for local rainfall.

## Software and dependencies

Analyses are written in **R**. Use a current R version compatible with the packages called in the scripts. Packages used across the workflow include `dplyr`, `tidyr`, `ggplot2`, `lubridate`, `purrr`, `slider`, `patchwork`, `lmtest`, `sandwich`, `car`, `segmented`, `nlme`, `trend`, `mblm` and `gt`; individual scripts may require additional packages. Consult the actual `library()` and `pkg::function()` calls for the definitive list.

For a reproducible computational environment, record the R version and package versions using `sessionInfo()` or `renv`. A dependency lockfile is not currently documented here; results may vary slightly across package versions.

## Reproducing the analysis

1. **Obtain input data** from their original providers, respecting access and licensing conditions. Place files in the paths expected by the scripts under `data/` (notably `data/cipsap/`, `data/enso/` and `data/qld_futuremodels/tas/` and `pr/`). Some projection inputs may need to be requested from the Queensland Future Climate team.
2. **Open the repository as an RStudio project** (if using RStudio) or set the R working directory to the **repository root**, not `scripts/`. Input paths such as `data/cipsap/...` and output paths such as `graphs/...` are relative to this root.
3. **Install the required packages** and check for any local absolute paths, including `setwd()` statements, before execution. Paths referring to the original author's computer must be replaced or removed.
4. **Run the scripts in the order below**, preferably in a fresh R session. The scripts exchange objects through the R environment; they are not currently guaranteed to be runnable independently.
5. **Check the outputs** in `graphs/` and `tables/` against the accompanying thesis/supplementary material. Review warnings and missing-data counts rather than assuming that successful execution implies valid outputs.

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

These are intermediate in-memory objects, not independently versioned datasets. For stricter reproducibility, a future improvement would be to save these objects in a documented interchange format and remove reliance on a shared R session.

## Outputs

- `graphs/`: thesis figures, supplementary figures and diagnostic plots generated by the scripts.
- `tables/`: analytical tables where scripts have been configured to write to this directory.

**Check each script's export paths:** some scripts may write HTML, CSV or image files under `graphs/` rather than `tables/`. Output folders are distinct from original data; generated files should not be treated as primary observations.

## Reproducibility, limitations and reuse

- **Scope:** This repository supports the quantitative climate analysis informing a *first-pass*, qualitative climate risk assessment. It does not constitute a fully quantitative risk model.
- **Input availability:** Some future projection files were provided directly by a research team and may not be deposited publicly. Independent replication of those analyses requires authorised access to the same model outputs.
- **Environment:** The scripts should be run from a clean session with a documented R/package environment. Any remaining machine-specific paths should be removed before public release.
- **Data quality:** Missing months are not silently imputed in the historical annual/seasonal summaries. Modelled results and trend significance should be interpreted alongside diagnostic checks and uncertainty estimates.
- **Provenance:** Preserve raw input files unchanged, and document any updates to versions or time coverage; such changes can alter published results.
- **Reuse:** The workflow may be adapted to other CIPSA stations and future projection datasets, but variable names, calibration periods, observational overlap, season definitions, units and site-specific assumptions must be revalidated.

### Citation and licensing

When using this work, cite the accompanying thesis/project and **the original data providers**. The repository's software licence and the licensing/redistribution permissions for third-party inputs should be specified before public distribution. **No permission to redistribute externally supplied climate model data is implied by this README.**

## Suggested steps before public release

- Remove tracked `.RData`, `.Rhistory`, local machine paths and any restricted raw data; maintain an appropriate `.gitignore`.
- Add a reproducible R environment (`renv.lock`) and record `sessionInfo()` for the final analysis.
- Confirm that all nine scripts run from a clean session in the documented order and that all input/output paths resolve.
- Include a data manifest with exact source versions, access dates, download links and checksums where redistribution is permitted.
- Verify that figures and tables match the submitted thesis and document any deliberate differences.
