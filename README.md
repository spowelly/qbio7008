# qbio7008
## Datasets and analysis for QBIO7008 project - First Pass Climate Risk Assessment and Adaptation Options for Lord Howe Island.

This repo contains scripts that clean, analyse and produce tables and figures for a first-pass climate change risk assessment for Lord Howe Island. It contains four folders:

### data
Contains all data sources used for analysis: 
Used by scripts folder.
cipsap for local climate observations and extremes. enso for relative nino3.4 index
Not used by scripts folder.
severe_weather contains aggregated information for severe weather events including cyclones.
not_used: not used in analysis but useful for future analysis

Full information about data sources, including descriptions and usage are in: datasources.xlsx

### scripts
Contains all scripts used for analysis and data visualisation.
scripts/Old contains previous scripts/draft scripts before refactoring.

Run order for scripts is as follows:
1. dataImport
2. dataImport_Future
3. annualClimate
4. monthlyClimate
5. changepoints
6. climateExtremes
7. enso
8. summaryTables
9. regionalContext

Script summaries are within scripts.

### tables
Output location for tables.

### graphs
Output location for graphs.

Scripts may be repurposed for use to develop risk assessments in other locations using CIPSAP data and relevant future projection models (should be sourced directly from relevant agency). 
