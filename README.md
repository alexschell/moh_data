This repo contains datasets for fatalities data released by the Gaza Ministry of Health in 2023-2024.

## Datasets 

### Raw Data Files
These are the ~bottom-level source files, one for each of the 4 fatality lists MoH released so far via Telegram:
- `data/source_files/fatalities_20231026.txt` ([source - pdf](https://t.me/MOHMediaGaza/4300))
    - Converted to text using Adobe Acrobat (File > Export a PDF > Text (Accessible))
- `data/source_files/fatalities_20240105.xlsx` ([source - xlsx](https://t.me/MOHMediaGaza/4740))
    - Original xlsx
- `data/source_files/fatalities_20240329.txt` ([source - pdf](https://t.me/MOHMediaGaza/5261))
`data/source_files/fatalities_20240430.txt` ([source - pdf](https://t.me/MOHMediaGaza/5405))
    - Converted from PDF to text as above

### Minimally Processed CSVs
- Files: `data/fatalities_(20231026|20240105|20240329|20240430)_raw.csv` 
- These are the respective outputs of `scripts/01 Data Ingestion (20231026|20240105|20240329|20240430).R` and are more accessible & useful while still faithful to the original documents (no data cleaning, etc.)

### Partly Cleaned Up CSVs (WIP - will be deprecated)
- Files: `data/fatalities_(20231026|20240105|20240329|20240430)_wip.csv`
- R code: `scripts/02 Preprocessing (20231026|20240105|20240329|20240430).R`
