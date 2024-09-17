This repo contains datasets for fatalities data released by the Gaza Ministry of Health in 2023-2024.

## Datasets 

### Raw Data Files
These are the (near) bottom-level source files, one for each of the fatality lists MoH released to date via Telegram:
- `data/source_files/fatalities_20231026_src.txt` ([source - pdf](https://t.me/MOHMediaGaza/4300)) *
- `data/source_files/fatalities_20240105_src.xlsx` ([source - xlsx](https://t.me/MOHMediaGaza/4740))
- `data/source_files/fatalities_20240329_src.txt` ([source - pdf](https://t.me/MOHMediaGaza/5261)) *
- `data/source_files/fatalities_20240430_src.txt` ([source - pdf](https://t.me/MOHMediaGaza/5405)) *
- `data/source_files/fatalities_20240630_src.txt` ([source - pdf](https://t.me/MOHMediaGaza/5652)) *
- `data/source_files/fatalities_20240831_src.txt` ([source - pdf](https://t.me/MOHMediaGaza/5823)) *

\* Converted to txt using Adobe Acrobat (File > Export a PDF > Text (Accessible))

### Lightly Processed CSVs
These are more usable datasets in CSV format after light preprocessing (such as standardization of date formats, handling of missing/special values):
- Files: `data/fatalities_(20231026|20240105|20240329|20240430|20240630|20240831).csv` 
- Parsing & preprocessing scripts: `scripts/01 Data Ingestion (20231026|20240105|20240329|20240430|20240630|20240831).R`

### Airwars.org Incident Reports
Moved to [alexschell/airwars-gaza-data](https://github.com/alexschell/airwars-gaza-data)
