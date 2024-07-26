This repo contains datasets for fatalities data released by the Gaza Ministry of Health in 2023-2024.

## Datasets 

### Raw Data Files
These are the (near) bottom-level source files, one for each of the 5 fatality lists MoH released via Telegram as of 2024-06-30:
- `data/source_files/fatalities_20231026_src.txt` ([source - pdf](https://t.me/MOHMediaGaza/4300)) *
- `data/source_files/fatalities_20240105_src.xlsx` ([source - xlsx](https://t.me/MOHMediaGaza/4740))
- `data/source_files/fatalities_20240329_src.txt` ([source - pdf](https://t.me/MOHMediaGaza/5261)) *
- `data/source_files/fatalities_20240430_src.txt` ([source - pdf](https://t.me/MOHMediaGaza/5405)) *
- `data/source_files/fatalities_20240630_src.txt` ([source - pdf](https://t.me/MOHMediaGaza/5652)) *

\* Converted to txt using Adobe Acrobat (File > Export a PDF > Text (Accessible))

### Lightly Processed CSVs
These are more usable datasets in CSV format after light preprocessing (such as standardization of date formats, handling of missing/special values):
- Files: `data/fatalities_(20231026|20240105|20240329|20240430|20240630).csv` 
- Parsing & preprocessing scripts: `scripts/01 Data Ingestion (20231026|20240105|20240329|20240430|20240630).R`

### Airwars.org Incident Reports
These are CSVs of casualties scraped from incident reports compiled by [Airwars](https://airwars.org/conflict/israel-and-gaza-2023/) (as of 2024-07-26, there are 550 incidents spanning from 2023-10-07 through 2024-05-12, mostly up to 2023-10-25). Many of the fatalities have been matched to Ministry of Health fatality records.
- Files: `data/airwars/airwars_incidents.csv` `data/airwars/airwars_individuals.csv`
- Script: `scripts/airwars/Incident Report Parsing.R`
