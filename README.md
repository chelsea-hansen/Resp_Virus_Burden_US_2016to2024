
Data and code for research project entitled “Hospitalizations and Deaths
from Major Respiratory Viruses in the US: an ensemble time series
modeling study, 2016-2024”

## Repository Structure

### `data/`

This folder contains data files used for running the time series
regression models.

- **`Data_public.rds`**  
  Data used for running time series regression models.
  - `date`: MMWR week ending date  
  - `season`: Respiratory virus season defined as week 27 of year *x*
    through week 26 of year *(x + 1)*  
  - `year_decimal`: Continuous variable for year
- **`Recorded_burden.rds`**  
  Recorded season-level, virus-specific burden data used for comparison
  with modeled estimates.
  - 

------------------------------------------------------------------------

### `R/`

This folder contains the functions needed to reproduce the analysis.

- **`Model_functions.R`**  
  Runs the five time series models included in the ensemble.  
  Key arguments:
  - `dat`: Dataset name  
  - `y_value`: Outcome variable being modeled

------------------------------------------------------------------------

### `scripts/`

This folder contains scripts that call the functions in the `R/` folder.

- **`Mortality_models.R`**  
  Script to run the mortality models.
