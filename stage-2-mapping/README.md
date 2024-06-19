# MCH-ML Stage 2: Indicator geospatial mapping

This folder contains scripts to prepare and run spatial predictive models for five indicators related to post-neonatal mortality.

## Prepared indicators

The five indicators to be mapped are:
- Child stunting (%)
- Child wasting (%)
- Household wealth index
- Age at first birth for women ages 20-49
- Women meeting the SDG empowerment indicator: making decisions regarding their own contraception, healthcare, and sexual activity (%)

## Scripts

The scripts in this folder can be run in order:

1. `01-indicator-collapse.R`: This script downloads, cleans, and collapses each indicator by country and DHS topic. Three DHS recodes are pulled for each country: the individual woman recode (for age at first birth and empowerment), childen recode (for stunting and wasting), and household recode (for household wealth index). This script has a similar function to `../data-prep/data-preparation.R`, except the data is collapsed by survey cluster rather than split into individual children and age categories.
2. `02-combine-datasets.R`: This script combines datasets across countries, producing one collapsed dataset for each indicator and one microdata dataset for each recode type.
3. `03-geostatistical-model.R`: This script runs geospatial models for all indicators, relying on functions from the external [`mbg` package](https://github.com/henryspatialanalysis/mbg).

## External resources

See [Bhatt _et al._ (2017)](https://doi.org/10.1098/rsif.2017.0520) for an overview of the geospatial modeling methods.
