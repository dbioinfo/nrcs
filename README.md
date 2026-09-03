# NRCS Data Wranglin

This repository contains a data wrangling pipeline for the Pietrasiask lab. The pipeline takes 2 main inputs in its current state: `MetaData_PLFA_MM.xlsx` and a directory of csv files `PLFAData`
 
The metadata file defines the samples that are used in the final report as well as several categorical ecological variables. In this file, the column name that defines the SampleID is `Sample ID 2`, which is the ID used to match metadata to data aggregated from PLFA csvs.

The `PLFAData` directory is full of CSVs from a third party who measured various biomass markers. Importantly, the same `Sample ID 2` column must be present in each file to provide each sample with a unique identifier. Negligible measurements ('<0.01') are collapsed to 0 for computational purposes.

```mermaid
flowchart TD
    A([PLFA_merged.csv]) --> B[crust_cleaner.R]
    C([MetaData_PLFA_MM.xlsx]) --> B
    B --> D([cleaned_crust_data.csv])
    D --> E[crust_plfa_eda.R]
    C --> E
    E --> F([plfa_eda.pdf])
    G([PLFAData/*]) --> H[nrcs_plfa_wranglin.R]
    H --> A
```


The data comes from a large range of arid climates, providing insight on microclimates, lichen demographics and substrate specificity in the American Southwest.

![samples](figs/AI_site_map_category_cropped.png)


![relbiomass](figs/RelativeBiomass.png)

