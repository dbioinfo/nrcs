NRCS Data Wranglin



```mermaid
flowchart LR
    A([Biological_Results_Master_QCed]) --> B[crust_cleaner.R]
    C([MetaData_PLFA_MM]) --> B
    B --> D([cleaned_crust_data])
    D --> E[crust_plfa_eda]
    C --> E
    E --> F([plfa_eda.pdf])
```
