This repository contains code for the publication 

"National, regional, and global trends in insufficient physical activity among adults from 2000 to 2022: a pooled analysis of 507 population-based surveys with 5·7 million participants" (*Lancet Global Health 2024*) by Tessa Strain, Seth Flaxman, Regina Guthold, Elizaveta Semenova, Melanie Cowan, Leanne M Riley, Fiona C Bull, Gretchen A Stevens, and the Country Data Author Group,  

avilable at this [link](https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(24)00150-5/fulltext).

## Folder Structure

```
.
├── PinA.Rproj
├── README.md
├── Stata
│   ├── GPAQ_code_for_github.do
│   ├── IPAQ_code_for_github.do
│   └── readme.md
├── data
│   ├── covariates.csv
│   ├── pina_dataset.csv
│   └── readme.md
├── figures
│   └── readme.md
├── fits
│   ├── model_f.rds
│   ├── model_m.rds
│   └── readme.md
├── predictions
│   └── readme.md
├── results
│   ├── Strain et al 2024 country and region estimates - Github.xlsx
│   └── readme.md
├── src
│   ├── 01_brms.R
│   ├── 02_draws.R
│   ├── 03_make_fit_plots.R
│   └── readme.md
└── tmp_files
    └── readme.md    
```


## Folder Descriptions
- **Stata**: Stata scripts.
- **data**: Contains `pina_dataset.csv` and `covariates.csv` datasets.
- **figures**: Stores generated figures.
- **fits**: Includes `brms` fitting results.
- **predictions**: Includes predictions from fitted models.
- **results**: Includes results.
- **src**: Source code for the project.
- **tmp_files**: Temporary files generated during analysis.
