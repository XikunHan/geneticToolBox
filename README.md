

A set of useful functions for genomic data analysis. 

## Install package
```r
devtools::install_github("XikunHan/geneticToolBox")
library(geneticToolBox)
```


## Files
#### list all files with detailed information in a folder
```r
# check many large files, eg. GWAS summary statistics.

df <- file_info(path = "~/data/", pattern = "GWAS")
summary(df)
df[which.min(size_MB), ]
```

#### read the last line of all files in a folder
```r
# check many log files

df <- read_last_line(path = "~/log/", pattern = ".")
dd <- df[!str_detect(content, "Warning"), ]
```


## Analyse
#### check data sets before merging
```r
# eg. data type of IDs, duplicates
merge_check(df_1, df_2, by.x = "ID1", by.y = "ID2")
```


#### merge multiple data tables
```r
df_m <- mergeDataSets(list(df_1, df_2, df_3), by = "id", all = TRUE)
```


#### meta-analysis based on inverse variance method for a data table.
```r
df_res <- meta_inverse_variance_df(df_res, beta1 = "estimate_1", se1 = "std.error_1", 
beta2 = "estimate_2", se2 = "std.error_2")
```
