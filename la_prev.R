## pactice data extraction for prevalence estimates

setwd("~/Documents/R_projects/prevalence_estimates")

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(haven))


## LA data - extract, combine, tidy


depress.LA <- read_stata("~/Documents/R_projects/prevalence_estimates/Diag-depression-prev-LA-level.dta")

chd.LA <- read_excel("~/Documents/R_projects/prevalence_estimates/CHD-prevalence-estimates-LA-level.xlsx")

stroke.LA <- read_excel("~/Documents/R_projects/prevalence_estimates/Stroke-estimated-prevalence-LA-level-with-IMD.xlsx")

dxbp.LA <- read_excel("~/Documents/R_projects/prevalence_estimates/Diagnosed-hypertension-estimates-LA-level.xlsx")

# qofbp.LA <- read_excel("~/Documents/R_projects/prevalence_estimates/Diagnosed-hypertension-estimates-practice-level.xlsx",1)

# undxbp.LA <- read_excel("~/Documents/R_projects/prevalence_estimates/Undiagnosed-hypertension-estimates-practice-level.xlsx")

# stroke1.LA <- read_excel("~/Documents/R_projects/prevalence_estimates/Stroke-estimated-prevalence-prac-level-with-IMD.xlsx")

pad1.LA <- read_excel("~/Documents/R_projects/prevalence_estimates/PAD-prevalence estimates/PAD-prevalence-estimates-LA-level-no-IMD.xlsx")

pad1imd.LA <- read_excel("~/Documents/R_projects/prevalence_estimates/PAD-prevalence estimates/PAD-prevalence-estimates-LA-level-with-IMD.xlsx")

copd.LA <- read_excel("~/Documents/R_projects/prevalence_estimates/COPD-estimated-prevalence-LA-level.xlsx")

list <- list(depress.LA, chd.LA, stroke.LA, dxbp.LA, pad1.LA, pad1imd.LA, copd.LA )

## Look at table dimenstions
sapply(list, dim)
sapply(list, str)



l_depress.LA <- gather(depress.LA, indicator, value, 4:ncol(depress.LA))
l_chd.LA <- gather(chd.LA, indicator, value, 4:ncol(chd.LA))
l_stroke.LA <- gather(stroke.LA, indicator, value, 4:ncol(stroke.LA))
l_dxbp.LA <- gather(dxbp.LA, indicator, value, 4:ncol(dxbp.LA))
# l_undxbp.LA <- gather(undxbp.LA, indicator, value, 2:ncol(undxbp.LA))
# l_qofbp.LA <- gather(qofbp.LA, indicator,value, 4:ncol(qofbp.LA))
# l_stroke1.LA <- gather(stroke1.LA, indicator, value, 2:ncol(stroke.LA))

l_pad1.LA <- gather(pad1.LA, indicator, value, 4:ncol(pad1.LA))
l_padimd1.LA <- gather(pad1imd.LA, indicator, value, 4:ncol(pad1imd.LA))
l_copd.LA <- gather(copd.LA, indicator, value, 4:ncol(copd.LA))

## Labels

l_depress.LA$disease <- "depression"
l_chd.LA$disease <- "chd"
l_stroke.LA$disease <- "stroke"
# l_stroke1.LA$disease <- "stroke_IMD"
l_dxbp.LA$disease <- "bp"
# l_undxbp.LA$disease <- "undx_bp"
# l_qofbp.LA$disease <- "bp"
l_pad1.LA$disease <- "pad"
l_padimd1.LA$disease <- "pad_IMD"
l_copd.LA$disease <- "copd"

list1 <- list(l_depress.LA, l_chd.LA, l_stroke.LA, l_dxbp.LA, l_pad1.LA, l_padimd1.LA, l_copd.LA)

## Check table dimensions
sapply(list1, dim)

sapply(list1, summary)

la_all <- bind_rows(list1)
dim(la_all)

la_all$value <- round(as.numeric(la_all$value),2) ## check and round numeric data
summary(la_all$value)

## write file

write.csv(la_all, file = "comb_la_prev.csv")

## Identify indicators 

s <- split(la_all, factor(la_all$disease))
s1 <- as.data.frame(unlist(lapply(s, function(x) levels(factor(x$indicator)))))
colnames(s1) <- "measure"
s1

## Create wide version of estimates

la_allw <- la_all %>% filter(indicator == "estimate") %>% select(la_code, value, disease)

la_allw <- la_allw %>% spread(disease, value)
head(la_allw)

## write file

write.csv(la_allw, file = "la_prev_estimates.csv")

dim(la_allw)

## missing data

missData <- apply(la_allw, 2, function(x) mean(is.na(x)))
missData

require(knitr)

kable(la_allw[which(is.na(la_allw$bp)),]) ## missing LA data for BP
kable(la_allw[which(is.na(la_allw$chd)),]) ## missing data for CHD

