

setwd("~/Documents/R_projects/prevalence_estimates")

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tidyr))

unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/Depression-estimates.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")

unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/CHD-prevalence-estimates.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")

unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/PAD-prevalence-estimates.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")


unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/Stroke-estimated-prevalence.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")


unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/Hypertension-estimates.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")

## GP data - extract, combine, tidy


depress.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Depression-prevalence-estimates-practice-level.xlsx")

chd.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/CHD-prevalence-estimates-&-QOF-prac-level.xlsx")

stroke.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Stroke-estimated-prevalence-&-QOF-practice-level.xlsx")

dxbp.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Diagnosed-hypertension-estimates-practice-level.xlsx",3)

qofbp.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Diagnosed-hypertension-estimates-practice-level.xlsx",1)

undxbp.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Undiagnosed-hypertension-estimates-practice-level.xlsx")

list <- list(depress.GP, chd.GP, stroke.GP, dxbp.GP, undxbp.GP, qofbp.GP )

sapply(list, dim)
sapply(list, class)

  
  

 
l_depress.GP <- gather(depress.GP, indicator, value, 2:ncol(depress.GP))
l_chd.GP <- gather(chd.GP, indicator, value, 2:ncol(chd.GP))
l_stroke.GP <- gather(stroke.GP, indicator, value, 2:ncol(stroke.GP))
l_dxbp.GP <- gather(dxbp.GP, indicator, value, 2:ncol(dxbp.GP))
l_undxbp.GP <- gather(undxbp.GP, indicator, value, 2:ncol(undxbp.GP))
l_qofbp.GP <- gather(qofbp.GP, indicator,value, 2:ncol(qofbp.GP))

l_depress.GP$disease <- "depression"
l_chd.GP$disease <- "chd"
l_stroke.GP$disease <- "stroke"
l_dxbp.GP$disease <- "bp"
l_undxbp.GP$disease <- "undx_bp"
l_qofbp.GP$disease <- "bp"

list1 <- list(l_depress.GP, l_chd.GP, l_stroke.GP, l_dxbp.GP, l_undxbp.GP, l_qofbp.GP)
sapply(list1, dim)
sapply(list1, summary)


gp_all <- rbindlist(list1)
dim(gp_all)

gp_all$value <- round(as.numeric(gp_all$value),2)
summary(gp_all$value)

write.csv(gp_all, file = "comb_gp_prev.csv")

## Identify indicators
s <- split(gp_all, factor(gp_all$disease))
s1 <- as.data.frame(unlist(lapply(s, function(x) levels(factor(x$indicator)))))
colnames(s1) <- "measure"
s1


gp_all_prev <- gp_all %>%
  mutate(prevalence = ifelse(indicator == "Estprevalence" | indicator == "Estimated prevalence" | indicator == "EstPrev" | indicator == "Estprev" | indicator == "estimate",1,0)) %>%
  mutate(qof = ifelse(indicator == "QOFprev" | indicator == "QOFprevalence\r\n" | indicator == "Prevalence\r\n(per cent)", 1, 0)) 

head(gp_all_prev)
gp_all_prev$index <- with(gp_all_prev, paste(disease, indicator, sep = "."))


gp_all_prev1 <- as.data.table(gp_all_prev %>% filter(prevalence == 1)) %>% select(practice_code, index, value)
wgp <- data.table(spread(gp_all_prev1, index, value))


gp_all_prev2 <- as.data.table(gp_all_prev %>% filter(qof == 1)) %>% select(practice_code, index, value)
wgp1 <- data.table(spread(gp_all_prev2, index, value))


setkey(wgp, practice_code)
setkey(wgp1, practice_code)

DT <- wgp[wgp1]

apply(DT, 2,  function(x) mean(is.na(x)))

colnames(DT) <- c("prac.code", "bp", "bp1", "chd", "depression", "stroke", "bp_notdiag", "bp.qof", "chd.qof", "depression.qof")


library(DT)
datatable(DT,
          extensions = 'Buttons', 
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
          caption = "Prevalence and QOF estimates")

dt <- data.frame(DT)

source('~/Documents/R_projects/K_GP/panelcor.R')
pairs(dt[,-1], lower.panel = panel.smooth, pch = 20)

