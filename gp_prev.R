

setwd("~/Documents/R_projects/prevalence_estimates")

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(haven))



unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/PAD-prevalence estimates.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")

unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/CHD-prevalence-estimates.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")


unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/Stroke-estimated-prevalence.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")


unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/Hypertension-estimates.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")

unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/FW%3a_Stroke_prevalence_model_estimates.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")

unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/COPD_prevalence_estimates_&_Technical_Document.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")

unzip(zipfile = "~/Documents/R_projects/prevalence_estimates/COPD-prevalence-estimates.zip", 
      exdir = "~/Documents/R_projects/prevalence_estimates")

## GP data - extract, combine, tidy


depress.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Depression-prevalence-estimates-practice-level.xlsx")

chd.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/CHD-prevalence-estimates-&-QOF-prac-level.xlsx")

stroke.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Stroke-estimated-prevalence-&-QOF-practice-level.xlsx")

dxbp.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Diagnosed-hypertension-estimates-practice-level.xlsx",3)

qofbp.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Diagnosed-hypertension-estimates-practice-level.xlsx",1)

undxbp.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Undiagnosed-hypertension-estimates-practice-level.xlsx")

stroke1.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/Stroke-estimated-prevalence-prac-level-with-IMD.xlsx")

pad1.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/PAD-prevalence estimates/PAD-prevalence-estimates-&-QOF-practice-level-no-IMD.xlsx")

pad1imd.GP <- read_dta("~/Documents/R_projects/prevalence_estimates/PAD-prevalence estimates/PAD-prevalence-estimates-practice-level+QOF-with-IMD.dta")

copd.GP <- read_excel("~/Documents/R_projects/prevalence_estimates/COPD-estimated+QOF-prevalence-practice-level.xlsx")

list <- list(depress.GP, chd.GP, stroke.GP, dxbp.GP, undxbp.GP, qofbp.GP, stroke1.GP, pad1.GP, pad1imd.GP, copd.GP )


## Look at table dimenstions
sapply(list, dim)
sapply(list, class)

  
  
## Convert files to long format
 
l_depress.GP <- gather(depress.GP, indicator, value, 2:ncol(depress.GP))
l_chd.GP <- gather(chd.GP, indicator, value, 2:ncol(chd.GP))
l_stroke.GP <- gather(stroke.GP, indicator, value, 2:ncol(stroke.GP))
l_dxbp.GP <- gather(dxbp.GP, indicator, value, 2:ncol(dxbp.GP))
l_undxbp.GP <- gather(undxbp.GP, indicator, value, 2:ncol(undxbp.GP))
l_qofbp.GP <- gather(qofbp.GP, indicator,value, 2:ncol(qofbp.GP))
l_stroke1.GP <- gather(stroke1.GP, indicator, value, 2:ncol(stroke.GP))

l_pad1.GP <- gather(pad1.GP, indicator, value, 2:ncol(pad1.GP))
l_padimd1.GP <- gather(pad1imd.GP, indicator, value, 2:ncol(pad1imd.GP))
l_copd.GP <- gather(copd.GP, indicator, value, 2:ncol(copd.GP))

## Labels

l_depress.GP$disease <- "depression"
l_chd.GP$disease <- "chd"
l_stroke.GP$disease <- "stroke"
l_stroke1.GP$disease <- "stroke_IMD"
l_dxbp.GP$disease <- "bp"
l_undxbp.GP$disease <- "undx_bp"
l_qofbp.GP$disease <- "bp"
l_pad1.GP$disease <- "pad"
l_padimd1.GP$disease <- "pad_IMD"
l_copd.GP$disease <- "copd"

list1 <- list(l_depress.GP, l_chd.GP, l_stroke.GP, l_dxbp.GP, l_undxbp.GP, l_qofbp.GP, l_stroke1.GP, l_pad1.GP, l_padimd1.GP, l_copd.GP)

## Check table dimensions
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

## Recode different indicator names as prevalence estimate and QOF estimates

gp_all_prev <- gp_all %>%
  mutate(prevalence = ifelse(indicator == "Estprevalence" | indicator == "Estimated prevalence" | indicator == "Prevestimate" | indicator == "EstPrev" | indicator == "Estprev" | indicator == "estimate",1,0)) %>%
  mutate(qof = ifelse(indicator == "QOFprev" | indicator == "QOFprevaelnce201415"| indicator == "QOFprevalence\r\n" | indicator == "QOFprevalence" | indicator == "Prevalence\r\n(per cent)", 1, 0)) 

head(gp_all_prev)
gp_all_prev$index <- with(gp_all_prev, paste(disease, indicator, sep = "."))


gp_all_prev1 <- as.data.table(gp_all_prev %>% filter(prevalence == 1)) %>% select(practice_code, index, value)
gp_all_prev1 <- gp_all_prev1[-(75511:75512),] ## exclude duplicate rows

## Convert file back to wide format
wgp <- data.table(spread(gp_all_prev1, index, value))



## Convert QOF dato wide format
gp_all_prev2 <- as.data.table(gp_all_prev %>% filter(qof == 1)) %>% select(practice_code, index, value)
gp_all_prev2 <- gp_all_prev2[-(30366:30367),]

wgp1 <- data.table(spread(gp_all_prev2, index, value))


# Join datasets
setkey(wgp, practice_code)
setkey(wgp1, practice_code)

df <- wgp[wgp1]

## NAs

apply(df, 2,  function(x) mean(is.na(x)))

## Simplify column names

colnames(df) <- c("prac.code", "bp(%)", "bp1", "chd", "copd", "depression","pad_IMD", "pad",  "stroke.IMD", "stroke", "bp_notdiag", "bp.qof", "chd.qof", "copd.qof", "depression.qof")

kable(head(df))



write.csv(df, "prev_est_wide.csv")


library(DT)
datatable(df,
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
          caption = "Prevalence and QOF estimates")

dt <- data.frame(df)

dt <- dt %>%
  select(bp1, bp.qof, depression, depression.qof, chd, chd.qof, copd, copd.qof) %>%
  mutate(bp.diff = bp1- bp.qof, bp.ave = (bp1 + bp.qof)/2, 
         dep.diff = depression - depression.qof, dep.ave = (depression + depression.qof)/2,
         chd.diff = chd - chd.qof, chd.ave = (chd + chd.qof)/2,
         copd.diff = copd - copd.qof, copd.ave = (copd + copd.qof)/2)

summary(dt)

## Bland Altman plots - plot differences between model and observed vs mean model + observed values

q1 <-qplot(data = dt, 100 * bp.ave, 100 * bp.diff) + 
  geom_hline(yintercept = 100 * mean(dt$bp.diff, na.rm = TRUE), colour = "red") + 
  geom_hline(yintercept = 100 * (mean(dt$bp.diff, na.rm = TRUE) + 1.96 * sd(dt$bp.diff, na.rm = TRUE)), colour = "blue") + 
  geom_hline(yintercept = 100 * (mean(dt$bp.diff, na.rm = TRUE) - 1.96 * sd(dt$bp.diff, na.rm = TRUE)), colour = "blue") 


q2 <-qplot(data = dt, dep.ave, dep.diff) + geom_hline(yintercept = mean(dt$dep.diff, na.rm = TRUE), colour = "red") + geom_hline(yintercept = mean(dt$dep.diff, na.rm = TRUE) + 1.96 * sd(dt$dep.diff, na.rm = TRUE), colour = "blue") + geom_hline(yintercept = mean(dt$dep.diff, na.rm = TRUE) - 1.96 * sd(dt$dep.diff, na.rm = TRUE), colour = "blue")

q3 <-qplot(data = dt, chd.ave, chd.diff) + geom_hline(yintercept = mean(dt$chd.diff, na.rm = TRUE), colour = "red")+ geom_hline(yintercept = mean(dt$chd.diff, na.rm = TRUE) + 1.96 * sd(dt$chd.diff, na.rm = TRUE), colour = "blue") + geom_hline(yintercept = mean(dt$chd.diff, na.rm = TRUE) - 1.96 * sd(dt$chd.diff, na.rm = TRUE), colour = "blue")

q4 <-qplot(data = dt, copd.ave, copd.diff) + geom_hline(yintercept = mean(dt$chd.diff, na.rm = TRUE), colour = "red")+ geom_hline(yintercept = mean(dt$copd.diff, na.rm = TRUE) + 1.96 * sd(dt$copd.diff, na.rm = TRUE), colour = "blue") + geom_hline(yintercept = mean(dt$copd.diff, na.rm = TRUE) - 1.96 * sd(dt$copd.diff, na.rm = TRUE), colour = "blue")

library(gridExtra)
grid.arrange(q1, q2, q3, q4, ncol = 2 )

## Some plots and summaries

dev.off()
require(corrplot)
df <- data.frame(df)
cor <- cor(df[,-1], use = 'complete.obs')
corrplot(cor,  order = 'hclust', tl.cex = 0.5, tl.col = "black", addrect = 2, main = "Correlations between observed and predicted prevalences")

qplot(data = filter(gp_all_prev, prevalence ==1), value, geom = "density", fill = disease) + xlab("Estimated prevalence %") + facet_wrap(~disease, scales = "free") + ggtitle("Density plots of estimated disease prevelance")

qplot(data = filter(gp_all_prev, prevalence ==1| qof ==1),  disease, log10(value), geom = "boxplot", fill = disease) + xlab("Estimated prevalence % (log scale)") + coord_flip() + ggtitle("Variation in practice level prevalence by disease")



## NAs

prevna <-  gp_all_prev %>% 
  filter(prevalence ==1) %>%
  group_by(disease) %>%
  summarise(meanNA = mean(is.na(value)))

qplot(data = prevna, disease, meanNA *100, geom = "point")


pracNA <- gp_all_prev %>% filter(is.na(value) & prevalence == 1) %>% select(practice_code, disease, value)
head(pracNA)

pracNA <- pracNA[-(231:232),]

pracNAw <- spread(pracNA, disease, value)
kable(pracNAw)

