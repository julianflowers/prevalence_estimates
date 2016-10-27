## practice data extraction for prevalence estimates to create tidier output files

setwd("~/Documents/R_projects/prevalence_estimates")


## Load packages
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(haven))


## unzip and store files
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

## Note there are errors in the xlsx files which need correcting

## gp data - extract, combine, tidy


depress.gp <- read_excel("~/Documents/R_projects/prevalence_estimates/Depression-prevalence-estimates-practice-level.xlsx")

chd.gp <- read_excel("~/Documents/R_projects/prevalence_estimates/CHD-prevalence-estimates-&-QOF-prac-level.xlsx",sheet = "EstPrev+QOFPrev")

stroke.gp <- read_excel("~/Documents/R_projects/prevalence_estimates/Stroke-estimated-prevalence-&-QOF-practice-level.xlsx")

dxbp.gp <- read_excel("~/Documents/R_projects/prevalence_estimates/Diagnosed-hypertension-estimates-practice-level.xlsx",3)

qofbp.gp <- read_excel("~/Documents/R_projects/prevalence_estimates/Diagnosed-hypertension-estimates-practice-level.xlsx",1)

undxbp.gp <- read_excel("~/Documents/R_projects/prevalence_estimates/Undiagnosed-hypertension-estimates-practice-level.xlsx")

stroke1.gp <- read_excel("~/Documents/R_projects/prevalence_estimates/Stroke-estimated-prevalence-prac-level-with-IMD.xlsx")

pad1.gp <- read_excel("~/Documents/R_projects/prevalence_estimates/PAD-prevalence-estimates-&-QOF-practice-level-no-IMD.xlsx")

pad1imd.gp <- read_dta("~/Documents/R_projects/prevalence_estimates/PAD-prevalence-estimates-practice-level+QOF-with-IMD.dta")

copd.gp <- read_excel("~/Documents/R_projects/prevalence_estimates/COPD-estimated+QOF-prevalence-practice-level.xlsx")

list <- list(depress.gp, chd.gp, stroke.gp, dxbp.gp, undxbp.gp, qofbp.gp, stroke1.gp, pad1.gp, pad1imd.gp, copd.gp )


## Look at table dimenstions
sapply(list, dim)
sapply(list, str)

copd.gp <- copd.gp %>% select(practice_code, CCGgeogcode, `2011regionname`, listsize, qofprev = QOFprevalence, estimate = Estprevalence, min95, max95)
depress.gp <- depress.gp %>% select(practice_code, qofprev, estimate, min95, max95) 
chd.gp <- chd.gp %>% select(practice_code = practice_code, qofprev = QOFprev, estimate = Estprev, min95 = min95, max95 = max95)
stroke.gp <- stroke.gp %>% select(practice_code = practice_code, qofprev = QOFPrev, estimate = EstPrev, min95 = min95, max95=max95) 
dxbp.gp <- dxbp.gp %>% select(practice_code, estimate, min95, max95) 
qofbp.gp <- qofbp.gp %>% rename(practice_code = `Practice Code`, qofprev = `QOFprevalence\r\n`)
undxbp.gp <- undxbp.gp %>% select(practice_code, estimate, min95, max95)
stroke1.gp <- stroke1.gp %>% select(practice_code, estimate, min95, max95)
pad1.gp <- pad1.gp %>% select(practice_code, estimate = EstPrev, qofprev = QOFPrev, min95, max95)

head(depress.gp)

## join bp qof

dxbp.gp <- left_join(dxbp.gp, qofbp.gp) 
dxbp.gp <- dxbp.gp %>% select(-Estprevalence)

## create lookups

ccglookup <- select(copd.gp, practice_code, CCGgeogcode, `2011regionname`) %>% arrange(practice_code)

listlookup <- copd.gp %>% select(practice_code,listsize)  %>% arrange(practice_code)

copd.gp <- copd.gp %>% select(-CCGgeogcode, -`2011regionname`, -listsize)

copd.gp <- copd.gp %>% rename(practice_code=practice_code, qofprev = QOFprevalence, estimate = Estprevalence, min95=min95, max95 = max95)
## Convert files to long format
 
l_depress.gp <- gather(depress.gp, indicator, value, 2:ncol(depress.gp))
l_chd.gp <- gather(chd.gp, indicator, value, 2:ncol(chd.gp))
l_stroke.gp <- gather(stroke.gp, indicator, value, 2:ncol(stroke.gp))
l_dxbp.gp <- gather(dxbp.gp, indicator, value, 2:ncol(dxbp.gp))
l_undxbp.gp <- gather(undxbp.gp, indicator, value, 2:ncol(undxbp.gp))
l_stroke1.gp <- gather(stroke1.gp, indicator, value, 2:ncol(stroke1.gp))
l_pad1.gp <- gather(pad1.gp, indicator, value, 2:ncol(pad1.gp))
l_copd.gp <- gather(copd.gp, indicator, value, 2:ncol(copd.gp))

## Add disease label

l_depress.gp$disease <- "depression"
l_chd.gp$disease <- "chd"
l_stroke.gp$disease <- "stroke"
l_stroke1.gp$disease <- "stroke_IMD"
l_dxbp.gp$disease <- "bp"
l_undxbp.gp$disease <- "undx_bp"
l_pad1.gp$disease <- "pad"
l_copd.gp$disease <- "copd"

list1 <- list(l_depress.gp, l_chd.gp, l_stroke.gp, l_dxbp.gp, l_undxbp.gp, l_stroke1.gp, l_pad1.gp, l_copd.gp)

## Check table dimensions
sapply(list1, dim)

sapply(list1, summary)

## Bind rows together

gpprev<- bind_rows(list1)
dim(gpprev)


## Convert value field to numeric and round to 2 decimal places



# ## write files
# 
# write.csv(gpprev, file = "comb_gp_prev.csv")
# 
# ## clean up
# l <- ls()
# rm(l)
# ##==================================================================
# 
# 
# ## reload
# library(readr)
# prev <- read_csv("comb_gp_prev.csv")
# 
# ## summary
# 
# summary(prev)
# 
# ## there are different labels for QOF, prevalence, list size => more tidying needed
# ## Identify indicator names
# s <- split(prev, factor(prev$disease))
# s1 <- as.data.frame(unlist(lapply(s, function(x) levels(factor(x$indicator)))))
# colnames(s1) <- "measure"
# s1 ## list of measure names by disease
# 
# 
# 
# ## recode
# 
# prev$indicator <- ifelse(prev$indicator %in% pop, 'pop', prev$indicator)
# 
# 
# ## check measures
# 
# 
# unique(prev$indicator)
# 
# 
# ##### Recode register variables
# 
# qofreg <- c( "Register", "Register201415", "Estregister", "Est-QOF", "QOFregister" )
# 
# 
# ## recode
# 
# prev$indicator <- ifelse(prev$indicator %in% qofreg, 'register', prev$indicator)


### NAs

mean(is.na(gpprev))

### Remove NAs

gpprev <- gppprev %>% na.omit() %>% mutate(ind1 = paste(disease, indicator, sep = "-"))


#### Reshape

prev1 <- gpprev  %>% select(-disease, -indicator, -X1) %>% spread(ind1, value) 

prev1 %>% mutate(`bp-qofprev` = `bp-qofprev` * 100)

## Add ccg, region and listsize


prev1 <- prev1 %>% left_join(ccglookup)
prev1 <- prev1 %>% left_join(listlookup)

prev1 <- prev1 %>% select(1, 33:34, 32,  2:31)

## Save file

write_csv(prev1, paste0("gp.estimates",lubridate::today(), ".csv"))

## and long version

prev1 %>% gather(indicator, value, 4:34) %>% write_csv(paste0("gp.estimates_long",lubridate::today(),".csv"))











