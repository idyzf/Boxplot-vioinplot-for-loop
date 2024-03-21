library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyselect)
library(tidyverse)
library(tidyr)
library(readxl)
library(flextable)
library(gtsummary)
library(openxlsx)
library(rstatix)
library(tidyverse)
library(ggpubr) 
library(plotly) 
library(foreign)

rm(list=ls())
clinical<- read.xlsx("clinical_variables_oficial.xlsx")
names(clinical)
ruv_data <- read_excel("ruvcorrected.xlsx")

data_boxplot <- clinical[,c(1,12)]
names(ruv_data)
CDK7 <- ruv_data[,c(1,2)]
CDK7$ID <- CDK7$Name

data <- left_join(CDK7, data_boxplot)

file_name = paste("Boxplot CDK7.tiff", sep="")
tiff(file_name)
# Specify the desired order of groups:
desired_order <- c("First", "Second", "Third", "Fourth")

# Reorder the factor levels based on the specified order:
data$Survival.Groups <- factor(data$Survival.Groups, levels = desired_order)

# Create the boxplot, ensuring correct group order:
df<- na.omit(data)

stat.test <- df %>% t_test(CDK7 ~ Survival.Groups)
stat.test

# Box plot
stat.test <- stat.test %>% add_xy_position(x = "Survival.Groups")
bxp <- ggboxplot(df, x = "Survival.Groups", y = "CDK7", fill = "Survival.Groups", 
                 palette = c("blue", "green", "orange", "red"), xlab = "Groups",
                 ylab = "CDK7",)
bxp + stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)

dev.off()  # Close the tiff file

