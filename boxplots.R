library(tidyverse)
library(ggplot2)
library(readxl)


## Data Wrangling

# Arrange dataset by SexCohort variable
data_reorder <- data %>% arrange(SexCohort) %>% mutate(name=factor(SexCohort, levels=c("Female_CN", "Male_CN", "Female_EOAD", "Male_EOAD", "Female_EOnonAD", "Male_EOnonAD")))

# Select variables for analysis
data_select <- data_reorder %>% select(SexCohort, WholeFBB_SUVR, MetaROI_FTP, Braak_1_FTP, Braak_12_FTP, Braak_34_FTP, Braak_34_FTP, Braak_56_FTP, MTL_thickness, Ent_thickness, MeanCortThick)

# Use dplyr gather function to re-shape dataset
data_boxplot <- data_select %>% gather("variable", "value", -SexCohort)

# Arrange dataset by SexCohort variable
data_boxplot2 <- data_boxplot %>% arrange(SexCohort) %>% mutate(SexCohort=factor(SexCohort, levels=c("Female_CN", "Male_CN", "Female_EOAD", "Male_EOAD", "Female_EOnonAD", "Male_EOnonAD")))

## Data Visualization

# Normal Control sex comparison of PET measures
CN_visual <- data_boxplot2 %>% filter(SexCohort == c("Female_CN", "Male_CN")) %>% ggplot(aes(x=variable, y=value, color=SexCohort)) + # give x,y and group info
  geom_point(aes(x=variable, y=value, fill=SexCohort), position=position_jitterdodge(), size=2.5, alpha=0.7) + # plot the data
  ylab("Standardized Uptake Value ratio") + # y-axis label
  xlab("PET measure") + # x-axis label
  theme_minimal() + labs(col="Subgroup", fill="Subgroup") + # adding theme minimal and naming the Legend title
  theme(axis.text.x = element_text(size=8), axis.text.y=element_text(size=10, face="bold")) + # setting font size and style for the axis
  geom_point(aes(group=SexCohort),stat="summary", fun.y="median", size=3, position=position_dodge(0.75), color="white", shape=17) + # adding the triangle for the median of the group, this is done twice so it looks like the icon has a border
  geom_point(aes(group=SexCohort),stat="summary", fun.y="median", size=2, position=position_dodge(0.75), color="#F26D04", shape=17) + 
  scale_color_manual(values=c("#052049", "#007CBE", "#052049", "#007CBE", "#052049", "#007CBE"))  # setting manually the colors for each group
  
# Early-onset Alz Dis. sex comparison of PET measures
EOAD_visual <- data_boxplot2 %>% filter(SexCohort == c("Female_EOAD", "Male_EOAD")) %>% ggplot(aes(x=variable, y=value, color=SexCohort)) + # give x,y and group info
  geom_point(aes(x=variable, y=value, fill=SexCohort), position=position_jitterdodge(), size=2.5, alpha=0.7) + # plot the data
  ylab("Standardized Uptake Value ratio") + # y-axis label
  xlab("PET measure") + # x-axis label
  theme_minimal() + labs(col="Subgroup", fill="Subgroup") + # adding theme minimal and naming the Legend title
  theme(axis.text.x = element_text(size=8), axis.text.y=element_text(size=10, face="bold")) + # setting font size and style for the axis
  geom_point(aes(group=SexCohort),stat="summary", fun.y="median", size=3, position=position_dodge(0.75), color="white", shape=17) + # adding the triangle for the median of the group, this is done twice so it looks like the icon has a border
  geom_point(aes(group=SexCohort),stat="summary", fun.y="median", size=2, position=position_dodge(0.75), color="#F26D04", shape=17) + 
  scale_color_manual(values=c("#052049", "#007CBE", "#052049", "#007CBE", "#052049", "#007CBE"))  # setting manually the colors for each group

# Early onset non-Alz Dis sex comparison of PET measures
EOnonAD_visual <- data_boxplot2 %>% filter(SexCohort == c("Female_EOnonAD", "Male_EOnonAD")) %>% ggplot(aes(x=variable, y=value, color=SexCohort)) + # give x,y and group info
  geom_point(aes(x=variable, y=value, fill=SexCohort), position=position_jitterdodge(), size=2.5, alpha=0.7) + # plot the data
  ylab("Standardized Uptake Value ratio") + # y-axis label
  xlab("PET measure") + # x-axis label
  theme_minimal() + labs(col="Subgroup", fill="Subgroup") + # adding theme minimal and naming the Legend title
  theme(axis.text.x = element_text(size=8), axis.text.y=element_text(size=10, face="bold")) + # setting font size and style for the axis
  geom_point(aes(group=SexCohort),stat="summary", fun.y="median", size=3, position=position_dodge(0.75), color="white", shape=17) + # adding the triangle for the median of the group, this is done twice so it looks like the icon has a border
  geom_point(aes(group=SexCohort),stat="summary", fun.y="median", size=2, position=position_dodge(0.75), color="#F26D04", shape=17) + 
  scale_color_manual(values=c("#052049", "#007CBE", "#052049", "#007CBE", "#052049", "#007CBE"))  # setting manually the colors for each group

