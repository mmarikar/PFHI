# 3/5/2025
# Renamed KidneyRel1m.png as PFHI_Kidney_Male.png
# Renamed KidneyRel1m as PFHI_Kidney_Male

# 2/28/2025
# increase sizes of axis labels and plot title by modifying the theme's axis text and title, and centering the heading and increasing its font size

# Load required packages
library(ggplot2)
library(dplyr)
library(readxl)

PFHICo <- read_excel("OrganWeightsPFAS.xlsx", sheet='PFHI', na = c("", "NA")) 
#View(PFHICo)
# Include only male (rows 61-120)
PFHICom <- PFHICo[-c(61:120),]
#View(PFHICom)
# Ensure Group is treated as an ordered factor 
dose_levels <- c(0, 12.5, 25, 50, 100, 200) # Define correct dose order
PFHICom$Group <- factor(PFHICom$Group, levels = dose_levels, ordered = TRUE)

# Manually select 25,50,100 and 200 mg/kg-day groups
top_doses <- PFHICom %>%
  filter(Group %in% c(25,50,100,200)) %>% #select only 100 and 200 mg/kg-day
  group_by(Group) %>%
  summarise(max_LiverRel = max(LiverRel, na.rm = TRUE)) %>%
   arrange(desc(max_LiverRel)) %>% # Sort by median values
   slice(1:4) %>% # Select the two highest median values
  mutate(y_position = max_LiverRel + 0.2) # Adjust y-position for asterisk placement

#Get overall max LiverRel to set y-axis limits
y_max <- max(PFHICom$LiverRel + 0.3) #Increase spacing to avoiad cutoff


# Create the violin + box plot
#LiverRel1m
PFHI_Liver_Male <- ggplot(PFHICom, aes(x = Group, y = LiverRel, fill = Group)) +
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") + # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.9) + # Box plot
  
  # Ensure the median is properly displayed
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "red") +
  
  # Add asterisks above the 100 and 200 mg/kg-day groups 
  geom_text(data = top_doses, aes(x = Group, y = y_position, label = "*"),
            size = 6, color = "black", vjust = -0.5) + #Move the asterisk up
  
  #Manually set y-axis limits to prevent asterisk cut-off
  ylim(NA, y_max) +
  
  labs(title = "PFHI - Male Rat Liver/Body Weight 90-Day Study",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  
  scale_fill_brewer(palette = "Set2", name = "Group") + # Colorful palette
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    axis.text = element_text(size = 14), # increase tick label size
    axis.title = element_text(size = 16), # increase axis title size
    plot.title = element_text(size = 18, hjust = 0.5) # increase plot title size
  )

# Save the plot as a PNG file
#ggsave(filename = "LiverRel1m.png", plot = LiverRel1m, width = 8, height = 6, dpi = 300)
ggsave(filename = "PFHI_Liver_Male.png", plot = PFHI_Liver_Male, width = 8, height = 6, dpi = 300)
PFHI_Liver_Male