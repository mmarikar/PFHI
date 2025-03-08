# 3/5/2025
# Introduced PFHI_Kidney_FeMale


# 2/27/2025
# increase sizes of axis labels and plot title by modifying the theme's axis text and title, and centering the heading and increasing its font size

# Load required packages
library(ggplot2)
library(dplyr)
library(readxl)

PFHICo <- read_excel("OrganWeightsPFAS.xlsx", sheet='PFHI', na = c("", "NA")) 
#View(PFHICo)
# Include only female (rows 61-120)
PFHICom <- PFHICo[c(61:120),]
#View(PFHICom)
# Ensure Group is treated as an ordered factor 
dose_levels <- c(0, 12.5, 25, 50, 100, 200) # Define correct dose order
PFHICom$Group <- factor(PFHICom$Group, levels = dose_levels, ordered = TRUE)

# Manually select 50,100 and 200 mg/kg-day groups
top_doses <- PFHICom %>%
  filter(Group %in% c(25,50,100,200)) %>% #select only 50, 100 and 200 mg/kg-day
  group_by(Group) %>%
  summarise(max_KidneyRel = max(KidneyRel, na.rm = TRUE)) %>%
   arrange(desc(max_KidneyRel)) %>% # Sort by median values
   slice(1:2) %>% # Select the two highest median values
  mutate(y_position = max_KidneyRel + 0.2) # Adjust y-position for asterisk placement

#Get overall max LiverRel to set y-axis limits
y_max <- max(PFHICom$KidneyRel + 0.3) #Increase spacing to avoid cutoff

#Debugging using 2 lines below
#View(PFHICom$KidneyRel)
#View(max((PFHICom$KidneyRel)))


# Create the violin + box plot
#KidneyRel1m
PFHI_Kidney_Female<- ggplot(PFHICom, aes(x = Group, y = KidneyRel, fill = Group)) +
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") + # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.9) + # Box plot
  
  # Ensure the median is properly displayed
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "red") +
  
  # Add asterisks above the 100 and 200 mg/kg-day groups 
  geom_text(data = top_doses, aes(x = Group, y = y_position, label = "*"),
            size = 6, color = "black", vjust = -0.5) + #Move the asterisk up
  
  #Manually set y-axis limits to prevent asterisk cut-off
  ylim(NA, y_max) +
  
  labs(title = "PFHI - Female Rat Kidney/Body Weight 90-Day Study",
       x = "Dose Group (mg/kg-day)", y = "Kidney/BW Ratio (%)") +
  
  scale_fill_brewer(palette = "Set2", name = "Group") + # Colorful palette
  theme_bw() +
  theme(legend.position = "none") +

  theme(
    axis.text = element_text(size = 14), # increase tick label size
    axis.title = element_text(size = 16), # increase axis title size
    plot.title = element_text(size = 18, hjust = 0.5) # increase plot title size
  )


# Save the plot as a PNG file
ggsave(filename = "PFHI_Kidney_Female.png", plot = PFHI_Kidney_Female, width = 8, height = 6, dpi = 300)

# Display the plot
#KidneyRel1m
PFHI_Kidney_Female