# MammalResearchABWS
Data repository for ABWS paper 2
##### Temporal patterns of a Mammal Species along with 3 Main Disturbances #### 

#Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Read the data files with updated file path
golden_jackal_data <- read.csv("C:/Users/HP/Desktop/#ST_Overlaps_among_species/goldenjackalSTdata.csv")
human_data <- read.csv("C:/Users/HP/Desktop/#ST_Overlaps_among_species/human_STdata.csv")
straydog_data <- read.csv("C:/Users/HP/Desktop/#ST_Overlaps_among_species/straydogSTdata.csv")
vehicle_data <- read.csv("C:/Users/HP/Desktop/#ST_Overlaps_among_species/vehicleSTdata.csv")

# Ensure that your time data is in the correct format
golden_jackal_data$Time <- hms(golden_jackal_data$Time) # Assuming 'Time' is a column in "HH:MM:SS" format
human_data$Time <- hms(human_data$Time)
straydog_data$Time <- hms(straydog_data$Time)
vehicle_data$Time <- hms(vehicle_data$Time)

# Extract hour from the time data
golden_jackal_data$Hour <- hour(golden_jackal_data$Time)
human_data$Hour <- hour(human_data$Time)
straydog_data$Hour <- hour(straydog_data$Time)
vehicle_data$Hour <- hour(vehicle_data$Time)

# Aggregate data by hour for plotting
golden_jackal_activity <- golden_jackal_data %>%
  group_by(Hour) %>%
  summarise(Activity_Count = n()) %>%
  mutate(Species = "Golden Jackal")

human_activity <- human_data %>%
  group_by(Hour) %>%
  summarise(Activity_Count = n()) %>%
  mutate(Species = "Human")

straydog_activity <- straydog_data %>%
  group_by(Hour) %>%
  summarise(Activity_Count = n()) %>%
  mutate(Species = "Stray Dog")

vehicle_activity <- vehicle_data %>%
  group_by(Hour) %>%
  summarise(Activity_Count = n()) %>%
  mutate(Species = "Vehicle")

# Combine the four data frames for plotting
activity_data <- bind_rows(golden_jackal_activity, human_activity, straydog_activity, vehicle_activity)
#  Polar Plot (with thicker lines)
polar_plot <- ggplot(activity_data, aes(x = Hour, y = Activity_Count, fill = Species)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.5) + # Increased line width
  coord_polar(start = -pi/2) +
  scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d:00", seq(0, 23, by = 1))) +
  scale_fill_manual(values = c("Golden Jackal" = "gold", "Human" = "dodgerblue", "Stray Dog" = "coral", "Vehicle" = "mediumorchid")) +
  labs(title = "Temporal Activity patterns of Golden Jackal in Relation to some disturbances",
       subtitle = "A Comparative Polar Plot Over a 24-Hour Period",
       fill = "Species") +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = "lightyellow", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA),
    plot.title = element_text(hjust = 0.5, size = 26, face = "bold", color = "darkred"), # Bigger title
    plot.subtitle = element_text(hjust = 0.5, size = 20, face = "italic", color = "darkred"), # Bigger subtitle
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 16, color = "black"), # Larger axis text
    axis.text.y = element_blank(),
    legend.title = element_text(size = 22, face = "bold", color = "darkred"), # Larger legend title
    legend.text = element_text(size = 18, color = "black"), # Larger legend text
    legend.position = "top"
  )
Save the Polar Plot as a JPEG file
ggsave("C:/Users/HP/Desktop/Golden_Jackal_Polar_Plot.jpeg", plot = polar_plot, width = 14, height = 14, units = "in", dpi = 300)


### Ripley’s K function ###

Load necessary libraries
library(spatstat)
library(ggplot2)

# Load your data
nilgai_data <- read.csv("C:/Users/HP/Desktop/nilgaiSTdata.csv")
human_data <- read.csv("C:/Users/HP/Desktop/human_STdata.csv")
straydog_data <- read.csv("C:/Users/HP/Desktop/straydogSTdata.csv")
vehicle_data <- read.csv("C:/Users/HP/Desktop/vehicleSTdata.csv")

# Filter and prepare the data
nilgai_data <- nilgai_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

human_data <- human_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

straydog_data <- straydog_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

vehicle_data <- vehicle_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Define the study area window based on the range of your coordinates
xrange <- range(c(nilgai_data$Longitude, human_data$Longitude, straydog_data$Longitude, vehicle_data$Longitude))
yrange <- range(c(nilgai_data$Latitude, human_data$Latitude, straydog_data$Latitude, vehicle_data$Latitude))
study_area <- owin(xrange, yrange)

# Convert the data into spatial point patterns
nilgai_ppp <- ppp(nilgai_data$Longitude, nilgai_data$Latitude, window = study_area)
human_ppp <- ppp(human_data$Longitude, human_data$Latitude, window = study_area)
straydog_ppp <- ppp(straydog_data$Longitude, straydog_data$Latitude, window = study_area)
vehicle_ppp <- ppp(vehicle_data$Longitude, vehicle_data$Latitude, window = study_area)

# Compute Ripley's K-function for each dataset
K_nilgai <- Kest(nilgai_ppp)
K_human <- Kest(human_ppp)
K_straydog <- Kest(straydog_ppp)
K_vehicle <- Kest(vehicle_ppp)

# Adjust the layout and text sizes
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 1.5) + 0.1)

# Plot the K-function for each species/disturbance with adjusted text size
plot(K_nilgai, main = "Ripley's K-function: Nilgai", col = "blue", lwd = 2,
     cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)
plot(K_human, main = "Ripley's K-function: Human Disturbance", col = "red", lwd = 2,
     cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)
plot(K_straydog, main = "Ripley's K-function: Stray Dog Disturbance", col = "yellow", lwd = 2,
     cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)
plot(K_vehicle, main = "Ripley's K-function: Vehicle Disturbance", col = "purple", lwd = 2,
     cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)

# Save the final 2x2 plot as a JPEG file
jpeg("C:/Users/HP/Desktop/Ripley_K_All_Combined.jpeg", width = 1000, height = 1000)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 1.5) + 0.1)
plot(K_nilgai, main = "Ripley's K-function: Nilgai", col = "blue", lwd = 2,
     cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)
plot(K_human, main = "Ripley's K-function: Human Disturbance", col = "red", lwd = 2,
     cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)
plot(K_straydog, main = "Ripley's K-function: Stray Dog Disturbance", col = "yellow", lwd = 2,
     cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)
plot(K_vehicle, main = "Ripley's K-function: Vehicle Disturbance", col = "purple", lwd = 2,
     cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)
dev.off()


#### Species accumulation Curve ####

# Install and load the vegan package if not already installed
if (!require(vegan)) install.packages("vegan", dependencies = TRUE)
library(vegan)

# Define input and output paths
input_file <- "C:/Users/HP/Desktop/GLM7/SAC.csv"  # Update path if needed
output_dir <- "C:/Users/HP/Desktop/GLM7"
output_file <- file.path(output_dir, "cBeautiful_Species_Accumulation_Curve_Enhanced_Grid_Squares.jpeg")

# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Load the data, ensure it's properly formatted
data <- read.csv(input_file, row.names = 1)

# Check and clean the data
str(data)  # Check the structure of the data
data <- data[, sapply(data, is.numeric)]  # Keep only numeric columns

# Convert counts to presence/absence (if needed)
data[data > 0] <- 1

# Handle NA values by removing rows or filling with zeros
data[is.na(data)] <- 0

# Check if the data frame is empty after cleaning
if (nrow(data) == 0 || ncol(data) == 0) {
  stop("The cleaned data is empty. Please check the data file for correct formatting.")
}

# Create the Species Accumulation Curve using the 'random' method
spec_accum <- specaccum(data, method = "random")

# Check if the spec_accum object contains valid data
if (all(is.infinite(spec_accum$richness))) {
  stop("Species accumulation curve calculation failed due to invalid data. Please check input data.")
}

# Save the Species Accumulation Curve plot as a JPEG in GLM7 directory on your desktop
jpeg(filename = output_file, width = 2400, height = 1800, units = "px", res = 300)  # Increased size for more plot area

# Adjust plot margins to increase space on the left and bottom sides
par(mar = c(6, 6, 3, 3) + 0.1, bg = "white")  # Increased bottom and left margins

# Plot the species accumulation curve with adjusted title and axis label sizes
plot(spec_accum, 
     xlab = "Number of Sampling Days/Camera Traps", 
     ylab = "Number of Species", 
     main = "Species Accumulation Curve",  # Title appears once, centered
     ci.type = "polygon",  # Keep the polygon for CI shading
     col = "#1f78b5",        # Deep blue line for the main curve
     ci.col = adjustcolor("#FF69B4", alpha.f = 0.4), # Pink for the CI area with transparency
     lwd = 5,                # Thicker main curve line for emphasis
     ci.lty = 0,             # No line type for CI polygon
     cex.main = 1.7,         # Increased size for the main title
     cex.lab = 1,            # Standard size for axis labels
     cex.axis = 1.5,           # Standard size for axis text
     lty = 1,                # Solid line type
     pch = 16,               # Use filled points
     xaxt = "n",             # Custom x-axis ticks
     yaxt = "n")             # Custom y-axis ticks

# Add custom x-axis ticks in increments of 1
x_ticks <- seq(0, max(spec_accum$sites, na.rm = TRUE), by = 1)  # Scale x-axis by 1
axis(1, at = x_ticks, labels = x_ticks, cex.axis = 0.8, font = 2)

# Add complete y-axis ticks scaled by 2 including 20
y_ticks <- seq(0, 20, by = 2)  # Include all numbers from 0 to 20 in increments of 2
axis(2, at = y_ticks, labels = y_ticks, cex.axis = 0.8, font = 2)  # Use scaled labels

# Add vertical error bars for standard errors using arrows
arrows(x0 = spec_accum$sites, 
       y0 = spec_accum$richness - spec_accum$sd, 
       x1 = spec_accum$sites, 
       y1 = spec_accum$richness + spec_accum$sd, 
       angle = 90, 
       code = 3, 
       length = 0.05, 
       col = "black", 
       lwd = 1.5)  # Error bars with arrows

# Add a subtle gradient background
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
     col = adjustcolor("#f0f8ff", alpha.f = 0.1), border = NA)  # Light blue gradient background

# Add larger, square, darker grid lines for enhanced visual appeal
grid(nx = 15, ny = 15, col = adjustcolor("gray60", alpha.f = 0.8), lty = "solid", lwd = 0.5)  # Larger square grids

# Add decorative elements to make the plot more appealing
abline(h = seq(0, 20, by = 2), col = adjustcolor("gray60", alpha.f = 0.6), lty = "dotted")  # Horizontal guide lines
abline(v = seq(0, max(spec_accum$sites, na.rm = TRUE), by = 5), col = adjustcolor("gray60", alpha.f = 0.6), lty = "dotted")  # Vertical guide lines

# Add points to highlight each data point on the curve
points(spec_accum$sites, spec_accum$richness, pch = 21, bg = "#ff4500", col = "black", cex = 1.5)

# Enhance the plot with a border
box(which = "plot", lty = "solid", col = "darkgray", lwd = 2)  # Dark gray box around the plot for emphasis

# Close the graphics device to save the plot
dev.off()

