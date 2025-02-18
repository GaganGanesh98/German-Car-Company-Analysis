# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

# Loading the dataset
df <- read_csv("~/CSV_Data_Sets/germany.csv")

# Checking the structure and first few rows
str(df)
head(df)
summary(df)



#Data Cleaning & Preprocessing
#Handle missing values, remove duplicates, and convert categorical data

# Checking for missing values
colSums(is.na(df))

# Remove duplicate rows
df <- df %>% distinct()

# Convert categorical variables to factors
df$Fuel_Type <- as.factor(df$fuel)
df$Gear_Type <- as.factor(df$gear)
df$Brand <- as.factor(df$make)



# Visualizing the Gear to EV Transition
#Analyze how fuel types have changed over the years

# Counting the number of cars by Fuel Type per year
fuel_trend <- df %>%
  group_by(year, Fuel_Type) %>%
  summarise(count = n())

# Create the plot with enhancements
ggplot(fuel_trend, aes(x = year, y = count, color = Fuel_Type, group = Fuel_Type)) +
  geom_line(size = 1.2) +  # Thicker lines for better visibility
  geom_point(size = 3) +   # Add points for each data point
  theme_minimal(base_size = 14) +  # Use a minimal theme with larger font
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    axis.title.x = element_text(face = "bold", size = 14),             
    axis.title.y = element_text(face = "bold", size = 14),             
    legend.title = element_text(face = "bold", size = 12),            
    legend.text = element_text(size = 10),                            
    panel.grid.major = element_line(color = "pink"),              
    panel.grid.minor = element_blank()                              
  ) +
  labs(
    title = "Fuel Type Transition Over the Years",
    x = "Year",
    y = "Number of Cars",
    color = "Fuel Type"  # Legend title
  ) 
#####



#Identifying the Best-Selling Cars

# Finding the top-selling brands
top_brands <- df %>%
  count(Brand, sort = TRUE) %>%
  top_n(11, n)

# Bar plot of top-selling brands
ggplot(top_brands, aes(x = reorder(Brand, n), y = n, fill = Brand)) +
  geom_bar(stat = "identity",
           show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Selling Car Brands in Germany",
       x = "Brand",
       y = "Number of Cars Sold")
####




#Analyze the Trend of Car Prices Over Time
# Calculating average price per year for each brand
price_trend <- df %>%
  group_by(year, Brand) %>%
  summarise(Average_Price = mean(price, na.rm = TRUE))

# View the summarized data
head(price_trend)

#Visualize the Price Trend Over Time
# Plot the price trends for different brands
ggplot(price_trend, aes(x = year, y = Average_Price, color = Brand, group = Brand)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Average Car Prices Over Time by Brand",
       x = "year",
       y = "Average Price (€)",
       color = "Car Brand") +
  theme(legend.position = "right")
####




#Best Fuel Type for Cost Efficiency
#comparing fuel types based on car price and performance

# Calculating average price by fuel type
fuel_price <- df %>%
  group_by(fuel) %>%
  summarise(Average_Price = mean(price, na.rm = TRUE))

# Creating the plot with enhancements
ggplot(fuel_price, aes(x = reorder(fuel, -Average_Price), y = Average_Price, fill = fuel)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8,show.legend = FALSE) +  
  geom_text(aes(label = paste0("€", round(Average_Price, 2))),  
            vjust = -0.5, size = 5, color = "black") +         
  scale_fill_brewer(palette = "Set3") +                      
  theme_minimal(base_size = 14) +                              
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    axis.title.x = element_text(face = "bold", size = 14),           
    axis.title.y = element_text(face = "bold", size = 14),             
    axis.text.x = element_text(angle = 45, hjust = 1),               
  ) +
  labs(
    title = "Average Car Price by Fuel Type",
    x = "Fuel Type",
    y = "Average Price (€)"
  ) +
  scale_y_continuous(labels = scales::comma)  
#####




#Horsepower vs. Car Price
# Check for missing values in 'hp' and 'price'
sum(is.na(df$hp))    # Check for missing values in 'hp'
sum(is.na(df$price)) # Check for missing values in 'price'

# Remove rows with missing values in 'hp' or 'price'
df_clean <- df %>% filter(!is.na(hp)) %>% filter(!is.na(price))

# Create the scatter plot
ggplot(df_clean, aes(x = hp, y = price)) +
  geom_point(alpha = 0.3, color = "red", shape = "square") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  theme_minimal() +
  labs(title = "Horsepower vs Car Price",
       x = "Horsepower (hp)",
       y = "Price (€)")
####




#Fuel Efficiency vs. Horsepower 
#Are fuel-efficient cars less powerful?

ggplot(df, aes(x = hp, y = mileage)) +
  geom_hex(bins = 35) +  # Adjust the number of bins as needed
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_fill_viridis_c() +  # Use a color scale for better visualization
  theme_minimal() +
  labs(title = "Horsepower vs Mileage",
       x = "Horsepower (hp)",
       y = "Mileage (km/L)",
       fill = "Count")
####




#EV Adoption & Growth
#Analyze how the number of EVs has changed over time

# Filter for EVs and count per year
ev_trend <- df %>%
  filter(Fuel_Type == "Electric") %>%
  group_by(year) %>%
  summarise(EV_Count = n())

# Plot EV growth over years with enhancements
ggplot(ev_trend, aes(x = year, y = EV_Count)) +
  geom_line(color = "#1f78b4", size = 1.5) +  
  geom_point(color = "#33a02c", size = 3) +  
  theme_minimal(base_size = 14) +             
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    axis.title.x = element_text(face = "bold", size = 14),             
    axis.title.y = element_text(face = "bold", size = 14),             
    panel.grid.major = element_line(color = "gray90"),                 
    panel.grid.minor = element_blank()                                 
  ) +
  labs(
    title = "Growth of Electric Vehicles in Germany",
    x = "Year",
    y = "Number of EVs"
  ) 
####



#Correlation Between Car Price and Performance
#Find if expensive cars have better fuel efficiency, horsepower, or safety ratings

# Correlation heatmap
library(corrplot)

# Selecting numeric columns and calculating correlations
numeric_cols <- df %>%
  select_if(is.numeric) %>%
  cor(use = "complete.obs")  # Using pairwise complete observations

# Custom color palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Plotting correlation heatmap with enhancements
corrplot(numeric_cols, 
         method = "color",       
         type = "upper",         
         tl.cex = 0.9,           
         tl.col = "black",        
         tl.srt = 45,            
         number.cex = 0.8,       
         addCoef.col = "black",  
         col = col(200),        
         diag = FALSE,           
         cl.pos = "r",          
         cl.ratio = 0.2,         
         mar = c(0, 0, 1, 0))    
####

