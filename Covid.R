# Load necessary R packages 
library(ggplot2) 
library(dplyr) 
library(tidyr) 
# Import the data 
data <- read.csv("covid_19_clean_complete.csv") 
data$Date <- as.Date(data$Date, format="%Y-%m-%d") 
# Prepare data summaries 
global_trend <- data %>% 
group_by(Date) %>% 
summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered)) 
latest_data <- data %>% 
f
 ilter(Date == max(Date)) 
top_countries <- latest_data %>% 
group_by(Country.Region) %>% 
summarise(Confirmed = sum(Confirmed)) %>% 
arrange(desc(Confirmed)) %>% 
head(10) 
top_countries_deaths <- latest_data %>% 
group_by(Country.Region) %>% 
summarise(Deaths = sum(Deaths)) %>% 
arrange(desc(Deaths)) %>% 
head(10) 
who_region_data <- latest_data %>% 
  group_by(WHO.Region) %>% 
  summarise(Confirmed = sum(Confirmed)) 
 
# Pie chart for WHO region distribution 
who_region_distribution <- latest_data %>% 
  group_by(WHO.Region) %>% 
  summarise(Count = n()) 
 
# Faceted line chart for WHO region trends 
who_region_trend <- data %>% 
  group_by(WHO.Region, Date) %>% 
  summarise(Confirmed = sum(Confirmed), 
            Deaths = sum(Deaths), 
            Recovered = sum(Recovered)) 
 
who_region_trend_long <- who_region_trend %>% 
  pivot_longer(cols = c(Confirmed, Deaths, Recovered), 
               names_to = "Category", 
               values_to = "Count") 
 
# Generate and save each plot 
ggsave("global_trend.png", ggplot(global_trend, aes(x = Date)) + 
         geom_line(aes(y = Confirmed, color = "Confirmed Cases")) + 
         geom_line(aes(y = Deaths, color = "Deaths")) + 
         geom_line(aes(y = Recovered, color = "Recovered")) + 
         labs(title = "Global COVID-19 Trends Over Time", x = "Date", y = "Number of Cases") + 
         scale_color_manual(values = c("blue", "red", "green")) + 
         theme_minimal(), width = 8, height = 6) 
 
ggsave("top_countries.png", ggplot(top_countries, aes(x = reorder(Country.Region, -Confirmed), y = 
Confirmed)) + 
         geom_bar(stat = "identity", fill = "orange") + 
         labs(title = "Top 10 Countries by Confirmed Cases", x = "Country/Region", y = "Number of 
Confirmed Cases") + 
         theme_minimal() + 
         coord_flip(), width = 8, height = 6) 
 
ggsave("who_region.png", ggplot(who_region_data, aes(x = reorder(WHO.Region, -Confirmed), y = 
Confirmed)) + 
         geom_bar(stat = "identity", fill = "purple") + 
         labs(title = "Confirmed Cases by WHO Region", x = "WHO Region", y = "Number of Confirmed 
Cases") + 
         theme_minimal() + 
         coord_flip(), width = 8, height = 6) 
 
# Plot for top 10 countries by confirmed deaths 
ggsave("top_countries_deaths.png", ggplot(top_countries_deaths, aes(x = reorder(Country.Region, 
Deaths), y = Deaths)) + 
         geom_bar(stat = "identity", fill = "red") + 
         labs(title = "Top 10 Countries by Number of Confirmed Deaths",  
              x = "Country/Region",  
              y = "Number of Confirmed Deaths") + 
         theme_minimal() + 
         coord_flip(), width = 8, height = 6) 
 
# Pie chart for WHO region distribution 
ggsave("who_region_distribution.png", ggplot(who_region_distribution, aes(x = "", y = Count, fill = 
WHO.Region)) + 
         geom_bar(width = 1, stat = "identity") + 
         coord_polar("y", start = 0) + 
         labs(title = "Distribution of Cases by WHO Region") + 
         theme_minimal() + 
         theme(axis.title.x = element_blank(), 
               axis.title.y = element_blank(), 
               panel.grid = element_blank(), 
               axis.text.x = element_blank(), 
               axis.ticks = element_blank()) + 
         geom_text(aes(label = scales::percent(Count / sum(Count))),  
                   position = position_stack(vjust = 0.5)), width = 8, height = 6) 
 
# Faceted line chart for trends by WHO region 
ggsave("who_region_trend.png", ggplot(who_region_trend_long, aes(x = Date, y = Count, color = 
Category)) + 
         geom_line() + 
         facet_wrap(~WHO.Region, scales = "free_y") + 
         labs(title = "COVID-19 Trends by WHO Region", x = "Date", y = "Number of Cases", color = 
"Category") + 
         theme_minimal() + 
         theme(legend.position = "bottom"), width = 8, height = 6)
