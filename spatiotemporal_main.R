library(tidyverse) #For plotting and data handling
library(glmmTMB) #For fitting the model
library(effects) #For calculating estimates and confidence intervals from the model

setwd(getwd()) #specify working directory
df <- read_csv("data.csv", col_types = "fifffiidf") #Read data and set correct data types into the columns

# Create data frames for drawing rectangles to represent months in the line plots
may <- data.frame(xmin=1, xmax=3.214, ymin=0.000001, ymax=Inf, label = "May") 
jun <- data.frame(xmin=3.214, xmax=5.357, ymin=0.000001, ymax=Inf, label = "Jun")
jul <- data.frame(xmin=5.357, xmax=7.571, ymin=0.000001, ymax=Inf, label = "Jul")
aug <- data.frame(xmin=7.571, xmax=9.786, ymin=0.000001, ymax=Inf, label = "Aug")
sep <- data.frame(xmin=9.786, xmax=11.929, ymin=0.000001, ymax=Inf, label = "Sep")
oct <- data.frame(xmin=11.929, xmax=Inf, ymin=0.000001, ymax=Inf, label = "Oct")



###### E. nilssonii ########

sub_data <- filter(df, taxa == "E. nilssonii") # Subset data with one taxa
fit <- glmmTMB(formula = cbind(succ, fail) ~  period + year + lat + lat*period + (1|station/place) + (1|mic),  # Fit glmmTMB-model
               data = sub_data, family = binomial(link = "logit"))
summary(fit) # Print model summary
summary(effall <- allEffects(fit, se=list(level=0.95))) # Print model estimates

#Create dataframe for visualizing estimates and confidence intervals
sum <- summary(effperiod <- Effect(c("period", "lat"),fit, se=list(level=0.95)))
est <- mutate(as_tibble(sum$effect), period = seq(1,12,1)) %>%
  pivot_longer(!period, names_to = "lat", values_to = "estimate")
upp <- mutate(as_tibble(sum$upper), period = seq(1,12,1)) %>%
  pivot_longer(!period, names_to = "lat", values_to = "upper")
low <- mutate(as_tibble(sum$lower), period = seq(1,12,1)) %>%
  pivot_longer(!period, names_to = "lat", values_to = "lower")
eff <- left_join(est, upp,  by = c("period", "lat")) %>%
  left_join(low,  by = c("period", "lat"))

# Create lat_offset variable to improve clarity of the graph
eff <- mutate(eff, lat = as.numeric(lat), lat_offset = NA) 
for (i in 1:nrow(eff)){
  eff$lat_offset[i] <- eff$lat[i]-59
  if (eff$lat_offset[i]== 4)
    eff$lat_offset[i] <- 3
  if (eff$lat_offset[i]== 6)
    eff$lat_offset[i] <- 4
  if (eff$lat_offset[i]== 7)
    eff$lat_offset[i] <- 5
}
eff <- mutate(eff, lat = as.character(lat))

# Visualize results as a line plot
ggplot(data = filter(eff), aes(period+lat_offset*0.1-0.3, estimate)) +
  geom_rect(data = may, aes(xmin=xmin-0.5, xmax=xmax-0.5, ymin=ymin, ymax=ymax), fill="black", alpha=0.05, inherit.aes = FALSE) +
  geom_rect(data = jul, aes(xmin=xmin-0.5, xmax=xmax-0.5, ymin=ymin, ymax=ymax), fill="black", alpha=0.05, inherit.aes = FALSE) +
  geom_rect(data = sep, aes(xmin=xmin-0.5, xmax=xmax-0.5, ymin=ymin, ymax=ymax), fill="black", alpha=0.05, inherit.aes = FALSE) +
  geom_line(aes(color = lat), size = 1.25) +
  geom_point(aes(color = lat), size = 3) +
  geom_errorbar(aes(ymax=upper, ymin=lower, colour = lat), size = 0.75, width=.1) +
  scale_x_continuous(name = "Period", breaks = seq(1,12,1)) +
  scale_y_continuous(name = "Activity of Eptesicus nilssonii", breaks = c(0.001, 0.02, 0.4), trans = "log") +
  scale_colour_manual(values = c("#d59c00", "#ffcd41", "#ffe68b", "#d583cd", "#83416a"), name = "Latitude °N") +
  theme_minimal(base_line_size = 0) + 
  theme(panel.grid.minor.x = element_line(color = "grey85", size = 1, linetype= "solid"),
        panel.grid.major.x = element_line(color = "black", size = 1, linetype = "blank"),
        panel.grid.minor.y = element_line(color = "white", size = 1, linetype = "blank"),
        panel.grid.major.y = element_line(color = "grey85", size = 1, linetype = "dotted"),
        legend.position = "none") +
  coord_cartesian(xlim = c(0.6, 12), ylim = c(0.0002, 1))


###### Myotis spp. ###########

sub_data <- filter(df, taxa == "Myotis sp.") # Subset data with one taxa
fit <- glmmTMB(formula = cbind(succ, fail) ~  period + year + lat + lat*period + (1|station/place), # Fit glmmTMB-model
               data = sub_data, binomial(link = "logit"))
summary(fit) # Print model summary
summary(effall <- allEffects(fit, se=list(level=0.95))) # Print model estimates

#Create dataframe for visualizing estimates
sum <- summary(effperiod <- Effect(c("period", "lat"),fit, se=list(level=0.95)))
est <- mutate(as_tibble(sum$effect), period = seq(1,12,1)) %>%
  pivot_longer(!period, names_to = "lat", values_to = "estimate")
upp <- mutate(as_tibble(sum$upper), period = seq(1,12,1)) %>%
  pivot_longer(!period, names_to = "lat", values_to = "upper")
low <- mutate(as_tibble(sum$lower), period = seq(1,12,1)) %>%
  pivot_longer(!period, names_to = "lat", values_to = "lower")
eff <- left_join(est, upp,  by = c("period", "lat"))
eff <- left_join(eff, low,  by = c("period", "lat"))

# Create lat_offset variable to improve clarity in the graph
eff <- mutate(eff, lat = as.numeric(lat), lat_offset = NA)
for (i in 1:nrow(eff)){
  eff$lat_offset[i] <- eff$lat[i]-59
  if (eff$lat_offset[i]== 4)
    eff$lat_offset[i] <- 3
  if (eff$lat_offset[i]== 6)
    eff$lat_offset[i] <- 4
  if (eff$lat_offset[i]== 7)
    eff$lat_offset[i] <- 5
}
eff <- mutate(eff, lat = as.character(lat))

# Visualize results as a line plot
ggplot(data = filter(eff), aes(period+lat_offset*0.1-0.3, estimate)) +
  geom_rect(data = may, aes(xmin=xmin-0.5, xmax=xmax-0.5, ymin=ymin, ymax=ymax), fill="black", alpha=0.05, inherit.aes = FALSE) +
  geom_rect(data = jul, aes(xmin=xmin-0.5, xmax=xmax-0.5, ymin=ymin, ymax=ymax), fill="black", alpha=0.05, inherit.aes = FALSE) +
  geom_rect(data = sep, aes(xmin=xmin-0.5, xmax=xmax-0.5, ymin=ymin, ymax=ymax), fill="black", alpha=0.05, inherit.aes = FALSE) +
  geom_line(aes(color = lat), size = 1.25) +
  #geom_line(aes(y=336), linetype = "dashed")+
  geom_point(aes(color = lat), size = 3) +
  geom_errorbar(aes(ymax=upper, ymin=lower, colour = lat), size = 0.75, width=.1) +
  scale_x_continuous(name = "Period", breaks = seq(1,12,1)) +
  scale_y_continuous(name = "Activity of Myotis spp.", breaks = c(0.00004, 0.001, 0.02, 0.4), labels = c(0.00004, 0.001, 0.02, 0.4), trans = "log") +
  scale_colour_manual(values = c("#d59c00", "#ffcd41", "#ffe68b", "#d583cd", "#83416a"), name = "Latitude °N") + 
  theme_minimal(base_line_size = 0) + 
  theme(panel.grid.minor.x = element_line(color = "grey85", size = 1, linetype= "solid"),
        panel.grid.major.x = element_line(color = "black", size = 1, linetype = "blank"),
        panel.grid.minor.y = element_line(color = "white", size = 1, linetype = "blank"),
        panel.grid.major.y = element_line(color = "grey85", size = 1, linetype = "dotted"),
        legend.position = "bottom") +
  coord_cartesian(xlim = c(0.6, 12), ylim = c(0.0000019, 1))



###### P. nathusii ########

sub_data <- filter(df, taxa == "P. nathusii", station == "Huso" | station == "Tvarminne" | station == "Seili") # Subset data with one taxa and only the southernmost stations
fit <- glmmTMB(formula = cbind(succ, fail) ~ period + year+ period*year  + (1|place) + (1|mic), # Fit glmmTMB model
               data = sub_data, family = binomial(link = "logit"))
fit <- update(fit, control=glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3))) # Add iterations to reach convergence
summary(fit) # Print model summary
summary(effall <- allEffects(fit, se=list(level=0.95))) # Print model estimates

#Create dataframe for visualizing estimates
sum <- summary(effperiod <- Effect(c("period", "year"),fit, se=list(level=0.95)))
est <- mutate(as_tibble(sum$effect), period = seq(1,12,1)) %>%
  pivot_longer(!period, names_to = "year", values_to = "estimate")
upp <- mutate(as_tibble(sum$upper), period = seq(1,12,1)) %>%
  pivot_longer(!period, names_to = "year", values_to = "upper")
low <- mutate(as_tibble(sum$lower), period = seq(1,12,1)) %>%
  pivot_longer(!period, names_to = "year", values_to = "lower")
eff <- left_join(est, upp,  by = c("period", "year"))
eff <- left_join(eff, low,  by = c("period", "year"))

# Create lat_offset variable to improve clarity in the graph
eff <- mutate(eff, year = as.numeric(year), year_offset =NA)
for (i in 1:nrow(eff)){
  eff$year_offset[i] <- eff$year[i]-2014
  if (eff$year_offset[i]== 4)
    eff$year_offset[i] <- 3
  if (eff$year_offset[i]== 6)
    eff$year_offset[i] <- 4
  if (eff$year_offset[i]== 7)
    eff$year_offset[i] <- 5
}
eff <- mutate(eff, year = as.character(year))

# Visualize results as a line plot
ggplot(data = filter(eff), aes(period+year_offset*0.1-0.3, estimate)) +
  geom_rect(data = may, aes(xmin=xmin-0.5, xmax=xmax-0.5, ymin=ymin, ymax=ymax), fill="black", alpha=0.05, inherit.aes = FALSE) +
  geom_rect(data = jul, aes(xmin=xmin-0.5, xmax=xmax-0.5, ymin=ymin, ymax=ymax), fill="black", alpha=0.05, inherit.aes = FALSE) +
  geom_rect(data = sep, aes(xmin=xmin-0.5, xmax=xmax-0.5, ymin=ymin, ymax=ymax), fill="black", alpha=0.05, inherit.aes = FALSE) +
  geom_line(aes(color = year), size = 1.25) +
  geom_point(aes(color = year), size = 3) +
  geom_errorbar(aes(ymax=upper, ymin=lower, colour = year), size = 0.75, width=.1) +
  scale_x_continuous(name = "Period", breaks = seq(1,12,1)) +
  scale_y_continuous(name = "Activity of Pipistrellus nathusii", breaks = c(0.00004, 0.001, 0.02, 0.4), trans = "log") +
  scale_colour_manual(values = c("#083962", "#2062ac", "#5a8bcd", "#f6d59c", "#d5ac4a"), name = "Year") + 
  theme_minimal(base_line_size = 0) + 
  theme(panel.grid.minor.x = element_line(color = "grey85", size = 1, linetype= "solid"),
        panel.grid.major.x = element_line(color = "black", size = 1, linetype = "blank"),
        panel.grid.minor.y = element_line(color = "white", size = 1, linetype = "blank"),
        panel.grid.major.y = element_line(color = "grey85", size = 1, linetype = "dotted"),
        legend.position = "bottom") +
  coord_cartesian(xlim = c(0.6, 12), ylim = c(0.0008, 0.45))