# FigureSupp-Presence-by-State.R
######################################
# Janelle L. Morano

# Supplemental Fig 3. Presence of menhaden in samples and average across states 
# Supplemental Fig 4. Year round presence of menhaden in state samples


# last updated 28 April 2025
###############################################
###############################################

library(tidyverse)
library(cowplot)

# Read full data 1972-2023 
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")

ave.presence.sp <- data.list[[1]] |>
  group_by(State, Year) |>
  summarise(ave.pres = mean(Presence))

ave.presence.fa <- data.list[[2]] |>
  group_by(State, Year) |>
  summarise(ave.pres = mean(Presence))


# List of States to plot
states <- c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL")
states <- factor(states, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

# Color palette for North to South States AKA Dark to Light
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal8lite <- adjustcolor(pal8, alpha.f = 0.2) #make transparent


# Create an empty list to store plots
fig.comb <- list()


# Spring
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig.comb[[paste("Spring -", i)]] <- ggplot() + 
    # Observations, Presence data
    geom_point(data = subset(data.list[[1]], State ==i),
               aes(x = Year, y = Presence),
               position = position_jitter(width = 0.2, height = 0.2),
               color = "gray60", size = 0.5) +
    # Observations, Model fit
    geom_line(data = subset(ave.presence.sp, State ==i),
              aes(x = Year, y = ave.pres, group = State),
              color = "red", linewidth = 0.8) +

    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    ylim(c(0, 1)) +
    # xlim(c(2018, 2023)) +
    theme(
      # axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
  ggtitle(paste("Spring -", i))  # Title with state name
}


# Fall
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig.comb[[paste("Fall -", i)]] <- ggplot() + 
    # Observations, Presence data
    geom_point(data = subset(data.list[[2]], State ==i),
               aes(x = Year, y = Presence),
               position = position_jitter(width = 0.2, height = 0.2), 
               color = "gray60", size = 0.5) +
    # Observations, Average fit
    geom_line(data = subset(ave.presence.fa, State ==i),
              aes(x = Year, y = ave.pres, group = State),
              color = "red", linewidth = 0.8) +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    ylim(c(0, 1)) +
    # xlim(c(2018, 2023)) +
    theme(
      # axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
  ggtitle(paste("Fall -", i))  # Title with state name
}

fig.comb.plot <- plot_grid(plotlist = fig.comb, ncol = 2, byrow = FALSE)
# Print the combined plot
print(fig.comb.plot)
#ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figures/SupFig3.avepresence-by-state.png", width=9, height = 9)




###############################################
# Year-round
###############################################
statedata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20250402.csv", header = TRUE)

# Remove any samples without menhaden
statedata <- statedata %>%
  filter_at(vars(MenhadenTotal, Presence), all_vars(!is.na(.)))

statedata$Date <- as.Date(paste(statedata$Year, statedata$Month, "01", sep = "-"))

ave.state.pres <- statedata |>
  group_by(State, Date) |>
  summarise(ave.pres = mean(Presence))


# List of States to plot
contstates <- factor(c("RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"),
                     levels = c("RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

fig.cont <- list()
for (i in contstates) {
  fig.cont[[paste(i)]] <- ggplot() + 
    # Observations, Presence data
    geom_point(data = subset(statedata, State ==i),
               aes(x = Date, y = Presence),
               position = position_jitter(width = 0.2, height = 0.2), 
               color = "gray60", size = 0.5) +
    # Observations, average
    geom_line(data = subset(ave.state.pres, State ==i),
              aes(x = Date, y = ave.pres, group = State),
              color = "red", linewidth = 0.8) +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    ylim(c(0, 1)) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    theme(
      # axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    ggtitle(paste(i))  # Title with state name
}

fig.cont.plot <- plot_grid(plotlist = fig.cont, ncol = 1)
print(fig.cont.plot)


###############################################
# By month
###############################################
ave.month <- statedata |>
  group_by(State, Month) |>
  summarise(ave.pres = mean(Presence, na.rm = TRUE),
            se = sd(Presence, na.rm = TRUE) / sqrt(n()))

# Fill in months in States without samples with NA
all.months <- c(1:12)
ave.month <- ave.month |>
  ungroup() |>
  complete(State, Month = all.months)

pal6 <- c("#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal6lite <- adjustcolor(pal6, alpha.f = 0.2) #make transparent


fig.month <- list()
for (i in seq_along(contstates)) {
  state_name <- contstates[i]
  color <- pal6[(i - 1) %% length(pal6) + 1]  # Cycle through palette if more states than colors
  colfill <- pal6lite[(i - 1) %% length(pal6lite) + 1]  # Cycle through palette if more states than colors
  
  fig.month[[state_name]] <- ggplot() + 
    # Observations, Presence data
    geom_point(data = subset(statedata, State == state_name),
               aes(x = Month, y = Presence),
               position = position_jitter(width = 0.4, height = 0.2), 
               color = "gray90", size = 0.5) +
    # Observations, average
    geom_ribbon(data = subset(ave.month, State == state_name), 
                aes(x = Month, 
                    ymin = pmax(0, ave.pres - (1.96 * se)), 
                    ymax = pmin(1, ave.pres + (1.96 * se)), 
                    group = State),
                fill = colfill) +  
    geom_line(data = subset(ave.month, State == state_name),
              aes(x = Month, y = ave.pres, group = State),
              color = color, linewidth = 1.2) +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    scale_y_continuous(breaks = 0:1, labels = c(0,1)) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    ggtitle(state_name)
}

fig.month.plot <- plot_grid(plotlist = fig.month, ncol = 1)
print(fig.month.plot)

ggsave(file = "/Users/janellemorano/Git/menhaden-dist-ms/figures/SupFig4.year-round-avepresence-by-state.png", dpi = 400, width=9, height = 9)
