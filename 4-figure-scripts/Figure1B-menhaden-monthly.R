# Figure1B-menhaden-monthly.R
######################################
# Janelle L. Morano

# Figure 1B. Monthly Distribution of Menhaden

# last updated 16 April 2026
###############################################
###############################################

rm(list = ls())

library(tidyverse)
library(cowplot)
library(here)

dir <- here::here()

data <- read.csv(file = here(dir, "2-input-data", "combined-allsurveys-menhaden-data-20260414.csv"), header = TRUE)

pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")


# List of States to plot
state.levels <- c("MENH", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL")
states <- factor(unique(na.omit(data$State)), levels = state.levels)
states <- sort(states)

# Month labels
month.labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Create an empty list to store plots
fig <- list()

for (i in states) { 
   
  plot.data <- subset(data, State == i & MenhadenTotal > 0 & !is.na(Month) & !is.na(MenhadenTotal))
  
  if(nrow(plot.data) > 0) {

    p <- ggplot(data = plot.data, aes(x = Month, y = MenhadenTotal, group = Month)) + 
        stat_summary(fun = "mean",
                    geom = "bar",
                    width = 0.4,
                    fill = pal8[which(states == i)],
                    alpha = 0.8) + 
        stat_summary(fun.data = "mean_se",
                    geom = "errorbar",
                    color = pal8[which(states == i)],
                    linewidth = 0.8,
                    width = 0.2) + 
        coord_cartesian(xlim = c(0.5, 12.5), ylim = c(0, 300)) +
        scale_x_continuous(breaks = 1:12, labels = month.labels) +
        theme_classic() +
        theme(legend.position = "none",
                text = element_text(size = 12),
                axis.title = element_blank()) +
        annotate("text", x = 0.2, y = 300, label = i, fontface = "bold", hjust = 0, size = 3)
        
    # Label last plot with x-axis labels
    if (i == tail(states, 1)) {
    p <- p + theme(axis.text.x = element_text()) +
                labs(x = "Month")
    } else {
    p <- p + theme(axis.text.x = element_blank())
    }
    
    fig[[paste(i)]] <- p
  }
}



final.plot <- plot_grid(plotlist = fig, ncol = 1, align = "v")

# Add y-axis label using ggdraw()
y.label <- ggdraw() +
    draw_label("Menhaden Sample Abundance (Number, 1972-2023)", 
                angle = 90, 
                size = 12)

# 3. Use plot_grid again to put the label and the plot together
# We give the label a small 'rel_width' so it doesn't take up too much space
final.plot <- plot_grid(y.label, final.plot, ncol = 2, rel_widths = c(0.05, 1))

# Print the combined plot
print(final.plot)
#ggsave(file = here(dir, "5-figures", "Fig1B-monthly-menhaden.png"), width=6, height = 9)


# Proportion of samples with menhaden by survey
data |>
group_by(Survey) |>
summarise(n_samples  = n(),
    presence   = sum(Presence == 1),
    absence    = sum(Presence == 0),
    prop_found = presence / n_samples)


# Samples by depth
data |>
group_by(Survey) |>
summarise(across(
    c(Depth), 
    list(mean = \(x) mean(x, na.rm = TRUE),
        min = \(x) min(x, na.rm = TRUE),
        max = \(x) max(x, na.rm = TRUE))
    ))
