################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Model Analysis
# Author: Josh Wilson
# Date: 13-06-2022
# References:
# https://cran.r-project.org/web/packages/mgcv/mgcv.pdf
# https://adibender.github.io/pammtools/

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Specify required packages
packages <- c("readr", "dplyr", "ggplot2", "mgcv", "gratia")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
    package_consent <- readline(
        prompt <- (paste("Install", new_packages, " y/n?\n")))
    if (tolower(package_consent) == "y") {
        install.packages(new_packages)
        }
    else print(paste("This code cannot be run without", new_packages))
}

# load packages, install if not available provided user approves
lapply(packages, require, character.only = TRUE)

# import data
data_ped <- read_csv("data/dibd_ped_data.csv") %>%
    mutate(flock = as.factor(flock))

#########################
#### Analysis of fit ####
#########################
# load model and print summary
fit <- readRDS("models/dibd-model-27-06-22_10-19.rds")
summary(fit)

# smooth terms
plot_smooth_term <- function(smooth) {
    data <- smooth_estimates(fit, smooth = smooth)
    term <- str_sub(smooth, 3, -2)
    plot <- ggplot(data, aes(x = !!sym(term))) +
        geom_line(aes(y = est)) +
        geom_ribbon(aes(ymin = est - se, ymax = est + se), alpha = 0.2) +
        coord_cartesian(ylim = c(-10, 10)) +
        ylab("Effect") +
        geom_rug(data = data_ped, aes(x = !!sym(term))) +
        theme_bw() +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0))

    if (term == "distance_x") plot <- plot + xlab("Distance [m]")
    if (term == "distance_z") plot <- plot + xlab("Altitude [m]")
    if (term == "velocity_x") plot <- plot + xlab("Approach Velocity [m/s]")
    if (term == "velocity_y") plot <- plot + xlab("Transverse Velocity [m/s]")
    if (term == "velocity_z") plot <- plot + xlab("Ascent Velocity [m/s]")
    if (term == "acceleration") plot <- plot + xlab("Acceleration [m/s/s]")
    if (term == "count") plot <- plot + xlab("Count")
    if (term == "tend") plot <- plot + xlab("Time Since Launch [s]")
    if (term == "wind_speed") plot <- plot + xlab("Wind Speed [m/s]")
    if (term == "cloud_cover") plot <- plot + xlab("Cloud Cover [%]")
    if (term == "high_tide") plot <- plot + xlab("Time From High Tide [hr]")
    if (term == "temperature") plot <- plot + xlab("Temperature (\u00B0C)")

    title <- paste0("plots/", term, ".png")
    height <- 3
    width <- 3
    ggsave(title, plot, height = height, width = width)
}

mapply(plot_smooth_term, smooths(fit)[smooths(fit) != "s(flock)"])

# factors
plot_factor_term <- function(term) {
    newdata <- sample_info(group_by(data_ped, !!sym(term)))
    prediction <- predict(
        fit,
        newdata = newdata,
        type = "terms",
        terms = term,
        se.fit = TRUE)
    newdata <- cbind(newdata, prediction) %>%
        mutate(
            specification = str_to_title(specification),
            obscuring = str_to_title(obscuring),
            location = case_when(
                location == "toorbul" ~ "Toorbul",
                location == "geoff skinner wetland" ~ "Wellington Point",
                location == "queens esplanade" ~ "Thorneside"))

    plot <- ggplot(data = newdata, aes(x = .data[[term]], y = fit)) +
        geom_pointrange(
            aes(
                ymin = fit - (1.96 * se.fit),
                ymax = fit + (1.96 * se.fit))) +
        coord_cartesian(ylim = c(-10, 10)) +
        ylab("Effect") +
        theme_bw() +
        scale_y_continuous(expand = c(0, 0)) +
        theme(
            axis.title.x = element_blank(),
            axis.text.x = element_text(
                angle = 90,
                vjust = 0.5,
                hjust = 0.95))

    title <- paste0("plots/", term, ".png")
    height <- 3
    width <- 3
    ggsave(title, plot, height = height, width = width)
}
factors <- c("specification", "obscuring", "location")
mapply(plot_factor_term, factors)

# random effects
plot <- draw(fit, select = "s(flock)") +
    coord_cartesian(ylim = c(-10, 10)) +
    ylab("Effect") +
    theme_bw() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/flock.png", plot, height = 3, width = 3)
