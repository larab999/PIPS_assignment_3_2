# Assignment 3.2 - 2023 Programming in Psychological Science
#
# Date            Programmer              Descriptions of Change
# ====         ================           ======================
# 27-Jan-23    Lara                       Original code

# Q3.2.1
# This function gives a list of ingredients
remind_me <- function() {
  return(
    list(
      "quinoa",
      "broth",
      "spinach",
      "onion",
      "garlic",
      "curry",
      "champignons",
      "pepper",
      "chili",
      "coconut milk"
    )
  )
}

remind_me()

# This function gives solutions to PIPS assignments 3.1.6, 3.1.10 or 3.1.12 
cheat <- function(x) { 
  if (x == "Q3.1.10") {
    library(plotly)
    human_body_measurements <-
      read.csv(
        "https://raw.githubusercontent.com/hannesrosenbusch/schiphol_class/master/Body%20Measurements%20_%20original_CSV.csv"
      )
    plot_ly(
      human_body_measurements,
      x = ~ LegLength,
      y = ~ TotalHeight,
      z = ~ ShoulderToWaist,
      type = "scatter3d",
      mode = "markers",
      opacity = 0.5
    )
  } else if (x == "Q3.1.6") {
    data(ChickWeight)
    max_weight_all <-
      aggregate(weight ~ Chick, data = ChickWeight, FUN = max)
    max_weight <-
      max_weight_all[max_weight_all$Chick %in% c(1, 20, 3, 40, 5),]
    max_weight$Chick <-
      factor(max_weight$Chick, levels = c(1, 20, 3, 40, 5))
    ggplot(max_weight, aes(x = Chick, y = weight)) +
      geom_bar(stat = "identity") +
      xlab("chick") +
      ylab("max_weight")
  } else if (x == "Q3.1.12") {
    library(quantmod)
    library(PerformanceAnalytics)
    getSymbols("OXY",
               src = "yahoo",
               from = "2022-01-01",
               to = "2022-12-31")
    chartSeries(OXY)
    print("OXY: hydrocarbon exploration, petrochemical manufacturing")
  } else {
    return("Invalid argument. Please use 'Q3.1.6' or 'Q3.1.10' or 'Q3.1.12'.")
  }
}

cheat()

# Q3.2.2
library(ggplot2)

# This function generates random art
make_art <- function(seed = NULL) {
  # The seed for reproducibility
  if (!is.null(seed))
    set.seed(seed)
  
  # We create a plot
  ggplot() +
    # We draw random shapes of random sizes and colors
    geom_point(
      aes(x = runif(50), y = runif(50)),
      shape = 21,
      size = runif(50, 2, 5),
      fill = sample(colors(), 50)
    ) +
    geom_line(aes(
      x = runif(50),
      y = runif(50),
      xend = runif(50),
      yend = runif(50),
    ),
    size = runif(50, 0.5, 2)) +
    geom_segment(aes(
      x = runif(50),
      y = runif(50),
      xend = runif(50),
      yend = runif(50)
    ),
    size = runif(50, 0.5, 2))
}

# Calling the function
make_art()