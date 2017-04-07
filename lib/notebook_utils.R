plotMPG <- function(){
  library(tidyverse,ggplot2)
  ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color = class)) + 
    geom_smooth(se = FALSE) +
    labs(
      title = paste(
        "Fuel efficiency generally decreases with engine size"
      ),
      subtitle = paste(
        "Two seaters (sports cars) are an exception because of their light weight"
      ),
      caption = "Data from fueleconomy.gov"
    )  
}