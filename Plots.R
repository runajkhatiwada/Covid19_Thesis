#rm(list=ls())

data.set <- read.csv("C:/Users/runajk/Documents/Study/Thesis/data.set.csv")

summary(data.set)
#data <- data.set[, -c(3,4,9)]

library(ggplot2)

plot.graph <- function (df_county, county) {
  y1 <- max(df_county$deaths.10000, na.rm = TRUE)
  y2 <- max(df_county$icu.case.10000, na.rm = TRUE)
  
  y_max <- ifelse(y1 < y2, y2, y1)
  y_max <- 1.25 * y_max
  
  ggplot(df_county, aes(x=Week.Number)) + theme_bw() + 
    theme(panel.grid.minor = element_line(colour="lightgray")) + 
    scale_x_continuous(minor_breaks = seq(1, 52, 1)) +
    geom_line(aes(y=Temperature*(y_max/25), color="Temperature"), show.legend = NA) +
    scale_y_continuous(sec.axis= sec_axis(~./(y_max/25), name="Temperature")) +
    geom_line(aes(y=deaths.10000, color="Death Cases/10000"), show.legend = NA) +
    geom_line(aes(y=icu.case.10000, color="ICU Cases/10000"), show.legend = NA) + 
    # geom_vline(xintercept = 9, linetype="dotted", color = "steelblue", size=1) +
    # annotate(geom='text', x=9, y=y_max, colour='steelblue', label='P1', size = 3) +
    # geom_vline(xintercept = 10, linetype="dotted", color = "steelblue", size=1) +
    # annotate(geom='text', x=10, y=y_max, colour='steelblue', label='P2', size = 3) +
    # geom_vline(xintercept = 11, linetype="dotted", color = "steelblue", size=1) +
    # annotate(geom='text', x=11, y=y_max, colour='steelblue', label='P3', size = 3) +
    geom_vline(xintercept = 12, linetype="dotted", color = "steelblue", size=1) +
    annotate(geom='text', x=12, y=y_max, colour='steelblue', label='P1', size = 3) +
    geom_vline(xintercept = 13, linetype="dotted", color = "steelblue", size=1) +
    annotate(geom='text', x=13, y=y_max, colour='steelblue', label='P2', size = 3) +
    geom_vline(xintercept = 14, linetype="dotted", color = "steelblue", size=1) +
    annotate(geom='text', x=14, y=y_max, colour='steelblue', label='P3', size = 3) +
    geom_vline(xintercept = 15, linetype="dotted", color = "steelblue", size=1) +
    annotate(geom='text', x=15, y=y_max, colour='steelblue', label='P4', size = 3) +
    geom_vline(xintercept = 16, linetype="dotted", color = "steelblue", size=1) +
    annotate(geom='text', x=16, y=y_max, colour='steelblue', label='P5', size = 3) +
    geom_vline(xintercept = 19, linetype="dotted", color = "steelblue", size=1) +
    annotate(geom='text', x=19, y=y_max, colour='steelblue', label='P6', size = 3) +
    # geom_vline(xintercept = 20, linetype="dotted", color = "steelblue", size=1) +
    # annotate(geom='text', x=20, y=y_max, colour='steelblue', label='P10', size = 3) +
    geom_vline(xintercept = 22, linetype="dotted", color = "steelblue", size=1) +
    annotate(geom='text', x=22, y=y_max, colour='steelblue', label='P7', size = 3) +
    geom_vline(xintercept = 23, linetype="dotted", color = "steelblue", size=1) +
    annotate(geom='text', x=23, y=y_max, colour='steelblue', label='P8', size = 3) +
    geom_vline(xintercept = 26, linetype="dotted", color = "steelblue", size=1) +
    annotate(geom='text', x=26, y=y_max, colour='steelblue', label='P9', size = 3) +
    # geom_vline(xintercept = 27, linetype="dotted", color = "steelblue", size=1) +
    # annotate(geom='text', x=27, y=y_max, colour='steelblue', label='P14', size = 3) +
    # geom_vline(xintercept = 28, linetype="dotted", color = "steelblue", size=1) +
    # annotate(geom='text', x=28, y=y_max, colour='steelblue', label='P15', size = 3) +
    # geom_vline(xintercept = 22, linetype="dotted", color = "steelblue", size=1) +
    # annotate(geom='text', x=22, y=y_max, colour='steelblue', label='P16', size = 3) +
    xlab("Weeks") + 
    ylab("Cases [ICU / Deaths]") +
    scale_colour_manual("",
                        breaks = c("Temperature", "ICU Cases/10000", "Death Cases/10000"),
                        values = c("Temperature"="black","ICU Cases/10000"="blue", "Death Cases/10000"="red"))+
    labs(title=county)
}


unique(data.set$Region) #List of all Swedish counties

county <- data.set[data.set$Region == "Sodermanland",]
plot.graph(county, "Sodermanland")
