# script created to generate Radar plot
# created 19/8/17

library(ggplot2)
library(ggradar)
library(reshape2)
library(ggthemes)
library(formattable)
library(htmltools)
library(webshot)


#setwd('/Users/Documents/AKCV')

# import data & subset 
df.radar <- read.csv("AK_metrics.csv")
df.radar <- df.radar[1:12,c(1:3)]

# drop unused factors and re-order
df.radar$Detail <- factor(df.radar$Detail)
df.radar$Detail <- factor(df.radar$Detail, levels = c("Statistics", "Linear Algebra", "Advertising","Marketing", "Communication","IT","Bash","Python","R","SQL","Sysadmin","Network Admin"))
# add colour column
df.radar$colour <- ifelse(df.radar$Area=="Mathematics", 'seagreen',ifelse(df.radar$Area == "Computer Science",'red','blue'))

# function and plot code cribbed from:
# http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

p1 <- ggplot(data=df.radar[order(df.radar$Detail),], aes(x=Detail, y=Value, group=1)) +
  geom_polygon(aes(group=1),size = 1,fill='royalblue',alpha=0.5) + 
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(margin = margin(t = 4),size = rel(1.5),colour=df.radar[order(df.radar$Detail),c("colour")]),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        panel.grid.major = element_line(colour = "lightblue")) +
  xlab("") + ylab("") + ylim(0,10) +
  scale_y_continuous(breaks = seq(0,10,by=2), limits=c(0,9)) +
  guides(color = guide_legend(ncol=2)) +
  coord_radar()
p1

#ggsave(filename="AKradarchart.png", p1, width=8, height=7)

# custom function to colour text in cell
sign_formatter <- formatter("span", 
                    style = x ~ style(color = ifelse(x == 'Mathematics', 'seagreen', 
                                                      ifelse(x == 'Computer Science', "red", "blue"))))

as.htmlwidget(formattable(df.radar[,1:3], list(
  Area = sign_formatter
)))


# custom export function to export htmlwidget formattable
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

ft <- formattable(df.radar[,1:3], list(
  Area = sign_formatter
))

export_formattable(ft,"ft.png")
