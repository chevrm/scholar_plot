## Load packages
library(scholar)
library(ggplot2)
library(dplyr)
library(gtable)
library(grid)
setwd("~/DEV/scholar_plot/")
## Import Google scholar info
scholarid <- 'VX3Laf8AAAAJ' # Low count example...me
#scholarid <- 'LrlwhwUAAAAJ' # High count example...Jonathan Eisen
#scholarid <- 'SfDzdgEAAAAJ' # Crazy count example...George Church

## Define how many years to include in plot
years_to_include <- 5

# Fetch scholar data, summarize, and combine
cits <- get_citation_history(scholarid) %>% filter(!is.na(year))
pubs <- get_publications(scholarid) %>% filter(!is.na(year)) %>% group_by(year) %>% summarize(pubs=n())
t <- data.frame(year=seq(min(cits$year, pubs$year), max(cits$year, pubs$year), 1))
cits <- left_join(t, cits, by='year')
cits$year <- as.factor(cits$year)

t <- data.frame(year=seq(min(pubs$year), max(pubs$year), 1))
pubs <- left_join(t, pubs, by='year')
pubs$year <- as.factor(pubs$year)
combo <- full_join(cits, pubs, by='year') %>% arrange(year)
combo[is.na(combo)] <- 0
combo <- combo %>% mutate(cumpubs=cumsum(pubs), cumcits=cumsum(cites)) %>% tail(years_to_include)

## Set plot params
pubcolor <- '#8EC3A7'
citcolor <- '#DC5356'
num_breaks <- 4

# Rounding function for yaxis upperbounds
roundUp <- function(x,to){
  to*(x%/%to + as.logical(x%%to))
}
10**floor(log10(max(combo$cumpubs)*1.05))

# Calculate yaxis upperbound by rounding the max+5% to the appropriate order of magnitude
pymax <- roundUp(max(combo$cumpubs)*1.05, 10**floor(log10(max(combo$cumpubs)*1.05)))

# Publication plot
pubplot <- ggplot(combo, aes(x=year, y=cumpubs)) +
  geom_bar(stat='identity', fill=pubcolor) +
  labs(x=NULL, y= NULL) +
  scale_y_continuous(limits=c(0,pymax), breaks=seq(0,pymax,pymax/num_breaks)) +
  theme(
    text = element_text(face='bold'),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "#AAAAAA", size = 0.5),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_text(color=pubcolor),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(color=pubcolor)
  )
#pubplot # Uncomment to check
# Calculate yaxis upperbound by rounding the max+5% to the appropriate order of magnitude
cymax <- roundUp(max(combo$cumcits)*1.05, 10**floor(log10(max(combo$cumcits)*1.05)))
# Citation plot
citplot <- ggplot(combo, aes(x=year, y=cumcits)) +
  geom_line(group=1, color=citcolor, size=1.5) +
  geom_point(pch=21,color='white',fill=citcolor, size=5) +
  labs(x=NULL, y= NULL) +
  scale_y_continuous(limits=c(0,cymax), breaks=seq(0,cymax,cymax/num_breaks)) +
  theme(
    text = element_text(face='bold'),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_text(color=citcolor),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(color=citcolor)
  )
#citplot # Uncomment to check, note there should be no major/minor gridlines here or they will plot OVER the publication bars

# Get the plot grobs
g1 <- ggplotGrob(pubplot)
g2 <- ggplotGrob(citplot)

# Get the locations of the plot panels in g1.
pp <- c(subset(g1$layout, name == "panel", se = t:r))

# Overlap panel for second plot on that of the first plot
g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)

#Swap margins and justifications
# Taken from the cowplot package:
# https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R 
hinvert_title_grob <- function(grob){
  # Swap the widths
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  # Fix the justification
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}

# Get the y axis from g2 (axis line, tick marks, and tick mark labels)
index <- which(g2$layout$name == "axis-l")  # Which grob
yaxis <- g2$grobs[[index]]                  # Extract the grob

# yaxis is a complex of grobs containing the axis line, the tick marks, and the tick mark labels.
# The relevant grobs are contained in axis$children:
#   axis$children[[1]] contains the axis line;
#   axis$children[[2]] contains the tick marks and tick mark labels.

# Swap tick marks and tick mark labels
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)

# Swap margins and fix justifications for the tick mark labels
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])

# Put ticks back into yaxis
yaxis$children[[2]] <- ticks

# Put the transformed yaxis on the right side of g1
g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
g1 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")

# Labels grob
cp <- paste0("Cumulative No. Publications: ", max(combo$cumpubs))
left = textGrob(cp, x = 0, y = 0.9, just = c("left", "top"), gp = gpar(col =  pubcolor))
cc <- paste0("Cumulative No. Citations: ", max(combo$cumcits))
right =  textGrob(cc, x = 1, y = 0.9, just = c("right", "top"), gp = gpar(col =  citcolor))
labs = gTree("Labs", children = gList(left, right))

# New row in the gtable for labels
height = unit(3, "grobheight", left)
g1 <- gtable_add_rows(g1, height, 2)  

# Put the label in the new row
g1 = gtable_add_grob(g1, labs, t=3, l=3, r=5)

# Turn off clipping in the plot panel
g1$layout[which(g1$layout$name == "panel"), ]$clip = "off"

# Print
#ggsave(paste0(scholarid,'.scholar_plot.pdf'), g1, width=5, height=5, dpi=600)
ggsave(paste0(scholarid,'.scholar_plot.png'), g1, width=5, height=5, dpi=600)