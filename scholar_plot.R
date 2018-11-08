## Load packages
library(ggplot2)
library(dplyr)
library(gtable)
library(grid)
library(httr)
library(stringr)
library(xml2)
library(rvest)
library(R.cache)

setwd("~/DEV/scholar_plot/")


## Set scholar functions - based on library(scholar)
tidy_id <- function(id) {
  id <- scholarid
  if (length(id)!=1) {
    id <- id[1]
    msg <- sprintf("Only one ID at a time; retrieving %s", id)
    warning(msg)
  }
  return(id)
}
get_profile <- function(id) {
  
  id <- tidy_id(id)
  
  url_template <- "http://scholar.google.com/citations?hl=en&user=%s"
  url <- sprintf(url_template, id)
  
  ## Generate a list of all the tables identified by the scholar ID
  page <- GET(url, handle=getOption("scholar_handle")) %>% read_html()
  tables <- page %>% html_table()
  
  ## The citation stats are in tables[[1]]$tables$stats
  ## but the number of rows seems to vary by OS
  stats <- tables[[1]]
  rows <- nrow(stats)
  
  ## The personal info is in
  name <- page %>% html_nodes(xpath="//*/div[@id='gsc_prf_in']") %>% html_text()
  bio_info <- page %>% html_nodes(xpath="//*/div[@class='gsc_prf_il']") %>% html_text()
  affiliation <- bio_info[1]
  
  ## Specialities (trim out HTML non-breaking space)
  specs <- iconv(bio_info[2], from="UTF8", to="ASCII")
  specs <- str_trim(tolower(str_split(specs, ",")[[1]]))
  
  ## Extract the homepage
  homepage <- page %>% html_nodes(xpath="//*/div[@id='gsc_prf_ivh']//a/@href") %>% html_text() 
  
  return(list(id=id, name=name, affiliation=affiliation,
              total_cites=as.numeric(as.character(stats[rows-2,2])),
              h_index=as.numeric(as.character(stats[rows-1,2])),
              i10_index=as.numeric(as.character(stats[rows,2])),
              fields=specs,
              homepage=homepage))
}
get_citation_history <- function(id) {
  
  ## Ensure only one ID
  id <- tidy_id(id)
  
  ## Read the page and parse the key data
  url_template <- "http://scholar.google.com/citations?hl=en&user=%s&pagesize=100&view_op=list_works"
  url <- sprintf(url_template, id)
  
  ## A better way would actually be to read out the plot of citations
  page <- GET(url, handle=getOption("scholar_handle")) %>% read_html()
  years <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_t']") %>%
    html_text() %>% as.numeric()
  vals <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_al']") %>%
    html_text() %>% as.numeric()
  
  df <- data.frame(year=years, cites=vals)
  
  return(df)
}
get_publications <- function(id, cstart = 0, pagesize=100, flush=FALSE) {
  
  ## Ensure we're only getting one scholar's publications
  id <- tidy_id(id)
  
  ## Define the cache path 
  cache.dir <- file.path(tempdir(), "r-scholar")
  setCacheRootPath(cache.dir)
  
  ## Clear the cache if requested
  if (flush) saveCache(NULL, key=list(id, cstart))
  
  ## Check if we've cached it already
  data <- loadCache(list(id, cstart))
  
  ## If not, get the data and save it to cache
  if (is.null(data)) {
    
    ## Build the URL
    url_template <- "http://scholar.google.com/citations?hl=en&user=%s&cstart=%d&pagesize=%d"
    url <- sprintf(url_template, id, cstart, pagesize)
    
    ## Load the page
    page <- GET(url, handle=getOption("scholar_handle")) %>% read_html()
    cites <- page %>% html_nodes(xpath="//tr[@class='gsc_a_tr']") 
    
    title <- cites %>% html_nodes(".gsc_a_at") %>% html_text()
    pubid <- cites %>% html_nodes(".gsc_a_at") %>%
      html_attr("href") %>% str_extract(":.*$") %>% str_sub(start=2)
    doc_id <- cites %>% html_nodes(".gsc_a_ac") %>% html_attr("href") %>%
      str_extract("cites=.*$") %>% str_sub(start=7)
    cited_by <- suppressWarnings(cites %>% html_nodes(".gsc_a_ac") %>%
                                   html_text() %>%
                                   as.numeric(.) %>% replace(is.na(.), 0))
    year <- cites %>% html_nodes(".gsc_a_y") %>% html_text() %>%
      as.numeric()
    authors <- cites %>% html_nodes("td .gs_gray") %>% html_text() %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      filter(row_number() %% 2 == 1)  %>% .[[1]]
    
    ## Get the more complicated parts
    details <- cites %>% html_nodes("td .gs_gray") %>% html_text() %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      filter(row_number() %% 2 == 0) %>% .[[1]]
    
    
    ## Clean up the journal titles (assume there are no numbers in
    ## the journal title)
    first_digit <- as.numeric(regexpr("[\\[\\(]?\\d", details)) - 1
    journal <- str_trim(str_sub(details, end=first_digit)) %>%
      str_replace(",$", "")
    
    ## Clean up the numbers part
    numbers <- str_sub(details, start=first_digit) %>%
      str_trim() %>% str_sub(end=-5) %>% str_trim() %>% str_replace(",$", "")
    
    ## Put it all together
    data <- data.frame(title=title,
                       author=authors,
                       journal=journal,
                       number=numbers,
                       cites=cited_by,
                       year=year,
                       cid=doc_id,
                       pubid=pubid)
    ## Check if we've reached pagesize articles. Might need
    ## to search the next page
    if (nrow(data) > 0 && nrow(data)==pagesize) {
      data <- rbind(data, get_publications(id, cstart=cstart+pagesize, pagesize=pagesize))
    }
    
    ## Save it after everything has been retrieved.
    if (cstart == 0) {
      saveCache(data, key=list(id, cstart))
    }
  }
  
  return(data)
}
get_article_cite_history <- function (id, article) {
  
  id <- tidy_id(id)
  url_base <- paste0("http://scholar.google.com/citations?", 
                     "view_op=view_citation&hl=en&citation_for_view=")
  url_tail <- paste(id, article, sep=":")
  url <- paste0(url_base, url_tail)
  
  doc <- GET(url, handle=getOption("scholar_handle")) %>% read_html()
  
  ## Inspect the bar chart to retrieve the citation values and years
  years <- doc %>%
    html_nodes(xpath="//*/div[@id='gsc_graph_bars']/a") %>%
    html_attr("href") %>%
    str_replace(".*as_yhi=(.*)$", "\\1") %>%
    as.numeric()
  vals <- doc %>%
    html_nodes(xpath="//*/span[@class='gsc_g_al']") %>%
    html_text() %>%
    as.numeric()
  
  df <- data.frame(year = years, cites = vals)
  
  ## There may be undefined years in the sequence so fill in these gaps
  tmp <- merge(data.frame(year=min(years):max(years)),
               df, all.x=TRUE)
  tmp[is.na(tmp)] <- 0
  
  tmp$pubid <- article
  return(tmp)
}
get_num_articles <- function(id) {  
  papers <- get_publications(id)
  return(nrow(papers))
}
get_oldest_article <- function(id) {
  papers <- get_publications(id)
  return(min(papers$year, na.rm=TRUE))
}
get_num_distinct_journals <- function(id) {
  id <- tidy_id(id)
  papers <- get_publications(id)
  return(length(unique(papers$journal)))
}
get_num_top_journals <- function(id, journals) {
  id <- tidy_id(id)
  papers <- get_publications(id)
  
  if (missing(journals)) {
    journals <-c("Nature", "Science","Proceedings of the National Academy of Sciences")
  }
  
  return(length(which(is.element(papers$journal, journals))))
}

## Import Google scholar info
scholarid <- 'VX3Laf8AAAAJ' # example...me
#scholarid <- 'LrlwhwUAAAAJ' # High count example...Jonathan Eisen
#scholarid <- 'SfDzdgEAAAAJ' # Crazy count example...George Church

## Define how many years to include in plot
years_to_include <- 5
current_year <- as.numeric(paste0("20", strftime(Sys.time(), format = "%y")))
doy <- as.numeric(strftime(Sys.time(), format = "%j"))

#
lastname <- sub(".+\\s(.+)$", "\\1", get_profile(scholarid)$name)
allpubs <- get_publications(scholarid) %>% filter(!is.na(year))
allpubs$first <- sub("^\\w+\\s(\\S+),.+", "\\1", allpubs$author)
allpubs$isfirst <- 'N'
allpubs[allpubs$first==lastname,'isfirst'] <- 'Y'

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

## Projected
combo$proj_c <- combo$cites
combo[combo$year==current_year,]$proj_c <- (cits[cits$year==current_year,]$cites /doy)*365
combo <- combo %>% mutate(proj_cumcits=cumsum(proj_c))

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
cymax <- roundUp(max(combo$proj_cumcits)*1.05, 10**floor(log10(max(combo$proj_cumcits)*1.05)))
maxyear <- as.character(max(as.numeric(as.character(combo$year))))
maxyearcit <- combo[combo['year']==maxyear,]$cumcits
projmaxyear <- combo[combo['year']==maxyear,]$proj_cumcits
# Citation plot
citplot <- ggplot(combo, aes(x=year, y=cumcits)) +
  geom_segment(aes(x=maxyear, xend=maxyear, y=maxyearcit, yend=projmaxyear), color='grey50', size=1.5) +
  geom_point(aes(y=proj_cumcits), pch=21,color='white',fill='grey50', size=5) +
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