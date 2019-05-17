# Common Plots:
#   A set of ggplots.
# Author: Samantha Piatt

# Imports
packages <- c("ggplot2", "dplyr", "cowplot", "reshape")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())),
                   repos = "http://cran.us.r-project.org")  
}
require(ggplot2)
require(cowplot)
require(reshape)
require(dplyr)


# Custom plot colors
plot_theme = theme_minimal()
plot_colors.Fruit = c("#591d91", "#9F167D", "#A2CD1C", "#D7CE1E", "#5393CC", "#808080")
plot_colors.Basic <- c("#242858","#88C448","#3299BB","#FF9900","#C7390B","#242858", "#808080")
highlights <- c("None" = "#ffffff", "Actual" =  "#86b3fe", "Predicted" = "#d4c802")
highlights.alpha = 0.3
set.seed(1)

# A Density Plot using default colors.
#   If using a melted dataset, specify a Levels column for ggplot fill: 
#     melt(..., id.var = c("Levels", ...)).
#
# Returns a ggplot
mms_density_plot <- function(data){
  density_plot <- function(column){
    columns <- colnames(common_plot_data)
    color = c("MP" = plot_colors.Fruit[1], "Other" = plot_colors.Fruit[6])
    label = levels(common_plot_data$Levels)
    
    plot <- if(("variable" %in% columns) & ("Levels" %in% columns)) {
      # If data has previously been melted
      ggplot(common_plot_data %>% filter(variable == column), 
             aes(x = value, colour = Levels, fill = Levels)) + 
        #geom_rug(aes(x = value, y = 0)) +
        plot_theme + xlab(column) + geom_density(alpha = 0, aes(y = ..density..)) +
        geom_histogram(bins = 500, alpha = 0.3, position = "identity",
                       aes(x = value, y = ..density.., colour = NULL, fill = Levels)) +
        scale_fill_manual(values = color, labels = label) +
        scale_colour_manual(values = color, labels = label)
    } else {
      ggplot(common_plot_data[column], aes_string(x = column)) + 
        plot_theme + xlab(column) +
        geom_density(fill = plot_colors.Fruit[6], colour = plot_colors.Fruit[6])
    }[]
    
    return(plot)
  }
  
  metricSubset <- c("Selected", "Priority", "Time", "Comments")
  featureSubset <- subtract(colnames(data), metricSubset)
  common_plot_data = data.frame(data[,featureSubset])
  common_plot_data$Levels <- "Other"
  common_plot_data$Levels[grep("MP", data$Comments)] <- "MP"
  
  common_plot_data <- melt(common_plot_data, id.vars = c("Levels"))
  
  plot_theme <- plot_theme + theme(legend.position = "none")
  plots <- lapply(featureSubset, density_plot)
  plots[[1]] = plots[[1]] + theme(legend.position = "top")
  
  gridPlot <- plot_grid(plotlist = plots, ncol = 2)
  return(gridPlot)
}

# Cross Validation plot for Random Forest
rf_cv_plot <- function(feature, response){
  rf.cv <- rfcv(feature, response, ntree = 20, cv.fold = 15)
  
  ggplot(data.frame(Variables = rf.cv$n.var, Error = rf.cv$error.cv)) + 
    theme_minimal() + 
    geom_point(aes(x = Variables, y = Error), colour = plot_colors.Fruit[2]) +  
    labs(title = "Cross Validation of Tree Depth")
}

# Find the replicated regions of the given input, and output start time, end time,
# and the selected value for that replicate.
# Intended to be used with a logical 1/0 or TRUE/FALSE vector.
# Selection labels format: c(label for 1/TRUE, label for 0/FALSE)
regions <- function(input, time, labels){
  tmp <- c(FALSE, abs(diff(input)) == 1)
  colors <- ifelse(input == 1, labels[1], labels[2])
  s <- c(time[1], time[tmp] + 1)
  e <- c(time[tmp], time[length(time)])
  col <- c(colors[1], colors[tmp])
  selections <- data.frame(start = s, end = e, Selection = col)
  return(selections)
}

# Dummy plot just for generating and returning Actual/Predicted legend
bottom_legend <- function(){
  data <- data.frame(value = c(1, 2), Time = c(0, 2))
  sel <- data.frame(start = c(0, 1), end = c(1, 2), 
                    Selection = c("Predicted", "Actual"))
  
  p <- ggplot() + theme_minimal() +
    geom_line(data = data, aes(x = Time, y = value)) +
    theme(legend.justification = "center", legend.position="bottom") +
    geom_rect(aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = Selection),
              data = sel, alpha = highlights.alpha) +
    scale_fill_manual(values = highlights)
  return(get_legend(p))
}

# retrieve 6 evenly spaced times for label limits
label_limits <- function(inrow){
  start = inrow[1]
  end = inrow[length(inrow)]
  ltime = end - start
  seg = floor(ltime/7)
  segments <- c(seg + start, seg*2 + start, seg*3 + start, seg*4 + start,
                seg*5 + start, seg*6 + start)
  return(segments)
}

# Create basic plot with no Highights, with Actuals Highlighted, 
# or with Actuals & Predicted Highlights.
basicTypePlot <- function(data, subset, plotTitle){
  melted <- melt(data.frame(data[,subset], Time = data$Time), id = 'Time')
  original <- as.POSIXct("2000-01-01 00:00:00.000000")
  label_segments <- label_limits(data$Time)
  label_text <- format(original + label_segments, "%m/%d/%y %H:%M") # "%m/%d/%y %H:%M"
  
  actual = FALSE
  predicted = FALSE
  
  # initial plot setup
  p <- ggplot() + ylab(plotTitle) + xlab("Date") + plot_theme +
        scale_x_discrete(limits = label_segments, labels = label_text)
  
  # Highlighting
  predicted = if("Highlight.Predicted" %in% colnames(data))
    regions(data$Highlight.Predicted, data$Time, c("Predicted", "None"))
  actual = if("Highlight.Actual" %in% colnames(data))
    regions(data$Highlight.Actual, data$Time, c("Actual", "None"))
  if(!is.null(actual) && !is.null(predicted)){
    dmin = abs(min(melted$value))
    dmax = abs(max(melted$value))
    half = ifelse(dmax == dmin, dmax / 2, ((dmax + dmin) / 2) - dmin)
  
    p <- p + geom_rect(data = actual, alpha = highlights.alpha, 
                  aes(xmin = start, xmax = end, ymin = -Inf, ymax = half, fill = Selection)) +
      geom_rect(data = predicted, alpha = highlights.alpha, 
                  aes(xmin = start, xmax = end, ymin = half, ymax = Inf, fill = Selection)) +
      scale_fill_manual(values = highlights)
  } else if (!is.null(actual) || !is.null(predicted)){
    d = if(!is.null(actual)){ actual } else { predicted }
    p <- p + geom_rect(data = d, alpha = highlights.alpha, 
                  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = Selection)) +
      scale_fill_manual(values = highlights)
  }
  
  # CI
  if(length(subset) == 1) {
    name.min = paste(subset[1], "Min", sep=".")
    name.max = paste(subset[1], "Max", sep=".")
    
    if (name.min %in% colnames(data) && name.max %in% colnames(data)) {
      d <- data.frame(Time = pull(data, "Time"), 
                      min=pull(data, name.min), max=pull(data, name.max))
      p <- p + geom_ribbon(data = d, aes(x = Time, ymin = min, ymax = max), 
                           alpha = 0.3, fill = plot_colors.Basic[1])
    }
  }
  
  # finish plot by adding lines
  p <- p + geom_line(data = melted, aes(x = Time, y = value, color = variable)) +
    scale_color_manual(values = plot_colors.Basic) +
    guides(color = guide_legend(title = plotTitle), fill=FALSE)
  
  return(p)
}

# Plots Grouped by Type with optional Highlights across Time
types_plot <- function(data, subsets, titles, plotTitle){
  # Create lists for plots.
  plotsList <- vector("list", length(subsets))
  legendsList <- vector("list", length(subsets))
  columns <- colnames(data)
  
  # function to Check that all columns exist
  columns_exist <- function(findThese, columns) all(findThese %in% columns)
  
  # loop through plot sets and create plots if the data exists
  for(i in 1:length(subsets)){
    if(columns_exist(subsets[[i]], columns)){
      # Initial plot and remove vertical grids
      p <- basicTypePlot(data, subsets[[i]], titles[i]) + 
        theme(legend.justification = "left", plot.margin = margin(1, 1, 1, 1),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
              axis.text.x = element_text(size = 8))
      
      # Get the legend from the plot, add it to legend list, 
      # and then remove it from the plot
      legendsList[[i]] <- if(length(subsets[[i]]) > 1) get_legend(p) else NULL
      p <- p + theme(legend.position='none')
      
      # Assign plot to plot list
      plotsList[[i]] <- p
    }
  }
  
  # Remove sets that weren't there
  emptyPlots <- sapply(plotsList, is.null)
  plotsList[emptyPlots] <- NULL
  legendsList[emptyPlots] <- NULL
  
  # Adjust plots based on what was available
  for(i in 1:length(plotsList)){
    # Add title to first plot
    if(i == 1) plotsList[[i]] <- plotsList[[i]] + 
        labs(title = plotTitle)
    
    # Remove x axis text from all but the last plot
    if(i != length(plotsList)) 
      plotsList[[i]] <- plotsList[[i]] + 
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
              axis.line.x=element_blank())
  }
  
  # Build grids
  g <- plot_grid(plotlist = plotsList, align = "hv", ncol = 1)
  l <- plot_grid(plotlist = legendsList, ncol = 1)
  
  # combine grids
  if(columns_exist(c("Highlight.Predicted"), columns)){
    gridPlot <- plot_grid(g, l, bottom_legend(), ncol = 2, 
                          rel_widths = c(7, 1), 
                          rel_heights = c(8, 1))
  } else {
    gridPlot <- plot_grid( g, l, rel_widths = c(7, 1))
  }
  
  return(gridPlot)
}

# Plot groups of the MMS data
mms_types_plot <- function(data){
  subsets <- list(c("FGM.Bx", "FGM.By", "FGM.Bz"),
                  c("FGM.Bt"),
                  c("DES.N", "DIS.N"),
                  c("DIS.Vx", "DIS.Vy", "DIS.Vz"),
                  c("DES.Vx", "DES.Vy", "DES.Vz"),
                  c("DES.T_para", "DES.T_perp", "DIS.T_para", "DIS.T_perp"))
  titles <- c("FGM.B", "FGM.Bt", "N", "DIS.V", "DES.V", "T")
  plotTitle = "Features Grouped by Type over Time"
  
  return(types_plot(data, subsets, titles, plotTitle))
}