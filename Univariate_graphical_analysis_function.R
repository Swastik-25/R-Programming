#====================================================================================
# Graph Function based on four suggestions
#====================================================================================

graphs_export <- function(dir,data,cols = NULL)
{
  "This function helps in automating the plotting of graphs for different variables
  present in a dataframe. This will save some additional time in EDA for the users.
  The function accepts the path of the directory where images are exported(dir), 
  dataframe (data), specific columns for which graphs needs to be plotted (cols)."
  
  
  # Store the working directory
  oldPath <- getwd()
  setwd(dir) # Temporarily set the 'dir' as working directory for exporting the graphs
  
  # Check if variables are provided or not
  if (is.null(cols))
  {
    cols = seq(1:ncol(data))  # assign all the columns list to this vector
  }
  
  for (i in cols)
  {
    #Create .png filenames
    png(paste(names(data)[i], ".png", sep=""))
    
    par(mfrow=c(2,1))
    
    # Check if the variable is numeric or not
    if(is.numeric(data[,i]))
    {
      # Plot a boxplot
      boxplot(data[,i],
              main = paste("Boxplot of",names(data)[i]),
              xlab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      # Plot a histogram
      hist(data[,i],
           main = paste("Histogram of",names(data)[i]),
           xlab = names(data)[i], col = "skyblue2")
      
    }
    
    # Check if the variable is factor or not
    else if(is.factor(data[,i]))
    {
      # Proportion of data
      x <- prop.table(table(data[,i]))
      
      # Plot a pie chart
      pie(x,names(x),main = paste("BarPlot of",names(data)[i]),
          col = rainbow(length(x)),radius = 1.0)
      
      # Plot a barplot
      barplot(x,main = paste("BarPlot of",names(data)[i]),
              col = rainbow(length(x)))

    }
    dev.off()
  }
  setwd(oldPath)
}

