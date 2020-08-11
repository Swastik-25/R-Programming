#====================================================================================
# User-Defined function for two sample t-test
#====================================================================================

pair_Ttest <- function(data,tar,cont)
{
  "This function accepts 3 arguments to perform a two sample t-test. 'data' will accept
  a dataframe, 'tar' will accept any factor/categorical variable (2 categories),
  'cont' will accept a numeric/continuous variable. 
  
  The function will give the barplot comparing the group means of the categories. It also
  produces the t-test output which shows if the relationship is significant or not."
  
  for (i in cont)
  {
    # Extracting the samples of both the categories in target variable
    S1 <- data[data[,tar] == names(table(data[,tar]))[2],i]
    S1 <- S1[!is.na(S1)]
    S2 <- data[data[,tar] == names(table(data[,tar]))[1],i]
    S2 <- S2[!is.na(S2)]
    
    # t-statistic calculation for two samples
    num <- mean(S1) - mean(S2)
    denom <- sqrt((sd(S1)**2/length(S1)) + (sd(S2)**2/length(S2)))
    t_stat <- num/denom
    
    # Degrees of freedom calculation
    df_num <- ((sd(S1)**2/length(S1)) + (sd(S2)**2/length(S2)))**2
    df_denom <- (sd(S1)**4/(length(S1)**2 * (length(S1) - 1) )) + (sd(S2)**4/(length(S2)**2 * (length(S2) - 1) ))
    df_t <- df_num / df_denom
    
    # two sided hypothesis check; p-value calculation
    p_val <- 2*pt(-abs(t_stat),df = df_t)
    
    print("Categorical - Continuous Two Sample t-Test",quote = F)
    print("==========================================",quote = F)
    print(paste("Test between :",tar," and ",i),quote = F)
    print(paste("t = ",t_stat,", df = ",df_t,", p-value = ",p_val),quote = F)
    cat("\n")
    
    # Plotting the barplot to show the mean value difference between two groups
    a <- c(mean(S2),mean(S1))
    names(a) <- c(unique(fram$TenYearCHD)[1], unique(fram$TenYearCHD)[2])
    barplot(a, main=paste("Mean Comparision of ",i," and ",tar), xlab = tar,
            ylab=paste("Mean value of ",i), col=rainbow(nrow(tab)))
    
  }
  
}


#====================================================================================
# User-Defined function for Chi square test
#====================================================================================

chisq_func <- function(data,tar,cat)
{
  "This function accepts 3 arguments to perform a chi square test. 'data' will accept
  a dataframe, 'tar' and 'cat' both will accept any factor/categorical variable. 
  
  The function will give the barplot comparing the proportion of data split between the 
  categories. It also produces the chi square test output which shows if the relationship
  is significant or not."
  
  for (var in cat)
  {
    # Cross tab between the categorical variables
    ctab <- table(data[,var], data[,tar])
    #ctab <- matrix(ctab, nrow = nrow(ctab), ncol = ncol(ctab))
    
    # Calculate the Chi-square test statistic
    ChiSq_stat = 0
    for (i in 1:ncol(ctab))
    {
      for (j in 1:nrow(ctab))
      {
        expt <- (rowSums(ctab)[j] * colSums(ctab)[i])/sum(ctab)
        temp <- (ctab[j,i] - expt)**2 / expt
        ChiSq_stat <- ChiSq_stat + temp
      }
    }
    
    # calculate the degrees of freedom
    chi_df <- (nrow(ctab) - 1) * (ncol(ctab) - 1)
    
    # calculate the p-value from chi-square distribution
    p_val <- pchisq(ChiSq_stat,df=chi_df,lower.tail = F)
    
    # Final observations of the Chi-Squared test
    
    print("Categorical - Categorical Chi-Squared Test",quote = F)
    print("==========================================",quote = F)
    print(paste("Test between :",tar," and ",var),quote = F)
    print(paste("X-squared = ",ChiSq_stat,", df = ",chi_df,", p-value = ",p_val),quote = F)
    cat("\n")
    
    if(length(unique(data[,var])) > 10)
    {
      cat("Warning: \nMore no. of categories than expected,the Chi-square estimate may be incorrect.")
    }
    else
    {
      # Proportion distribution 
      tab_data <- round(prop.table(table(data[,var], data[,tar])),3)
      
      # Plotting barplot between both the categorical variables
      my_bar <- barplot(tab_data, main=paste("Distribution of ",tar," by ",var),
                        ylab="Proportion", xlab = tar,col=rainbow(nrow(tab_data)),
                        ylim = c(0.00,1.00), beside=TRUE)
      
      # Add the legend
      legend("topright", legend = rownames(tab_data), title = var,col=rainbow(nrow(tab_data)),
             pch=15 , pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))
      
      # Add the text 
      text(y = tab_data+0.05 , x = my_bar, labels = as.character(tab_data) ,cex=1) 
      
    }
  }
  
}


#====================================================================================
# User-Defined function for to rearrange variables in a dataframe by it's datatypes
#====================================================================================

rearrange_func <- function(data)
{
  df <- sort(sapply(data,class),decreasing = T) 
  cols <- names(df)
  
  ordered_cols = c()
  for (i in 1:length(cols))
  {
    ordered_cols[i] <- cols[i]
  }
  return(data[,ordered_cols])
}
