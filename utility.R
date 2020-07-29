library(dplyr)
library(ggplot2)

f_rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}

f_eda <- function(df){
  df_missing <- data.frame(t(apply(df, 2, function(x){mean(is.na(x))})))
  
  df_summary_num <- data.frame(apply(dplyr::select_if(df, is.numeric), 2, summary))

  df_summary_char <- apply(dplyr::select_if(df, is.character), 2, table)
  
  ind_dups <- duplicated(df)
  
  output <- list()
  output[['missing_counts']] <- df_missing
  output[['summaries_fact_cols']] <- df_summary_char
  output[['summaries_num_cols']] <- df_summary_num
  output[['duplicate_rows']] <- df[ind_dups,]
  return(output)
} 