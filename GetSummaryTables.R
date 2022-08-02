GetSummaryTablesCIs <- function(df, colNames, testTitle){
  colnames(df) <- colNames
  healthy <- df %>% filter(TBI_Non %like% "Non")
  table <-get_summary_stats(healthy, show = c("min", "max", "mean", "sd"))
  table$Type <- "Control"
  table$Lower=qt(.025, table$n - 1)*table$sd/table$n + table$mean
  table$Upper=qt(.975, table$n - 1)*table$sd/table$n + table$mean
  #health <- table   %>%
  #  kbl(caption = "Summary Pre-Intervention Data for the Healthy Group" ) %>%
  #  kable_classic(full_width = F, html_font = "Cambria")
  
  
  shortTerm <- df %>% filter(TBI_Non %like% "Short")
  stable <-get_summary_stats(shortTerm, show = c("min", "max", "mean", "sd"))
  stable$Type <- "mTBI"
  stable$Lower=qt(.025, stable$n - 1)*stable$sd/table$n + stable$mean
  stable$Upper=qt(.975, stable$n - 1)*stable$sd/table$n + stable$mean
  #short <- table %>%
  #  kbl(caption = "Summary Pre-Intervention Data for the Short-Term Group") %>%
  #  kable_classic(full_width = F, html_font = "Cambria")
  
  longTerm <- df %>% filter(TBI_Non %like% "Long")
  ltable <-get_summary_stats(longTerm, show = c("min", "max", "mean", "sd"))
  ltable$Type <- "PCS"
  ltable$Lower=qt(.025, ltable$n - 1)*ltable$sd/table$n + ltable$mean
  ltable$Upper=qt(.975, ltable$n - 1)*ltable$sd/table$n + ltable$mean
  #long 
  
  fullTable <- rbind(table, stable, ltable)
  
  f_Table<- fullTable %>% #arrange(variable) %>%
      kbl(caption = testTitle) %>%
      kable_classic(full_width = F, html_font = "Cambria")
  
  return(f_Table)
}