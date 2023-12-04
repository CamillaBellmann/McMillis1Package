#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


my_data_summary <- function(df){
  for (col in colnames(df)) {
    col_data <- df[[col]]

    if (is.numeric(col_data) || is.integer(col_data)) {
      min_ <- min(col_data, na.rm = TRUE)
      q1_ <- quantile(col_data, 0.25, na.rm = TRUE)
      median_ <- median(col_data, na.rm = TRUE)
      mean_ <- mean(col_data, na.rm = TRUE)
      q3_ <- quantile(col_data, 0.75, na.rm = TRUE)
      max_ <- max(col_data, na.rm = TRUE)
      NAs <- sum(is.na(col_data))
      NAs <- round(NAs, digits = 0)

      col_summary <- data.frame(
        "Min." = min_,
        "1st Qu." = q1_,
        "Median" = median_,
        "Mean" = mean_,
        "3rd Qu." = q3_,
        "Max." = max_
      )
      names(col_summary) <- c("Min.:", "1st Qu.:", "Median:", "Mean:", "3rd Qu.:", "Max.:")
      col_summary <- round(col_summary, digits = 3)
      rownames(col_summary) <- ("") #otherwise it prints 25% and with rownames(col_summary) <- NULL it would print [,1] which is ugly as well.
      cat(col, "\n")
      print(t(col_summary))

      if(NAs != 0){
        cat("NA's   ", NAs, "\n")
      }
      cat("\n")
    }
    if (is.factor(col_data) || is.character(col_data)) {
      var_unique <- unique(col_data)
      NAs <- sum(is.na(col_data))
      NAs <- round(NAs, digits = 0)

      tabellen <- table(col_data)
      #if(length(tabellen) > 10) {
      # sort(tabellen, decreasing = TRUE)}
      cat(col, "\n")

      if (is.factor(col_data)) {
        levels_col <- levels(col_data)
        if (length(var_unique) > 5) {
          for (i in seq_along(levels_col[1:5])) {
            cat(levels_col[i], ":", tabellen[levels_col[i]], "\n")
          }
          other_count <- sum(tabellen[levels_col[-(1:5)]])
          cat("Other:", other_count, "\n")
        } else {
          for (i in seq_along(levels_col)) {
            cat(levels_col[i], ":", tabellen[levels_col[i]], "\n")
          }
        }
      } else {
        if (length(var_unique) > 5) {
          for (i in seq_along(var_unique[1:5])) {
            cat(var_unique[i], ":", tabellen[var_unique[i]], "\n")
          }
          other_count <- sum(tabellen[var_unique[-(1:5)]])
          cat("Other:", other_count, "\n")
        } else {
          for (i in seq_along(var_unique)) {
            cat(var_unique[i], ":", tabellen[var_unique[i]], "\n")
          }
        }
      }

      if (NAs != 0) {
        cat("NA's: ", NAs, "\n\n")
      } else {
        cat("\n\n")
      }
    }
  }
}


