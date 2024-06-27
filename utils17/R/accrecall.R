accrecall_inner <- function(data) {
  true_positive <- true_negative <- false_positive <- false_negative <- 0
  with(data, {
    true_positive <<- sum(人工评分 == 1 & gpt评分 == 1)
    true_negative <<- sum(人工评分 == 0 & gpt评分 == 0)
    false_positive <<- sum(人工评分 == 0 & gpt评分 == 1)
    false_negative <<- sum(人工评分 == 1 & gpt评分 == 0)
  })

  accuracy <- (true_positive + true_negative) / nrow(data)
  precision <- ifelse((true_positive + false_positive) > 0,
                      true_positive / (true_positive + false_positive), 0)
  recall <- ifelse((true_positive + false_negative) > 0,
                   true_positive / (true_positive + false_negative), 0)
  f1_score <- ifelse((precision + recall) > 0,
                     2 * (precision * recall) / (precision + recall), 0)

  return(data.frame(Accuracy = accuracy, Precision = precision,
                    Recall = recall, F1_Score = f1_score))
}

accrecall <- function(data, category = "") {
  cols <- names(data)
  overall_metrics <- accrecall_inner(data = data)
  overall_metrics$Style <- "整体"
  if (!nzchar(category)) {
    return(overall_metrics)
  }

  if (!(category %in% cols)) {
    return(overall_metrics)
  }

  styles <- unique(data[[category]])
  style_metrics <- do.call(rbind, lapply(styles, function(style) {
    style_data <- subset(data, data[[category]] == style)
    metrics <- accrecall_inner(style_data)
    metrics$Style <- style
    return(metrics)
  }))

  all_metrics <- rbind(overall_metrics, style_metrics)
  all_metrics <- all_metrics[, c("Style", "Accuracy", "Precision", "Recall",
                                 "F1_Score")]
  return(all_metrics)
}