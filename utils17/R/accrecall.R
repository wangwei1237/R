accrecall_inner <- function(data, actural, predict) {
  true_positive <- sum(data[[actural]] == 1 & data[[predict]] == 1)
  true_negative <- sum(data[[actural]] == 0 & data[[predict]] == 0)
  false_positive <- sum(data[[actural]] == 0 & data[[predict]] == 1)
  false_negative <- sum(data[[actural]] == 1 & data[[predict]] == 0)

  accuracy <- (true_positive + true_negative) / nrow(data)
  precision <- ifelse((true_positive + false_positive) > 0,
                      true_positive / (true_positive + false_positive), 0)
  recall <- ifelse((true_positive + false_negative) > 0,
                   true_positive / (true_positive + false_negative), 0)
  f1_score <- ifelse((precision + recall) > 0,
                     2 * (precision * recall) / (precision + recall), 0)
  fpr <- ifelse((true_negative + false_positive) > 0,
                false_positive / (true_negative + false_positive), 0)
  return(data.frame(Accuracy = accuracy, Precision = precision,
                    Recall = recall, F1_Score = f1_score, FPR = fpr))
}

accrecall <- function(data, category, actural, predict) {
  cols <- names(data)
  if (!(actural %in% cols) | !(predict %in% cols)) {
    return("")
  }

  overall_metrics <- accrecall_inner(data, actural, predict)
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
    metrics <- accrecall_inner(style_data, actural, predict)
    metrics$Style <- style
    return(metrics)
  }))

  all_metrics <- rbind(overall_metrics, style_metrics)
  all_metrics <- all_metrics[, c("Style", "Accuracy", "Precision", "Recall",
                                 "F1_Score", "FPR")]
  return(all_metrics)
}