library(dplyr)
library(ggplot2)
library(reshape2)

# 数据来源于七麦
data <- read.csv("./app_versions.csv",
                 stringsAsFactors = FALSE)

melted_data <- melt(data, id.vars = c("APP", "Version"), variable.name = "Type",
                    value.name = "Date")
melted_data$Date <- as.Date(melted_data$Date, format = "%Y/%m/%d")

clean_data <- melted_data |>
  filter(!is.na(Date)) |>
  group_by(APP, Type) |>
  mutate(Interval = ifelse(row_number() == 1, 0,
                           as.numeric(lag(Date, default = Date[1]) - Date))) |>
  filter(Interval > 0) |>
  ungroup()

print(clean_data, n = 300)

# 计算每个箱线图的中位数
median_data <- clean_data |>
  group_by(APP, Type) |>
  summarize(Median_Interval = median(Interval, na.rm = TRUE), .groups = "keep")

g <- ggplot(clean_data, aes(x = APP, y = Interval, fill = Type)) +
  geom_boxplot() +
  geom_text(data = median_data, aes(x = APP, y = Median_Interval,
                                    label = round(Median_Interval, 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3.5) +
  labs(title = "不同 APP 的发版时间间隔分布",
       x = "APP",
       y = "发版间隔（天）") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) + # 自定义颜色
  theme_minimal(base_family = "STKaiti") +
  theme(legend.position = "top",             # 图例放置在左上角
        legend.justification = c(0, 1),      # 设置图例的对齐方式
        legend.title = element_blank())

ggsave("app_versions.jpeg", plot = g,
       width = 10, height = 4, units = "in")