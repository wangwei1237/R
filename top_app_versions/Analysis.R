library(dplyr)
library(ggplot2)

# 数据来源于七麦
data <- read.csv("./top_app_release.csv",
                 stringsAsFactors = FALSE)
data$Date <- as.Date(data$Date, format = "%Y/%m/%d")

clean_data <- data |>
  filter(!is.na(Date)) |>
  group_by(APP, Type) |>
  arrange(desc(Date), .by_group = TRUE) |>
  mutate(Interval = ifelse(row_number() == 1, 0,
                           as.numeric(lag(Date, default = Date[1]) - Date))) |>
  filter(Interval > 0) |>
  ungroup()

# print(clean_data, n = 500)

# 计算每个箱线图的中位数
summary_data <- clean_data |>
  group_by(APP, Type) |>
  summarize(Median_Interval = median(Interval, na.rm = TRUE),
            Mean_Interval = mean(Interval, na.rm = TRUE),
            .groups = "keep")

g1 <- ggplot(clean_data, aes(x = APP, y = Interval, fill = Type)) +
  geom_boxplot() +
  geom_text(data = summary_data,
            aes(x = APP, y = Mean_Interval, label = round(Mean_Interval, 0)),
            position = position_dodge(width = 0.75), vjust = -0.3, size = 4) +
  labs(title = "TOP APP 的发版时间间隔分布",
       x = "",
       y = "发版间隔（天）") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) + # 自定义颜色
  theme_minimal(base_family = "STKaiti") +
  theme(legend.position = "top",             # 图例放置在左上角
        legend.justification = c(0, 1),      # 设置图例的对齐方式
        legend.title = element_blank())

# 用 t 分布估算平均值的 95% 置信区间
bar_data <- clean_data |>
  group_by(APP, Type) |>
  summarise(n = n(),
            min  = min(Interval, na.rm = TRUE),
            max  = max(Interval, na.rm = TRUE),
            mean = mean(Interval, na.rm = TRUE),
            sd = sd(Interval, na.rm = TRUE),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n),
            .groups = "keep") |>
  filter(mean < 30) |>
  filter(ci < 30)

print(bar_data)
g2 <- ggplot(bar_data, aes(x = APP, y = mean, fill = Type)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7) +
  geom_errorbar(aes(ymin = ifelse(mean - ci < 0, 0, mean - ci),
                    ymax = ifelse(mean + ci > 100, mean, mean + ci)),
                position = position_dodge(width = 0.8),
                width = 0.2) + 
  labs(title = "TOP APP 的平均发版时间隔（95%置信区间）",
       x = "",
       y = "平均发版间隔（天）") +
  #scale_fill_manual(values = c("lightblue", "lightgreen")) + # 自定义颜色
  theme_minimal(base_family = "STKaiti") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, 14),
                     breaks = seq(0, 14, by = 2)) +  # 强制x轴从y=0开始
  theme(legend.position = "top",             # 图例放置在左上角
        legend.justification = c(0, 1),      # 设置图例的对齐方式
        legend.title = element_blank())



ggsave("app_version_box.jpeg", plot = g1,
       width = 10, height = 4, units = "in")

ggsave("app_version_bar.jpeg", plot = g2,
       width = 10, height = 4, units = "in")