library(dplyr)
library(ggplot2)
library(reshape2)

# 数据来源于七麦
data <- read.csv("./app_versions.csv",
                 stringsAsFactors = FALSE)
data$APP <- factor(data$APP)
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

mean_data <- clean_data |>
  group_by(APP) |>
  summarize(Total_Mean_Interval = mean(Interval, na.rm = TRUE),
            .groups = "keep")

# 根据 APP 的整体中位数重新排序
clean_data <- clean_data |>
  left_join(mean_data, by = "APP") |>
  arrange(Total_Mean_Interval)

# 计算每个箱线图的中位数
median_data <- clean_data |>
  group_by(APP, Type) |>
  summarize(Median_Interval = median(Interval, na.rm = TRUE), .groups = "keep")

g <- ggplot(clean_data, aes(x = reorder(APP, Total_Mean_Interval), 
                            y = Interval, fill = Type)) +
  geom_boxplot() +
  geom_text(data = median_data, aes(x = APP, y = Median_Interval,
                                    label = round(Median_Interval, 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3.5) +
  labs(title = "AI 原生 APP 的发版时间间隔分布",
       x = "APP",
       y = "发版间隔（天）") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) + # 自定义颜色
  theme_minimal(base_family = "STKaiti") +
  theme(legend.position = "top",             # 图例放置在左上角
        legend.justification = c(0, 1),      # 设置图例的对齐方式
        legend.title = element_blank())

bar_data <- clean_data |>
  group_by(APP, Type) |>
  summarise(n = n(),
            min  = min(Interval, na.rm = TRUE),
            max  = max(Interval, na.rm = TRUE),
            mean = mean(Interval, na.rm = TRUE),
            sd = sd(Interval, na.rm = TRUE),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n),
            .groups = "keep")

print(bar_data)

g2 <- ggplot(bar_data, aes(x = reorder(APP, mean), y = mean, fill = Type)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7) +
  geom_errorbar(aes(ymin = ifelse(mean - ci < 0, 0, mean - ci),
                    ymax = mean + ci),
                position = position_dodge(width = 0.8),
                width = 0.2) + 
  labs(title = "AI 原生 APP 的平均发版时间隔（95%置信区间）",
       x = "",
       y = "平均发版间隔（天）") +
  #scale_fill_manual(values = c("lightblue", "lightgreen")) + # 自定义颜色
  theme_minimal(base_family = "STKaiti") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, 18),
                     breaks = seq(0, 18, by = 2)) +  # 强制x轴从y=0开始
  theme(legend.position = "top",             # 图例放置在左上角
        legend.justification = c(0, 1),      # 设置图例的对齐方式
        legend.title = element_blank())

ggsave("app_versions_box.jpeg", plot = g,
       width = 10, height = 4, units = "in")

ggsave("app_versions_bar.jpeg", plot = g2,
       width = 10, height = 4, units = "in")