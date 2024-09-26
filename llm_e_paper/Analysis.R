library(dplyr)
library(ggplot2)
library(scales)

# 论文数量
df1 <- read.csv("./LLM_E_Paper_Count.csv")

# 论文详细分析
df2 <- read.csv("./LLM_E_Paper_Analysis.csv")

# 论文数量趋势分析
g1 <- ggplot(df1, aes(x = Months, y = Count, group = 1)) +
  geom_line(color = "blue") +       # 添加折线
  geom_point(color = "blue") +      # 添加点
  theme_minimal() +                   # 使用简约主题
  labs(title = "LLM Evaluation Paper Count Over Months",
       x = "Months",
       y = "LLM Evaluation Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("paper_count_months.png", plot = g1, 
       width = 10, height = 4, units = "in")

topic_count <- as.data.frame(table(df2$topic))
topic_count <- topic_count[order(topic_count$Freq, decreasing = TRUE), ]
topic_count$start_value <- rep(0, length(topic_count$Var1))
g2 <- ggplot(topic_count, aes(x = reorder(Var1, Freq, decreasing = TRUE),
                              y = start_value, yend = Freq, color = Var1)) +
  geom_segment(linewidth = 2) +
  geom_point(aes(y = Freq), size = 3) +
  theme_minimal(base_size = 15, base_family = "STKaiti") +
  labs(title = "Papers of Different Topics Distribution",
       x = "Topic",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "None")
ggsave("paper_count_topics.png", plot = g2,
       width = 10, height = 4, units = "in")

# 详细 topic 分析
df2$date <- as.Date(paste0(df2$date, "-01"), format = "%b-%y-%d")
df2_summary <- df2 |>
  group_by(topic, date) |>
  arrange(desc(date), .by_group = TRUE) |>
  summarise(count = n(), .groups = "keep") |>
  ungroup()

g3 <- ggplot(df2_summary, aes(x = date, y = count)) +
  geom_line(linewidth = 1, color = "blue") +  # 绘制折线图，使用线宽为1，颜色为蓝色
  geom_point(size = 1, color = "red") +  # 添加点，使用点大小为1，颜色为红色
  labs(title = "Monthly Trend of Topics",  # 设置标题和轴标签
       x = "Month",
       y = "Number of Papers") +
  theme_minimal(base_size = 15, base_family = "STKaiti") +      # 使用简约主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +    # 旋转x轴标签
  scale_x_date(limits = as.Date(c("2023-04-01", "2024-09-01")), # 统一X轴范围和标签格式
               date_labels = "%y-%m") +
  scale_y_continuous(breaks = seq(10)) +
  facet_wrap(~ topic, nrow = 2, ncol = 5, scales = "free_y")   # 按topic分面，y轴独立刻度
ggsave("topics_trends.png", plot = g3,
       width = 10, height = 4, units = "in")

# 近 3 个月的 topic 趋势
df21_summary <- df2 |>
  filter(date >= as.Date("2024-06-01")) |>
  group_by(topic) |>
  summarise(count = n(), .groups = "keep") |>
  ungroup()

df21_summary$start_value <- rep(0, length(df21_summary$topic))
g4 <- ggplot(df21_summary, aes(x = reorder(topic, count, decreasing = TRUE),
                               y = start_value, yend = count, color = topic)) +
  geom_segment(linewidth = 2) +
  geom_point(aes(y = count), size = 3) +
  theme_minimal(base_size = 15, base_family = "STKaiti") +
  labs(title = "Papers Distribution for the Latest 3 Months",
       x = "Topic",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "None")
ggsave("paper_count_topics_3_months.png", plot = g4,
       width = 10, height = 4, units = "in")