library(ggplot2)
library(reshape2)

# 计算堆叠的条形图

# 创建数据框
data <- data.frame(
  Benchmark = c("MATH", "MathVista (testmini)", "MMMU (val)", "MMLU"),
  B = c(94.8, 73.2, 78.1, 92.3),   # B的得分
  A = c(60.3, 63.8, 69.1, 88.0)  # A的得分
)

melted_data <- melt(data, id.vars = c("Benchmark"), variable.name = "Type", 
                    value.name = "Value")

# 绘制横向条形图（A 和 B 都从 0 开始，重叠显示）
g <- ggplot(melted_data, aes(x = Benchmark, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "identity", width = 0.5,
           alpha = 0.7) +  # 重叠条形图
  scale_fill_manual(values = c("A" = "#1EB9B8", "B" = "#FF6F61")) +  # 设置填充颜色
  labs(title = "ML Benchmarks", y = "pass@1 accuracy", x = "", fill = "") +
  theme_minimal(base_family = "STKaiti") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    legend.position.inside = c(0.5, 1),
    legend.justification = c("center", "top")
  ) +
  geom_text(data = subset(melted_data, Type == "A"),
            aes(label = round(Value, 1)),
            position = position_nudge(y = -5),
            color = "black") +  # A 的数值标注在条形图内部
  geom_text(data = subset(melted_data, Type == "B"),
            aes(label = round(Value, 1)),
            position = position_nudge(y = 5),
            color = "black") +  # B 的数值标注在条形图顶部
  coord_flip()  # 交换坐标轴，生成横向条形图

ggsave("duibi.jpeg", plot = g, width = 10, height = 4, units = "in")