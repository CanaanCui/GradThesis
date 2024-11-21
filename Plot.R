# Load necessary library
library(ggplot2)

# Generate data
x <- seq(-11, 121, by = 1)
y1 <- 0.059 * x - 0.0006 * x^2
y2 <- 0.073 * x - 0.0006 * x^2

# Create data frame
data <- data.frame(
  x = rep(x, 2),
  y = c(y1, y2),
  label = rep(c("Re(0.059 * x - 0.0006 * x^2)", "Re(0.073 * x - 0.0006 * x^2)"), each = length(x))
)

# Create the plot
plot <- ggplot(data, aes(x = x, y = y, color = label)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) + # Set colors for the lines
  labs(x = "x", y = "y",
       color = "", # Empty color label for legend
       subtitle = "(x from -11 to 121)") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "#2E2E2E", color = NA),
    panel.background = element_rect(fill = "#2E2E2E", color = NA),
    legend.background = element_rect(fill = "#2E2E2E", color = NA),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.position = "bottom"
  )

# Save the plot to the specified directory
output_path <- "/Users/canaandrinkwater/Desktop/Thesis_Creativity/11152024/Output/plot.png"
ggsave(output_path, plot, width = 10, height = 6, dpi = 300)

cat("Plot saved to:", output_path, "\n")
