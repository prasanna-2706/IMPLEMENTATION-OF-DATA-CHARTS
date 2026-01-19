library(ggplot2)
library(dplyr)

student_performance <- read.csv(
  "C:/Users/PRASANNA/OneDrive/Documents/1.student_performance.csv"
)

student_performance <- na.omit(student_performance)

student_performance$Total_Marks <-
  student_performance$Internal_Test1 +
  student_performance$Internal_Test2 +
  student_performance$Assignment_Marks

subject_avg <- student_performance %>%
  group_by(Subject) %>%
  summarise(Average_Marks = mean(Total_Marks))

ggplot(subject_avg, aes(x = Subject, y = Average_Marks, fill = Subject)) +
  geom_bar(stat = "identity") +
  ggtitle("Subject-wise Average Marks\nPRASANNA S M | 23BAD087") +
  xlab("Subjects") +
  ylab("Average Marks") +
  theme_minimal()

test_avg <- data.frame(
  Test = c("Internal Test 1", "Internal Test 2", "Assignment"),
  Marks = c(
    mean(student_performance$Internal_Test1),
    mean(student_performance$Internal_Test2),
    mean(student_performance$Assignment_Marks)
  )
)

ggplot(test_avg, aes(x = Test, y = Marks, group = 1)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  ggtitle("Performance Trend Across Tests\nPRASANNA S M | 23BAD087") +
  xlab("Tests") +
  ylab("Average Marks") +
  theme_minimal()

grade_data <- student_performance %>%
  count(Final_Grade)

ggplot(grade_data, aes(x = "", y = n, fill = Final_Grade)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  ggtitle("Final Grade Distribution\nPRASANNA S M | 23BAD087") +
  theme_void()
