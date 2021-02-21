######### Validating with graphs

# MATH 

stu_sus_timss %>%
  ggplot(aes(x = math_scores, fill = factor(traditional_style_math,
                                            labels = c('Modern style',
                                                       "Traditional style"))))  +
  geom_density(alpha = .3) + 
  theme_classic() +
  scale_fill_manual(values = c('deepskyblue2', 'darksalmon')) +
  labs(x = "Math Scores", y = "Density", fill = "") + 
  geom_vline(aes(xintercept = mean(stu_sus_timss[traditional_style_math == 1, 'math_scores'])),  
             color = "darksalmon", linetype = "dashed", size = 3) + 
  geom_vline(aes(xintercept = mean(stu_sus_timss[traditional_style_math == 0, 'math_scores'])),  
             color = "deepskyblue2", linetype = "dashed", size = 3) + 
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "bottom") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(stu_sus_timss[stu_sus_timss$traditional_style_math == 1, 'math_scores']), 1)),
                                           "\nMean Score"), x = 700, y = 0.003, size = 6,
                    colour = "darksalmon") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(stu_sus_timss[stu_sus_timss$traditional_style_math == 0, 'math_scores']), 1)),
                                           "\nMean Score"), x = 400, y = 0.003, size = 6,
                    colour = "deepskyblue2") +
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) + 
  ggtitle("Math Scores by the Usage of Traditional vs. Modern Layouts in Math Classes")


# SCIENCE 

stu_sus_timss %>%
  ggplot(aes(x = science_scores, fill = factor(traditional_style_science,
                                               labels = c('Modern style',
                                                          "Traditional style"))))  +
  geom_density(alpha = .3) + 
  theme_classic() +
  scale_fill_manual(values = c('deepskyblue2', 'darksalmon')) +
  labs(x = "Science Scores", y = "Density", fill = "") + 
  geom_vline(aes(xintercept = mean(stu_sus_timss[traditional_style_science == 1, 'science_scores'])),  
             color = "darksalmon", linetype = "dashed", size = 3) + 
  geom_vline(aes(xintercept = mean(stu_sus_timss[traditional_style_science == 0, 'science_scores'])),  
             color = "deepskyblue2", linetype = "dashed", size = 3) + 
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "bottom") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(stu_sus_timss[stu_sus_timss$traditional_style_science == 1, 'science_scores']), 1)),
                                           "\nMean Score"), x = 700, y = 0.003, size = 6,
                    colour = "darksalmon") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(stu_sus_timss[stu_sus_timss$traditional_style_science == 0, 'science_scores']), 1)),
                                           "\nMean Score"), x = 400, y = 0.003, size = 6,
                    colour = "deepskyblue2") +
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) + 
  ggtitle("Science Scores by the Usage of Traditional vs. Modern Layouts in Science Classes")


# Wellness 

# Math classes
stu_sus_timss %>%
  ggplot(aes(x = wellness, fill = factor(traditional_style_math,
                                         labels = c('Modern style',
                                                    "Traditional style"))))  +
  geom_density(alpha = .3) + 
  theme_classic() +
  scale_fill_manual(values = c('deepskyblue2', 'darksalmon')) +
  labs(x = "Wellness Scores", y = "Density", fill = "") + 
  geom_vline(aes(xintercept = mean(stu_sus_timss[traditional_style_math == 1, 'wellness'])),  
             color = "darksalmon", linetype = "dashed", size = 3) + 
  geom_vline(aes(xintercept = mean(stu_sus_timss[traditional_style_math == 0, 'wellness'])),  
             color = "deepskyblue2", linetype = "dashed", size = 3) + 
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "bottom") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(stu_sus_timss[stu_sus_timss$traditional_style_math == 1, 'wellness']), 1)),
                                           "\nMean Score"), x = 1, y = 0.4, size = 6,
                    colour = "darksalmon") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(stu_sus_timss[stu_sus_timss$traditional_style_math == 0, 'wellness']), 1)),
                                           "\nMean Score"), x = 4, y = 0.4, size = 6,
                    colour = "deepskyblue2") +
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) + 
  ggtitle("Wellness Scores by the Usage of Traditional vs. Modern Layouts in Math Classes")

# Science classes
stu_sus_timss %>%
  ggplot(aes(x = wellness, fill = factor(traditional_style_science,
                                         labels = c('Modern style',
                                                    "Traditional style"))))  +
  geom_density(alpha = .3) + 
  theme_classic() +
  scale_fill_manual(values = c('deepskyblue2', 'darksalmon')) +
  labs(x = "Wellness Scores", y = "Density", fill = "") + 
  geom_vline(aes(xintercept = mean(stu_sus_timss[traditional_style_science == 1, 'wellness'])),  
             color = "darksalmon", linetype = "dashed", size = 3) + 
  geom_vline(aes(xintercept = mean(stu_sus_timss[traditional_style_science == 0, 'wellness'])),  
             color = "deepskyblue2", linetype = "dashed", size = 3) + 
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "bottom") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(stu_sus_timss[stu_sus_timss$traditional_style_science == 1, 'wellness']), 1)),
                                           "\nMean Score"), x = 1, y = 0.4, size = 6,
                    colour = "darksalmon") + 
  ggplot2::annotate("text", label = paste0(as.character(round(mean(stu_sus_timss[stu_sus_timss$traditional_style_science == 0, 'wellness']), 1)),
                                           "\nMean Score"), x = 4, y = 0.4, size = 6,
                    colour = "deepskyblue2") +
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) + 
  ggtitle("Wellness Scores by the Usage of Traditional vs. Modern Layouts in Science Classes")

