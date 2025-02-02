# tch-quality_sch-envir1b.R

# Teaching Styles, Teaching Quality, School Environmemt, Students' Beliefs, and Learning Outcomes
# Graphing

library(tidySEM)

fit_names <- paste0('fit_sem_multi_plot', 1:6)
outcomes_old <- c('MATH', 'SCIENCE', 'WELLNESS', 'APPLYING', 'REASONING', 'KNOWING')
outcomes_new <- c('Math', 'Science', 'Wellness', 'Applying', 'Reasoning', 'Knowing')
plot_multi_all <- list()

for (j in 1:length(fit_names)){


lat_var_old_ <- c('Math_Teacher_Understandable',
            'Teacher_Quality',			
            'Math_Teacher_Orderliness',	
            'Math_Important',
            'Student_Attitudes',			
            'Math_Enjoy',
            'Math_Strong',
            'Audibility',
            'Conditions',			
            'Visibility',				
            'Temperature',				
            'Safety',
            'n_tech',
            'Comfort_furniture'
)

lat_var_old <- c(lat_var_old_, outcomes_old[j])

lat_var_new_ <- c('Understandable',
                 'Teacher Quality', 
                 'Orderliness',
                 'Math Important',
                 'Student Attitudes',
                 'Math Enjoy',
                 'Math Strong',	
                 'Audibility',
                 'Conditions',			
                 'Visibility',				
                 'Temperature',				
                 'Safety',
                 'Technology',
                 'Comfort furniture')

lat_var_new <- c(lat_var_new_, outcomes_new[j])

####### Nodes

nodes <- get_nodes(eval(parse(text = fit_names[j]))) %>%
  dplyr::filter(name %in% lat_var_old)

# Ids to replace lat var names
vec <- c()
for(i in 1:length(nodes$name)){
  vec <- c(vec, which(nodes$name == lat_var_old[i]))
}


nodes_ <- nodes %>% mutate(label = replace(name, vec, rep(lat_var_new, each = 2)),
                          name = replace(name, vec, rep(lat_var_new, each = 2)))

nodes_$fill <- ifelse(nodes_$label %in% c('Teacher Quality', 
                                          'Conditions',				
                                          'Safety',
                                          'Technology',
                                          'Comfort furniture'), '#E6E600FF', 
                      ifelse(nodes_$label == 'Student Attitudes', '#D1E5F0', 
                      ifelse(nodes_$label %in% outcomes_new, '#EF8A62', 'white')))
#######  Edges

#edges <- get_edges(eval(parse(text = fit_names[j])), label = "est_sig")
#edges$is.sig <- grepl("\\*", edges$label)
#edges$is.neg <- grepl("\\-", edges$label)
edges <- parameterEstimates(eval(parse(text = fit_names[j])))
edges <- edges %>% filter(op %in% c('=~', '~'))
edges$rhs2 <- edges$rhs
for (i in 1:nrow(edges)){
  if (edges[i, 'op'] == '~'){
    edges[i, 'rhs'] <- edges[i, 'lhs']
    edges[i, 'lhs'] <- edges[i, 'rhs2']
 }
}

edges <- edges %>% select(lhs, rhs, group, est, pvalue) %>% filter(lhs %in% lat_var_old & rhs %in% lat_var_old)
names(edges) <- c('from', 'to', 'group', 'label', 'pvalue')
edges$arrow <- ifelse(edges$from == edges$to, 'none', 'last') 
edges <- edges %>% filter(arrow == 'last')
edges$group <- ifelse(edges$group == 2, 'Modern Teaching Style', 'Traditional Teaching Style')
edges$is.sig <- ifelse(edges$pvalue <= 0.1, T, F)
edges$is.neg <- ifelse(edges$label < 0, T, F)
edges$colour <- ifelse(edges$is.sig == TRUE & edges$is.neg == TRUE, 'darkred',
                       ifelse((edges$is.sig == TRUE & edges$is.neg == FALSE)|edges$from == 'Math_Teacher_Understandable',
                              'darkgreen', 'grey'))
edges$label <- round(edges$label, 2)
  
  
vec_from <- c()
vec_to <- c()

for(i in 1:length(edges$from)){
  vec_from <- c(vec_from, which(edges$from == lat_var_old[i]))
}
for(i in 1:length(edges$to)){
  vec_to <- c(vec_to, which(edges$to == lat_var_old[i]))
}



edges_ <- edges %>% mutate(from = case_when(from == 'Math_Teacher_Understandable' ~ 'Understandable',
                                            from == 'Teacher_Quality' ~ 'Teacher Quality',
                                            from == 'Math_Teacher_Orderliness' ~ 'Orderliness',
                                            from == 'MATH' ~ 'Math',
                                            from == 'Math_Important' ~ 'Math Important',
                                            from == 'Student_Attitudes' ~ 'Student Attitudes', 
                                            from == 'Math_Enjoy' ~ 'Math Enjoy', 
                                            from == 'SCIENCE' ~ 'Science',
                                            from == 'Math_Strong' ~ 'Math Strong',
                                            from == 'WELLNESS' ~ 'Wellness',
                                            from == 'n_tech' ~ 'Technology',
                                            from == 'Comfort_furniture' ~ 'Comfort furniture',
                                            from == 'APPLYING' ~ 'Applying',
                                            from == 'REASONING' ~ 'Reasoning',
                                            from == 'KNOWING' ~ 'Knowing',
                                            TRUE ~ from),
                           to = case_when(to == 'Math_Teacher_Understandable' ~ 'Understandable',
                                          to == 'Teacher_Quality' ~ 'Teacher Quality',
                                          to == 'Math_Teacher_Orderliness' ~ 'Orderliness',
                                          to == 'MATH' ~ 'Math',
                                          to == 'Math_Important' ~ 'Math Important',
                                          to == 'Student_Attitudes' ~ 'Student Attitudes', 
                                          to == 'Math_Enjoy' ~ 'Math Enjoy', 
                                          to == 'SCIENCE' ~ 'Science',
                                          to == 'Math_Strong' ~ 'Math Strong',
                                          to == 'WELLNESS' ~ 'Wellness',
                                          to == 'n_tech' ~ 'Technology',
                                          to == 'Comfort_furniture' ~ 'Comfort furniture',
                                          to == 'APPLYING' ~ 'Applying',
                                          to == 'REASONING' ~ 'Reasoning',
                                          to == 'KNOWING' ~ 'Knowing',
                                          TRUE ~ to)) %>% 
  filter(from %in% lat_var_new & to %in% lat_var_new & !arrow %in% c('none', 'both'))
 

edges_ <- edges_ %>% 
  mutate(alpha = case_when(from == "Teacher Quality" & to == 'Student Attitudes'~ 1,
                               from == "Conditions" & to == 'Student Attitudes'~ 1,
                               from == "Safety" & to == 'Student Attitudes'~ 1,
                               from == "Technology" & to == 'Student Attitudes'~ 1,
                               from == "Comfort furniture" & to == 'Student Attitudes'~ 1,
                               
                               from == "Student Attitudes" & to == 'Math'~ 1,
                               from == "Student Attitudes" & to == 'Science'~ 1,
                               from == "Student Attitudes" & to == 'Wellness'~ 1 ,
                           
                               from == "Student Attitudes" & to == 'Knowing'~ 1,
                               from == "Student Attitudes" & to == 'Reasoning'~ 1,
                               from == "Student Attitudes" & to == 'Applying'~ 1,
                               TRUE ~ 0.35))

edges_$size <- ifelse(edges_$alpha == 0.35, 1, 1.5)
edges_$label <- as.numeric(as.character(edges_$label))
edges_$label <- ifelse(edges_$size == 1, '', edges_$label)

####### Plotting

plot_multi <- graph_sem(eval(parse(text = fit_names[j])),
          nodes = nodes_,
          layout = get_layout("",	"",	"",	"",	"",	 'Math Important',	"",	 'Math Strong',	"",	"",
                              'Understandable',	"",	"",	 'Teacher Quality',	"",	"",	 'Math Enjoy',	"",	"",	"",
                              'Orderliness',	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	 '',
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              'Audibility',	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              'Visibility',	"",	"",	 'Conditions',	"",	"",	 'Student Attitudes',	"",	"",	"",
                              'Temperature',	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	 outcomes_new[j],
                              "",	"",	"",	 'Safety',	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	 'Technology',	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	 '',
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	 'Comfort furniture',	"",	"",	"",	"",	"",	"",
                              
                              rows = 17),
          edges = edges_,
          ellipses_width = 4, ellipses_height = 2,
          rect_height = 2, rect_width = 4,
          spacing_x = 2, spacing_y = 2, text_size = 4.5 )+ 
  theme(strip.text.x = element_text(size = 15),
        strip.background = element_rect(fill = 'lightblue'))+
  annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 23,
           alpha = .1, fill = "blue") +
  annotate('text', x = 0.5, y = 5, label = 'School Space',
           angle = 90, colour = 'darkred')

plot_multi_all <- c(plot_multi_all, list(plot_multi))

print(j)
}

#setwd("C:/Country/Russia/Data/SEASHELL/SEABYTE/edsus/Modeling")
plot_multi_all[[1]]
plot_multi_all[[2]]
plot_multi_all[[3]]
plot_multi_all[[4]]
plot_multi_all[[5]]


# Parameter Estimates

est1 <- as.data.frame(parameterEstimates(fit_sem1) %>% 
                        filter(lhs %in% c('Ind1', 'Ind2', 'Ind3', 'Ind4', 'Ind5')) %>%
                        select(lhs, est, se, pvalue) %>%
                        mutate(sig = ifelse(pvalue <= 0.001, '***', 
                                            ifelse(pvalue > 0.001 & pvalue <= 0.01, '**',
                                                   ifelse(pvalue > 0.01 & pvalue <= 0.05, '*',
                                                          ifelse(pvalue > 0.05 & pvalue <= 0.1, '.', '')))))%>%
                        select(lhs, est, se, sig)) 
est_multi1 <- as.data.frame(parameterEstimates(fit_sem_multi1) %>% 
                              filter(lhs %in% c('Ind1_1', 'Ind2_1', 'Ind3_1', 'Ind4_1', 'Ind5_1',
                                                'Ind1_2', 'Ind2_2', 'Ind3_2', 'Ind4_2', 'Ind5_2')) %>%
                              mutate(sig = ifelse(pvalue <= 0.001, '***', 
                                                  ifelse(pvalue > 0.001 & pvalue <= 0.01, '**',
                                                         ifelse(pvalue > 0.01 & pvalue <= 0.05, '*',
                                                                ifelse(pvalue > 0.05 & pvalue <= 0.1, '.', ''))))) %>%
                              select(lhs, est, se, sig)  %>%
                              mutate(group = substr(lhs, 6, 6)) %>%
                              mutate(lhs = substr(lhs, 1,4)) %>%
                              pivot_wider(id_cols = lhs, names_from = group, values_from = c(est, se, sig)) %>%
                              select(est_1, se_1, sig_1, est_2, se_2, sig_2))

est2 <- as.data.frame(parameterEstimates(fit_sem2) %>% 
                        filter(lhs %in% c('Ind1', 'Ind2', 'Ind3', 'Ind4', 'Ind5')) %>%
                        select(lhs, est, se, pvalue) %>%
                        mutate(sig = ifelse(pvalue <= 0.001, '***', 
                                            ifelse(pvalue > 0.001 & pvalue <= 0.01, '**',
                                                   ifelse(pvalue > 0.01 & pvalue <= 0.05, '*',
                                                          ifelse(pvalue > 0.05 & pvalue <= 0.1, '.', '')))))%>%
                        select(lhs, est, se, sig))
est_multi2 <- as.data.frame(parameterEstimates(fit_sem_multi2)  %>% 
                              filter(lhs %in% c('Ind1_1', 'Ind2_1', 'Ind3_1', 'Ind4_1', 'Ind5_1',
                                                'Ind1_2', 'Ind2_2', 'Ind3_2', 'Ind4_2', 'Ind5_2')) %>%
                              mutate(sig = ifelse(pvalue <= 0.001, '***', 
                                                  ifelse(pvalue > 0.001 & pvalue <= 0.01, '**',
                                                         ifelse(pvalue > 0.01 & pvalue <= 0.05, '*',
                                                                ifelse(pvalue > 0.05 & pvalue <= 0.1, '.', '')))))%>%
                              select(lhs, est, se, sig)  %>%
                              mutate(group = substr(lhs, 6, 6)) %>%
                              mutate(lhs = substr(lhs, 1,4)) %>%
                              pivot_wider(id_cols = lhs, names_from = group, values_from = c(est, se, sig)) %>%
                              select(est_1, se_1, sig_1, est_2, se_2, sig_2))

est3 <- as.data.frame(parameterEstimates(fit_sem3) %>% 
                        filter(lhs %in% c('Ind1', 'Ind2', 'Ind3', 'Ind4', 'Ind5')) %>%
                        select(lhs, est, se, pvalue) %>%
                        mutate(sig = ifelse(pvalue <= 0.001, '***', 
                                            ifelse(pvalue > 0.001 & pvalue <= 0.01, '**',
                                                   ifelse(pvalue > 0.01 & pvalue <= 0.05, '*',
                                                          ifelse(pvalue > 0.05 & pvalue <= 0.1, '.', '')))))%>%
                        select(lhs, est, se, sig))
est_multi3 <- as.data.frame(parameterEstimates(fit_sem_multi3)  %>% 
                              filter(lhs %in% c('Ind1_1', 'Ind2_1', 'Ind3_1', 'Ind4_1', 'Ind5_1',
                                                'Ind1_2', 'Ind2_2', 'Ind3_2', 'Ind4_2', 'Ind5_2')) %>%
                              mutate(sig = ifelse(pvalue <= 0.001, '***', 
                                                  ifelse(pvalue > 0.001 & pvalue <= 0.01, '**',
                                                         ifelse(pvalue > 0.01 & pvalue <= 0.05, '*',
                                                                ifelse(pvalue > 0.05 & pvalue <= 0.1, '.', '')))))%>%
                              select(lhs, est, se, sig)  %>%
                              mutate(group = substr(lhs, 6, 6)) %>%
                              mutate(lhs = substr(lhs, 1,4)) %>%
                              pivot_wider(id_cols = lhs, names_from = group, values_from = c(est, se, sig)) %>%
                              select(est_1, se_1, sig_1, est_2, se_2, sig_2))

est4 <- as.data.frame(parameterEstimates(fit_sem4) %>% 
                        filter(lhs %in% c('Ind1', 'Ind2', 'Ind3', 'Ind4', 'Ind5')) %>%
                        select(lhs, est, se, pvalue) %>%
                        mutate(sig = ifelse(pvalue <= 0.001, '***', 
                                            ifelse(pvalue > 0.001 & pvalue <= 0.01, '**',
                                                   ifelse(pvalue > 0.01 & pvalue <= 0.05, '*',
                                                          ifelse(pvalue > 0.05 & pvalue <= 0.1, '.', '')))))%>%
                        select(lhs, est, se, sig))
est_multi4 <- as.data.frame(parameterEstimates(fit_sem_multi4)  %>% 
                              filter(lhs %in% c('Ind1_1', 'Ind2_1', 'Ind3_1', 'Ind4_1', 'Ind5_1',
                                                'Ind1_2', 'Ind2_2', 'Ind3_2', 'Ind4_2', 'Ind5_2')) %>%
                              mutate(sig = ifelse(pvalue <= 0.001, '***', 
                                                  ifelse(pvalue > 0.001 & pvalue <= 0.01, '**',
                                                         ifelse(pvalue > 0.01 & pvalue <= 0.05, '*',
                                                                ifelse(pvalue > 0.05 & pvalue <= 0.1, '.', '')))))%>%
                              select(lhs, est, se, sig)  %>%
                              mutate(group = substr(lhs, 6, 6)) %>%
                              mutate(lhs = substr(lhs, 1,4)) %>%
                              pivot_wider(id_cols = lhs, names_from = group, values_from = c(est, se, sig)) %>%
                              select(est_1, se_1, sig_1, est_2, se_2, sig_2))

est5 <- as.data.frame(parameterEstimates(fit_sem5) %>% 
                        filter(lhs %in% c('Ind1', 'Ind2', 'Ind3', 'Ind4', 'Ind5')) %>%
                        select(lhs, est, se, pvalue) %>%
                        mutate(sig = ifelse(pvalue <= 0.001, '***', 
                                            ifelse(pvalue > 0.001 & pvalue <= 0.01, '**',
                                                   ifelse(pvalue > 0.01 & pvalue <= 0.05, '*',
                                                          ifelse(pvalue > 0.05 & pvalue <= 0.1, '.', '')))))%>%
                        select(lhs, est, se, sig))
est_multi5 <- as.data.frame(parameterEstimates(fit_sem_multi5)  %>% 
                              filter(lhs %in% c('Ind1_1', 'Ind2_1', 'Ind3_1', 'Ind4_1', 'Ind5_1',
                                                'Ind1_2', 'Ind2_2', 'Ind3_2', 'Ind4_2', 'Ind5_2')) %>%
                              mutate(sig = ifelse(pvalue <= 0.001, '***', 
                                                  ifelse(pvalue > 0.001 & pvalue <= 0.01, '**',
                                                         ifelse(pvalue > 0.01 & pvalue <= 0.05, '*',
                                                                ifelse(pvalue > 0.05 & pvalue <= 0.1, '.', '')))))%>%
                              select(lhs, est, se, sig)  %>%
                              mutate(group = substr(lhs, 6, 6)) %>%
                              mutate(lhs = substr(lhs, 1,4)) %>%
                              pivot_wider(id_cols = lhs, names_from = group, values_from = c(est, se, sig)) %>%
                              select(est_1, se_1, sig_1, est_2, se_2, sig_2))

est <- rbind.data.frame(est1, est2, est3, est4, est5)
est_multi <- rbind.data.frame(est_multi1, est_multi2, est_multi3, est_multi4, est_multi5)
est_all <- cbind.data.frame(est, est_multi)
