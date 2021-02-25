# tch-quality_sch-envir1b.R

# Teaching Styles, Teaching Quality, School Environmemt, Students' Beliefs, and Learning Outcomes
# Graphing

library(tidySEM)

lat_var_old <- c('Math_Teacher_Understandable',
            'Teacher_Quality',			
            'Math_Teacher_Oderliness',				
            'MATH',
            'Math_Important',
            'Student_Attitudes',			
            'Math_Enjoy',
            'SCIENCE',
            'Math_Strong',				
            'WELLNESS',
            'Audibility',
            'Conditions',			
            'Visibility',				
            'Temperature',				
            'Safety',
            'psf_n_tech',
            'Comfort_furniture'
)

lat_var_new <- c('Understandable',
                 'Teacher Quality', 
                 'Oderliness',
                 'Math',
                 'Math Important',
                 'Student Attitudes',
                 'Math Enjoy',
                 'Science',
                 'Math Strong',				
                 'Wellness',
                 'Audibility',
                 'Conditions',			
                 'Visibility',				
                 'Temperature',				
                 'Safety',
                 'Technology',
                 'Comfort furniture')

# Nodes

nodes <- get_nodes(fit_sem_multi) %>%
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
                      ifelse(nodes_$label %in% c('Math', 'Science', 'Wellness'), '#EF8A62', 'white')))
# Edges

# Ids to replace lat var names
edges <- get_edges(fit_sem_multi)
edges$is.sig <- grepl("\\*", edges$label)
edges$is.neg <- grepl("\\-", edges$label)
edges$colour <- ifelse(edges$is.sig == TRUE & edges$is.neg == TRUE, 'darkred',
                       ifelse((edges$is.sig == TRUE & edges$is.neg == FALSE)|edges$from == 'Math_Teacher_Understandable',
                              'darkgreen', 'grey'))
edges <- edges %>% filter(!colour == 'grey')

#edges$alpha  <- NULL
label <- data.frame(label = unlist(strsplit(edges$label, "[*]"))) %>% filter(label != '')
edges$label <- NULL
edges <- cbind.data.frame(edges, label)

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
                                            from == 'Math_Teacher_Oderliness' ~ 'Oderliness',
                                            from == 'MATH' ~ 'Math',
                                            from == 'Math_Important' ~ 'Math Important',
                                            from == 'Student_Attitudes' ~ 'Student Attitudes', 
                                            from == 'Math_Enjoy' ~ 'Math Enjoy', 
                                            from == 'SCIENCE' ~ 'Science',
                                            from == 'Math_Strong' ~ 'Math Strong',
                                            from == 'WELLNESS' ~ 'Wellness',
                                            from == 'psf_n_tech' ~ 'Technology',
                                            from == 'Comfort_furniture' ~ 'Comfort furniture',
                                            TRUE ~ from),
                           to = case_when(to == 'Math_Teacher_Understandable' ~ 'Understandable',
                                          to == 'Teacher_Quality' ~ 'Teacher Quality',
                                          to == 'Math_Teacher_Oderliness' ~ 'Oderliness',
                                          to == 'MATH' ~ 'Math',
                                          to == 'Math_Important' ~ 'Math Important',
                                          to == 'Student_Attitudes' ~ 'Student Attitudes', 
                                          to == 'Math_Enjoy' ~ 'Math Enjoy', 
                                          to == 'SCIENCE' ~ 'Science',
                                          to == 'Math_Strong' ~ 'Math Strong',
                                          to == 'WELLNESS' ~ 'Wellness',
                                          to == 'psf_n_tech' ~ 'Technology',
                                          to == 'Comfort_furniture' ~ 'Comfort furniture',
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
                               from == "Student Attitudes" & to == 'Wellness'~ 1,
                               TRUE ~ 0.35))

edges_$size <- ifelse(edges_$alpha == 0.35, 1, 1.5)
edges_$label <- as.numeric(as.character(edges_$label))
edges_$label <- ifelse(edges_$size == 1, '', edges_$label)

plot_multi <- graph_sem(fit_sem_multi,
          nodes = nodes_,
          layout = get_layout("",	"",	"",	"",	"",	 'Math Important',	"",	 'Math Strong',	"",	"",
                              'Understandable',	"",	"",	 'Teacher Quality',	"",	"",	 'Math Enjoy',	"",	"",	"",
                              'Oderliness',	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	 'Math',
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              'Audibility',	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              'Visibility',	"",	"",	 'Conditions',	"",	"",	 'Student Attitudes',	"",	"",	"",
                              'Temperature',	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	 'Science',
                              "",	"",	"",	 'Safety',	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	 'Technology',	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	 'Wellness',
                              "",	"",	"",	"",	"",	"",	"",	"",	"",	"",
                              "",	"",	"",	 'Comfort furniture',	"",	"",	"",	"",	"",	"",
                              
                              rows = 17),
          edges = edges_,
          ellipses_width = 4, ellipses_height = 2,
          spacing_x = 2, spacing_y = 2, text_size = 4.5 )+ 
  theme(strip.text.x = element_text(size = 15),
        strip.background = element_rect(fill = 'lightblue'))+
  annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 23,
           alpha = .1, fill = "blue") +
  annotate('text', x = 0.5, y = 5, label = 'School Learning Environment',
           angle = 90, colour = 'darkred')

plot_multi



















