library(tidyverse)
library(scales)
library(superheat)
library(ggrepel)
library(gridExtra)
library(ggthemes)
library(gt)
library(wesanderson)
library(ggpubr)

load('data.Rdata')


retrieve_table <- function(result,set){
  tables %>%
    filter(name==result, type==set) %>%
    unnest(cols = c(table)) %>%
    select(-c(paths, name, type))
  
}


# General distribution ----------------------------------------------------

# general_dist %>%
#   mutate(group = case_when(group =='Hispanic' ~'Latinx',
#                            TRUE ~ group)) %>% 
#     pivot_longer(census:health, names_to = 'type', values_to = 'proportion') %>%
#     mutate(type = fct_recode(type,
#                              'Census' = 'census', 
#                              'WOS\nUS' = 'full_dataset', 
#                              'Social\nSciences'='social_sciences',
#                              'Health'='health')) %>% 
#     ggplot(aes(group, proportion,fill = gender, label = scales::number(proportion*100, accuracy = 1.1)))+
#     geom_col(width = 0.5,position = 'dodge')+
#     geom_text(position = position_dodge(width = 1),vjust=-0.2)+
#     facet_grid(.~type)+
#     theme_minimal() +
#     labs(x='',y='')+
#   # scale_fill_manual(values = wes_palette("Zissou1", 2, type = "continuous"))+
#   scale_fill_grey()+
#   # scale_fill_viridis_d(option = "C")+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = pretty_breaks(3),
#                      limits = c(0,.50))+
#     theme(legend.position = c(0.095,.84),
#           legend.background = element_rect(color= 'grey30'),
#           legend.text = element_text(size=18, color= 'grey30'),
#           strip.placement = 'outside',
#           panel.background = element_rect(fill = NA, color = 'grey95'),
#           strip.background = element_rect(fill = 'grey95', color = NA), 
#           strip.text = element_text(size = 16),
#           axis.text.x = element_text(size = 16, angle = 25),
#           axis.text.y = element_text(size = 16),
#           legend.title = element_blank())

general_dist %>%
  pivot_longer(c(census:health,norm_wos), names_to = 'type', values_to = 'proportion') %>%
  mutate(type = factor(type,levels = c("census",'full_dataset','norm_wos','social_sciences','health')),
         type = fct_recode(type,
                           'a)\nCensus\n' = 'census', 
                           'b)\nWOS\nUS' = 'full_dataset', 
                           'c)\nWOS\nUS Residents' = 'norm_wos',
                           'd)\nSocial\nSciences'='social_sciences',
                           'e)\nHealth\n'='health')) %>% 
  ggplot(aes(group, proportion,fill = gender, label = scales::number(proportion*100, accuracy = 0.1)))+
  geom_col(width = 0.5,position = 'dodge')+
  geom_text(position = position_dodge(width = 1.1),vjust=-0.2)+
  facet_grid(.~type)+
  theme_minimal() +
  labs(x='',y='')+
  # scale_fill_manual(values = wes_palette("Zissou1", 2, type = "continuous"))+
  scale_fill_grey()+
  # scale_fill_viridis_d(option = "C")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = pretty_breaks(3),
                     limits = c(0,.50))+
  theme(legend.position = c(0.095,.84),
        legend.background = element_rect(color= 'grey30'),
        legend.text = element_text(size=18, color= 'grey30'),
        strip.placement = 'outside',
        panel.background = element_rect(fill = NA, color = 'grey30'),
        strip.background = element_rect(fill = 'grey95', color = NA), 
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 25),
        axis.text.y = element_text(size = 16),
        legend.title = element_blank())

ggsave('figures/plots/general_dist.png',width = 12, height = 7, dpi=300)



# Fields ------------------------------------------------------------------

## Heatmap
theme_empty <- function(){theme(axis.line=element_blank(),
                                # axis.text.x=element_blank(),
                                # axis.text.y=element_blank(),
                                # axis.ticks=element_blank(),
                                # axis.title.x=element_blank(),
                                # axis.title.y=element_blank(),
                                legend.position="none",
                                panel.background=element_blank(),
                                panel.border=element_blank(),
                                panel.grid.major =  element_line(colour = "grey",linetype = 'dotted'),
                                # panel.grid.major=element_blank(),
                                panel.grid.minor=element_blank(),
                                plot.background=element_blank())}

heatmap_plot_disciplines <- function(order = 'Black_Women'){
  
  groups_order <- c("White_Men","Black_Men","Latinx_Men","Asian_Men","Asian_Women","Latinx_Women","Black_Women","White_Women")
  
  df <- discipline_gender_agg %>% 
    select(-median_citations, -gender_prob,-racial_group_prob) %>% 
    mutate(gender = case_when(gender == 'M' ~ 'Men',
                              gender == 'F' ~ 'Women'),
           group = case_when(group =='Hispanic' ~'Latinx',
                                     TRUE ~ group),
           group = paste(str_to_title(group),gender, sep = '_'),
           group = factor(group, levels = groups_order)) %>%
    mutate(group_prob = sum(joint_prob)) %>% 
    group_by(EDiscipline,group) %>% 
    summarise(marginal_by_discipline = sum(joint_prob)/unique(EDiscipline_prob)) %>%
    group_by(group) %>% 
    mutate(marginal_by_discipline = scale(marginal_by_discipline)[,1])
           # EDiscipline = stringi::stri_replace_last(str = EDiscipline,replacement = '\n',regex =  ' ') )
  
  
  disciplines_order <- df %>% 
    filter(group == order) %>% 
    arrange(marginal_by_discipline) %>% pull(EDiscipline)
  
  
  disciplines_labels <- df %>% 
    filter(group == order) %>% 
    arrange(marginal_by_discipline) %>% 
    mutate(labels = case_when(
      EDiscipline == "Mathematics" ~ "Mathematics",
      EDiscipline == "Engineering and Technology" ~ "Eng. & Tech.",
      EDiscipline == "Chemistry" ~ "Chemistry",
      EDiscipline == "Earth and Space" ~ "Earth & Sp.",
      EDiscipline == "Biomedical Research"  ~ "Biomed. Res.",
      EDiscipline == "Clinical Medicine" ~ "Cli. Med.",
      EDiscipline == "Social Sciences" ~ "Soc. Sci.",
      EDiscipline == "Biology" ~"Biology",
      EDiscipline == "Humanities" ~ "Humanities",
      EDiscipline == "Professional Fields"  ~ "Prof. Fields.",
      EDiscipline == "Psychology" ~ "Psychology",
      TRUE ~ EDiscipline
    )) %>% pull(labels)
    
    
  df <- df %>%
    mutate(EDiscipline = factor(EDiscipline, levels = disciplines_order, labels = disciplines_labels),
           group = str_replace(group,'_','\n'),
           group = factor(group, levels = unique(group)))
  
  pl_data <-     discipline_gender_agg %>%
    mutate(#EDiscipline = stringi::stri_replace_last(str = EDiscipline,replacement = '\n',regex =  ' '),
           EDiscipline = factor(EDiscipline, levels = disciplines_order, labels = disciplines_labels)) %>% 
    group_by(EDiscipline, ESpecialite) %>% 
    summarise(mean_citations = sum(mean_citations*marginal_by_speciality)) 
  
  plot_left <- pl_data %>% 
    ggplot(aes(mean_citations,EDiscipline))+
    geom_boxplot(fill = 'grey') +
    labs(x = 'Citations by\nDiscipline', y = '')+
    theme_empty()+
    # geom_segment(data = data.frame(y = seq(0.5, length(unique(pl_data$EDiscipline))+0.5, 1),
    #                                xmin = -15, xmax = 50),
    #              aes(y = y, yend = y, x = xmin, xend = xmax),
    #              inherit.aes = FALSE,
    #              color = "gray60")+
    geom_hline(yintercept = seq(0.5, length(unique(pl_data$EDiscipline))+0.5, 1), color='gray30')+
    scale_x_continuous(minor_breaks = seq(0 , 100, 5), breaks = seq(0, 100, 10))+
    theme(text = element_text(size=22, colour = 'grey30'),
          axis.title.x = element_text(vjust = -10),
          plot.margin = margin(t = 5.5, r=0,b=87.25,l=-40),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank()
          # axis.ticks.length.x = unit(2.5, "cm")) 
    ) 
  
  plot_top <-
    discipline_gender_agg %>%
    group_by(gender,group) %>% 
    mutate(marginal_by_group = freq/sum(freq)) %>% 
    summarise(mean_citations = sum(mean_citations*marginal_by_group),
              mean_rel_citations = sum(mean_rel_citations*marginal_by_group)) %>% 
    ungroup() %>% 
    mutate(gender = case_when(gender == 'M' ~ 'Men',
                              gender == 'F' ~ 'Women'),
           group = paste(str_to_title(group),gender, sep = '_'),
           group = factor(group, levels = groups_order, labels = str_replace(groups_order,'_','\n'))) %>%
    select(-gender) %>%
    ggplot(aes(group))+
    # geom_col(aes(y = mean_citations))+
    geom_point(aes(y = mean_citations,color = 'Citations'), size =5, shape=19)+
    geom_point(aes(y = mean_rel_citations*15, color = 'Normalized\nCitations'), size =5, shape=18)+
      scale_color_manual(values = c('#3B9AB2','#F21A00'))+
      scale_y_continuous(
        name = "Citations",
        sec.axis = sec_axis( trans=~./15, name="Normalized\nCitations",breaks = c(1.07,1.2,1.34,1.47)))+
    labs(color='', x ='')+
    # labs(y = 'Citations', x = '')+
    theme_empty()+
    theme(legend.position = 'none',
          text = element_text(size=19, colour = 'grey30'),
          axis.title.y.left = element_text(angle = 90,vjust = 0.5, color ='#3B9AB2' ),
          axis.title.y.right = element_text(angle = 90,vjust = 0.5, color ='#F21A00' ),
          panel.grid.major.y =  element_line(colour = "grey",linetype = 'dotted'),
          plot.margin = margin(t = 0, r=-60,b=-25,l=120),
          # plot.margin = margin(t = 0, r=-125.5,b=-25,l=46),
          panel.grid.major.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank())
  
  main_plot <- ggplot(df, aes(group, EDiscipline, fill = marginal_by_discipline))+
    # geom_hline(yintercept = seq(0.5, length(unique(df$EDiscipline))+0.5, 1))+
    geom_tile(color='grey30')+
    # scale_fill_viridis_c()+
    scale_fill_gradientn(colours =
                           wesanderson::wes_palette("Zissou1", 10, type = "continuous"),
                           # viridis_pal(option = 'D')(7),
                         # brewer_pal( type = 'seq','Greens')(5),
                         breaks = c(-2,2),
                         labels = c('Under\nrepresentation', 'Over\nrepresentation'))+
    labs(fill='', x='', y= '')+
    guides( fill = guide_colorbar(title = '',barwidth = unit(150, units = 'pt')))+
    theme_minimal()+
    theme(text = element_text(size=18,color='grey30'),
          legend.position = 'bottom',
          # axis.text.y = element_text(size=20,color = c('grey15', 'grey45'), face = c('plain','bold')),
          axis.text.y = element_text(size=20),
          axis.text.x = element_text(size=24),
          panel.grid.major.y = element_blank()
          # panel.border = element_rect(color = "grey30", fill = NA, size = 1)
          )
  
  plot_empty <-  ggplot(df, aes(group, EDiscipline, fill = marginal_by_discipline)) +
    geom_blank()+
    theme_void()
  
  # lay <- rbind(c(1,1,1,1,1,1,1,NA),
  #              c(2,2,2,2,2,2,3,3),
  #              c(2,2,2,2,2,2,3,3),
  #              c(2,2,2,2,2,2,3,3),
  #              c(2,2,2,2,2,2,3,3),
  #              c(2,2,2,2,2,2,3,3))
  # 
  # grid.arrange(plot_top, main_plot, plot_left,
  #              layout_matrix = lay)
  grid.arrange(plot_top, plot_empty, main_plot, plot_left,
               ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 6))
}


png('figures/plots/heatmap_disciplines.png', height = 4000, width = 5000, res = 300)
heatmap_plot_disciplines()
dev.off()

# Social Sciences ---------------------------------------------------------

intersectional_plot <- function(data = 'socsci'){
  
  if (data=='socsci') {
    
    df <- retrieve_table(result = 'marginal_by_topic',set = 'socsci')
    df2 <- retrieve_table(result = 'joint_prob',set = 'socsci')
    
    mean_citations <- retrieve_table(result = 'citations', set = 'socsci') %>%
      left_join(retrieve_table('marginal_by_topic',set='socsci') %>%  select(topic, group, gender, marginal_by_topic=prob)) %>% 
      group_by(topic) %>% 
      summarise(mean_citations = sum(mean*marginal_by_topic))
    
    median_citations <- retrieve_table(result = 'citations', set = 'socsci') %>% 
      select(-c(top_words,prob,mean,sd)) %>%
      pivot_wider(names_from = c(group,gender),
                  values_from = median)
    
      labels <- read_csv('figures/plots/tables/top_words_socsci.csv')
  }
  if (data=='health') {
    df <- retrieve_table(result = 'marginal_by_topic',set = 'health')
    df2 <- retrieve_table(result = 'joint_prob',set = 'health')
    mean_citations <- retrieve_table(result = 'citations', set = 'health') %>%
      left_join(retrieve_table('marginal_by_topic',set='health') %>%  select(topic, group, gender, marginal_by_topic=prob)) %>% 
      group_by(topic) %>% 
      summarise(mean_citations = sum(mean*marginal_by_topic))

    median_citations <- retrieve_table(result = 'citations', set = 'health') %>% 
      select(-c(top_words,prob,mean,sd)) %>%
      pivot_wider(names_from = c(group,gender),
                  values_from = median)

    labels <- read_csv('figures/plots/tables/top_words_health.csv')
  }
  
  feminisation_by_topic <- df %>% 
    group_by(topic,gender,topic_proportion) %>% 
    summarise(feminisation = sum(prob)) %>% 
    filter(gender == 'Women') %>% 
    select(-gender)
  
  group_cv <- df %>% 
    group_by(group,topic) %>% 
    summarise(prob = sum(prob)) %>% 
    group_by(group) %>% 
    summarise(cv = sd(prob)/mean(prob))
  
  group_average <- df2 %>% 
    group_by(group) %>% 
    summarise(prob = sum(prob))
  #pull(prob)
  
  women_average <- df2 %>% 
    filter(gender=='Women') %>% 
    summarise(prob = sum(prob)) %>% pull(prob)
  
  df <- df %>%
    group_by(topic,group) %>% 
    summarise(prob = sum(prob),
              top_words = unique(top_words)) %>% 
    left_join(feminisation_by_topic, by = "topic") %>% 
    #left_join(topic_proportion, by = "topic") %>% 
    left_join(median_citations, by = "topic") %>% 
    left_join(mean_citations, by='topic') %>% 
    left_join(group_cv, by = "group") %>% 
    mutate(group_label = paste(group, 'CV:',round(cv, digits = 3)))
  
    group_labels <- df %>%
      ungroup() %>% 
      select(group_label,group) %>% 
      distinct() %>%
      mutate(group_label = str_replace(group_label,pattern = 'Hispanic', 'Latinx')) %>% 
      pull(group_label,group)

  topic_labels <- labels %>%
    mutate(topic = as_factor(topic)) %>%
    left_join(df, by = c("topic", "top_words"))
  
df %>% 
  # group_by(group) %>% 
  # mutate(mean_citations = scale(mean_citations,center = TRUE, scale = TRUE)[,1]) %>% 
  ggplot(., aes(prob,feminisation,size= topic_proportion, color=mean_citations)) +
      geom_point()+
      geom_point(color='grey30',shape=21)+
      facet_wrap(.~group,scales = "free_x", ncol = 2, labeller = labeller(group=group_labels)) +
      geom_vline(data= group_average, aes(xintercept = prob), color='grey70')+
      geom_hline(yintercept = women_average, color='grey70')+
      geom_label_repel(data = topic_labels, aes(label = label),
                       min.segment.length = 0,
                       color = 'black', size=4, fill=alpha(colour = 'white',alpha = 0.8))+
      # geom_text_repel(data = topic_labels, aes(label = label), color = 'black', size=4)+
      # scale_color_viridis_c()+
      scale_color_gradientn(colors =
                            # colours =
                             # viridis_pal(option = 'D')(7),
                           # brewer_pal( type = 'seq','YlGn')(5)
                           # brewer_pal( type = 'seq','Blues')(5)
                             wesanderson::wes_palette("Zissou1", 3, type = "continuous")
                           )+
      scale_y_continuous(labels = percent_format(accuracy = 1))+
      scale_x_continuous(labels = percent_format(accuracy = 1), n.breaks = 5)+
      labs(x= 'racial group proportion', y = 'feminization')+
      theme_minimal()+
      guides(size=FALSE, color = guide_colorbar(title = 'Mean citations',barwidth = unit(150, units = 'pt')))+
      theme(text = element_text(size=18),
            panel.grid.minor = element_blank(),
            legend.position = 'bottom',
            panel.border = element_rect(color = "grey50", fill = NA, size = 1), )
}

citation_distribution <- function(data= 'socsci'){
  
  
  df <- retrieve_table(result = 'citations', set = data) %>%
    left_join(retrieve_table('marginal_by_topic',set=data) %>%  
                select(topic, group, gender, marginal_by_topic=prob, topic_proportion)) %>% 
    group_by(topic) %>% 
    summarise(mean_citations = sum(mean*marginal_by_topic),
              topic_proportion = unique(topic_proportion)) 
    
    res1 <- weighted.mean(x = df$mean_citations,
                  w = df$topic_proportion)
    
    res2 <- Hmisc::wtd.quantile(x = df$mean_citations,
                        weights = df$topic_proportion,
                        normwt = TRUE,probs = seq(0,1,0.1))
    list('mean'=res1, 'quantiles'= res2)
  }


intersectional_plot('socsci')
ggsave('figures/plots/intersectional_plot_soscsci.png', width = 10, height = 8, dpi = 300)

read_csv('figures//plots/tables/top_words_socsci.csv') %>% gt::gt()
citation_distribution(data = 'socsci')

# top_words_socsci %>%
#   filter(topic%in%c(160,36,83, 218, 210, 297,110,14,18, 204, 273,251, 51, 28, 201, 77, 267, 1,254, 17))  %>%
#   select(Topic=topic, 'Top Words'=top_words) %>%
#   write_csv('figures/plots/tables/top_words_socsci.csv')


intersectional_plot('health')
ggsave('figures/plots/intersectional_plot_health.png', width = 10, height = 8, dpi = 300)


read_csv('figures/plots/tables/top_words_health.csv') %>% gt::gt()

citation_distribution(data = 'health')
# top_words_health %>%
#   filter(topic%in%c(11,14,16,57,61,66, 72, 73,83,87, 92,104,116, 119,132, 160,187,196)) %>%
#   select(Topic=topic, 'Top Words'=top_words) %>%
#   write_csv('figures/plots/tables/top_words_health.csv')

## Heatmap social sciences

heatmap_plot <- function(res='dist_diff_topic', dataset='socsci', group_order='Black_Women'){
  # order of the heatmap. Possible values:
  #"White_Men","Hispanic_Men","Black_Men","Asian_Men","White_Women","Hispanic_Women","Black_Women","Asian_Women" 
  #
  
  col_order <- c("White\nMen","Black\nMen","Latinx\nMen","Asian\nMen",
                 "Asian\nWomen", "Latinx\nWomen","Black\nWomen", "White\nWomen" )
  
  marginal_by_topic <- retrieve_table('marginal_by_topic', set=dataset)
  marginal_by_group <- retrieve_table('marginal_by_group', set=dataset)
  
  mean_citations <- retrieve_table(result = 'citations', set = dataset) %>% 
    select(-c(top_words,prob,median,sd)) %>% 
    left_join(marginal_by_topic, by = c("topic", "group", "gender")) %>% 
    group_by(topic) %>%
    summarise(mean_citations = sum(mean*prob))
  
  mean_citations_by_group <- retrieve_table(result = 'citations', set = dataset) %>% 
    select(-c(top_words,prob,median,sd)) %>% 
    left_join(marginal_by_group, by = c("topic", "group", "gender")) %>% 
    group_by(group,gender) %>%
    summarise(mean_citations = sum(mean*prob)) %>% 
    ungroup() %>% 
    arrange(gender, group) %>% 
    mutate(group = case_when(group == 'Hispanic'~'Latinx',T ~ group),
           group = paste(group, gender, sep = '\n'),
           group = factor(group, levels = col_order)) %>% 
    select(-gender) %>% 
    arrange(group)
  
  topics_group_matrix <-  retrieve_table(result = res,set = dataset) %>% 
    mutate(group = case_when(group == 'Hispanic'~'Latinx',T ~ group),
      group = paste(group, gender, sep = '_')) %>% 
    select(topic, group, prob) %>% 
    pivot_wider(names_from = 'group', values_from = 'prob') %>%
    select(-topic)
  
  topics_group_matrix <- topics_group_matrix %>% select_all(.funs = ~ gsub("_", "\n", .))
  
    topics_group_matrix <- topics_group_matrix[col_order]
    
    group_order <- gsub('_','\n',group_order)
    
    superheat(topics_group_matrix,
              yr = mean_citations$mean_citations,
              yr.point.alpha = 0.5, 
              yr.point.size = 0.75, 
              yr.axis.size = 22,
              yr.line.col = "tomato3",
              yr.plot.type = "scattersmooth", 
              yr.axis.name = "Average\nCitations", 
              yr.axis.name.size = 22,
              order.rows =order(topics_group_matrix[group_order]),
              
              heat.pal =wesanderson::wes_palette("Zissou1", 10, type = "continuous"),
              #yt = mean_citations_by_group$mean_citations, 
              #yt.plot.type = 'bar',
              #yt.axis.name = 'Average citations\nby group', yt.axis.name.size = 15,
              legend = FALSE,
              bottom.label.text.size = 8.5)
    
}

png('figures/plots/heatmap_socsci_Hispanic_Women.png', height = 3000, width = 4000, res = 300)
heatmap_plot(res='dist_diff_topic', dataset='socsci', group_order = 'Latinx_Women')
dev.off()

png('figures/plots/heatmap_socsci_Asian_Men.png', height = 3000, width = 4000, res = 300)
heatmap_plot(res='dist_diff_topic', dataset='socsci', group_order = 'Asian_Men')
dev.off()

png('figures/plots/heatmap_health_Hispanic_Women.png', height = 3000, width = 4000, res = 300)
heatmap_plot(res='dist_diff_topic', dataset='health', group_order = 'Latinx_Women')
dev.off()

png('figures//plots/heatmap_health_White_Men.png', height = 3000, width = 4000, res = 300)
heatmap_plot(res='dist_diff_topic', dataset='health', group_order = 'White_Men')
dev.off()

# citations_controlled ----------------------------------------------------

line_plot <-function(dataset='socsci', legend= FALSE){
  
  marginal_by_topic <- retrieve_table('marginal_by_topic', set=dataset)
  
  topics_order <- retrieve_table(result = 'citations', set = dataset) %>% 
    select(-c(top_words,prob,median,sd)) %>% 
    left_join(marginal_by_topic, by = c("topic", "group", "gender")) %>% 
    group_by(topic) %>%
    summarise(mean_citations = sum(mean*prob)) %>% 
    mutate(topic = fct_reorder(topic, mean_citations)) %>% 
    pull(topic) %>% levels()
  
  
  plt <- retrieve_table(result = 'citations',set = dataset) %>% 
    mutate(topic = fct_relevel(topic,topics_order)) %>% 
    arrange(topic) %>% 
    mutate(race = group,
           group = case_when(group =='Hispanic' ~'Latinx',
                             TRUE ~ group),
           group = paste(group,gender)) %>% 
    ggplot(aes(topic, mean, group = group, color = group)) +
    geom_smooth(size=2,se = TRUE)+
    geom_point(alpha=0.25)+
    labs(x = 'Topics sorted by average citations', y = 'Average citations by group', color='')+
    scale_color_brewer(palette = 'Paired')+
    theme_minimal()+
    guides(color=guide_legend(ncol=2))+
    theme(text = element_text(size=34,color= 'grey30'),
          axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = c(0.33,.83),
          legend.text = element_text(size = 34),
          legend.background = element_rect(color= 'grey30'),
          legend.direction = 'horizontal',
          plot.subtitle = element_text(hjust = 0.5, vjust = -6, size=34),
          legend.title = element_blank())
  
  if (!legend) {
    plt <- plt +
      theme(legend.position = 'none')
  }else{
    plt <- plt +
      theme(axis.title.x = element_blank())
  }
  plt  
  }
line_plot(dataset='socsci', legend = FALSE)
ggsave('figures/plots/lineplot_socsci.png', width = 16, height = 12, dpi = 300)

line_plot(dataset='health', legend = TRUE)
ggsave('figures/plots/lineplot_health.png', width = 16, height = 12, dpi = 300)

#merged plot

lp_socsci <- line_plot(dataset='socsci')
lp_health <- line_plot(dataset='health')

lp_socsci <- lp_socsci +
  theme(legend.position =c(0.5,-.12))+
  labs(x= '', y='', subtitle = 'Social Sciences')

lp_health <- lp_health + 
  theme(legend.position ='none')+
  labs( y='', subtitle = 'Health')


fig <- ggarrange(lp_socsci,lp_health,ncol = 1)

fig <- annotate_figure(fig,
                left = text_grob('Average citations by group', color = "grey30", rot = 90, size=26))

ggsave('figures/plots/lineplot.png', fig, width = 12, height = 12, dpi = 300)

# co-authors --------------------------------------------------------------

heatmap_collaborations <- function(data_type = 'first_last',value_type = 'diff'){
  
  
  df <- collaborations %>% 
    filter(d_type==data_type, 
           v_type==value_type)
  
  
  if (data_type == 'all' & value_type == 'diff') {
    norm_exp = -2
  }else{norm_exp = 1}
    
  ngroups <- 8
  if (data_type == 'first_last') {
    boxdata <- tibble(xmin = c(ngroups-2.5, 3.5, 0.5),
                      xmax = c(ngroups + 0.5, 5.5, 3.5),
                      ymin = c(0.5, 3.5, ngroups-2.5),
                      ymax = c(3.5, 5.5, ngroups +0.5))
  }
  if (data_type == 'all') {
    boxdata <- tibble(xmin = c(ngroups-3.5, 0.5),
                      xmax = c(ngroups + 0.5, 3.5),
                      ymin = c(0.5, ngroups-2.5),
                      ymax = c(4.5, ngroups +0.5))
  }
  
    g <-
      ggplot(df, aes(to, from, fill=value)) +
      geom_tile(color='grey30')+
      scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 9, type = "continuous"),
                           breaks = c(min(df$value),max(df$value)),
                           trans = modulus_trans(norm_exp),
                           labels = c('Under\nrepresentation', 'Over\nrepresentation'))+
      guides( fill = guide_colorbar(title = '',barwidth = unit(150, units = 'pt')))+
      geom_text(size =6, aes(label=percent(value,accuracy = 0.1))) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), data = boxdata, inherit.aes = FALSE,
                fill = "transparent", color = "black", size = 1.5) +
      theme_minimal()+
      scale_x_discrete(position = "top") +
      theme(text = element_text(size=18,color='grey15', face='bold'),
            legend.position = 'bottom',
            axis.text = element_text(size=16,color = c('grey15', 'grey45'), face = c('plain','bold')),
            panel.grid.major.y = element_blank()
            # panel.border = element_rect(color = "grey30", fill = NA, size = 1)
      )
    
    if (data_type == 'first_last') {
      
      g <- g +
        labs(fill = '', x= 'Last author', y = 'First author')
    }
    if (data_type == 'all') {
      g <- g +
        labs(fill = '', x= 'All co-authors', y ='All co-authors')
    }

  return(g)  
  
}


heatmap_collaborations(data_type = 'first_last', value_type = 'diff')
ggsave('figures/plots/heatmap_collaborations_first_last.png', width = 10, height = 8, dpi = 300)


heatmap_collaborations(data_type = 'all', value_type = 'diff')
ggsave('figures/plots/heatmap_collaborations_all.png', width = 10, height = 8, dpi = 300)


# LDA robustness  --------------------------------------------------------------




lda_robustness_plot <- function(scaled=TRUE){
  
  if (scaled) {
    df <- lda_robustness_scaled
  }
  if (!scaled) {
    df <- lda_robustness
  }
  
  df <- df %>% 
    select(-X1) %>% 
    as.data.frame(.)
  
  names(df) <- c(1:10, 'health', 'random')
  rownames(df) <- c(1:10, 'health', 'random')
  
  
  superheat(df,
            #bottom.label.text.size = 4,
            bottom.label.size = .15,
            left.label.size = 0.1,
            bottom.label.text.angle = c(rep(0,10),45,45))
}


png('figures/plots/lda_robustness.png', height = 3000, width = 3000, res = 300)
lda_robustness_plot()
dev.off()
