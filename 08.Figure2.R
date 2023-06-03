library(readxl)
result <- read_excel("result.xlsx")
View(result)

result$group=as.factor(result$group)
result$method=as.factor(result$method)
str(result)
colnames(result)[6:7]=c("NPV", "Balanced_accuracy")
colnames(result)[9]="F1_score"
library(ggplot2)
library(tidytree)
result$method <- gsub("Naïve_Bayesian_algorithm", "NBA", result$method )


# "Accuracy"
plot_acc=ggplot(result, aes(x = group, y = Accuracy, fill = method)) +
  geom_col(stat = "identity", position = "dodge") +
  labs(x = "group", y = "Accuracy") +
  theme(legend.position = "right")+ 
 
  scale_fill_manual(values =c( "#beb8dc", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C"))+
  
 
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank())+guides(fill = FALSE) # 隐藏图例

library(ggplot2)
library(ggpubr)
##Sensitivity
plot_sen =ggplot(result, aes(x = group, y = Sensitivity, fill = method)) +
  geom_col(stat = "identity", position = "dodge") +
  labs(x = "group", y = "Sensitivity") +
  theme(legend.position = "right")+ 
  
  scale_fill_manual(values =c( "#beb8dc", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C"))+
  
  
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank())+guides(fill = FALSE) # 隐藏图例
##Specificity
plot_spec =ggplot(result, aes(x = group, y = Specificity, fill = method)) +
  geom_col(stat = "identity", position = "dodge") +
  labs( x = "group", y = "Specificity") +
  theme(legend.position = "right")+ 
  
  scale_fill_manual(values =c( "#beb8dc", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C"))+
  
  
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank())+guides(fill = FALSE) # 隐藏图例
##Precision
plot_Pre =ggplot(result, aes(x = group, y = Precision, fill = method)) +
  geom_col(stat = "identity", position = "dodge") +
  labs( x = "group", y = "Precision") +
  theme(legend.position = "right")+ 
  
  scale_fill_manual(values =c( "#beb8dc", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C"))+
  
  
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank())+guides(fill = FALSE) # 隐藏图例
##negative_predictive_value
plot_npv =ggplot(result, aes(x = group, y = NPV, fill = method)) +
  geom_col(stat = "identity", position = "dodge") +
  labs( x = "group", y = "negative_predictive_value") +
  theme(legend.position = "right")+ 
  
  scale_fill_manual(values =c( "#beb8dc", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C"))+
  
 
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank())+guides(fill = FALSE) # 隐藏图例
##Balanced_accuracy
plot_ba =ggplot(result, aes(x = group, y = Balanced_accuracy, fill = method)) +
  geom_col(stat = "identity", position = "dodge") +
  labs( x = "group", y = "Balanced_accuracy") +
  theme(legend.position = "right")+ 
 
  scale_fill_manual(values =c( "#beb8dc", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C"))+
  

  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank())+guides(fill = FALSE) # 隐藏图例
##AUC
plot_AUC =ggplot(result, aes(x = group, y = AUC, fill = method)) +
  geom_col(stat = "identity", position = "dodge") +
  labs( x = "group", y = "AUC") +
  theme(legend.position = "right")+ 
 
  scale_fill_manual(values =c( "#beb8dc", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C"))+
  
 
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank())+guides(fill = FALSE) # 隐藏图例
##F1_score
plot_F1 =ggplot(result, aes(x = group, y = F1_score, fill = method)) +
  geom_col(stat = "identity", position = "dodge") +
  labs( x = "group", y = "F1_score") +
  theme(legend.position = "right")+ 
 
  scale_fill_manual(values =c( "#beb8dc", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C"))+
  
  
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank()) # 隐藏图例

ggarrange(plot_acc, plot_sen,plot_spec, plot_Pre,plot_npv,
          plot_ba ,plot_AUC,plot_F1,
          ncol = 4, nrow = 2)
ggsave(file="a.pdf",width = 19,height = 10)

