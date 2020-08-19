####Introduction####
#This document contains the R code that was used for the article:
#"Assessing the Validity and Reliability of a Low-Cost Microcontroller-Based Load Cell Amplifier 
#for Measuring Lower Limb and Upper Limb Muscular Force"
#Co-authored by Julie Gaudet
#Co-authored by Grant Handrigan
#Université de Moncton
#2020-08-18


####Libraries####

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(dslabs)
library(blandr)
library(lpSolve)
library(irr)
library(data.table)
library(pander)


  data  <- read_csv("C:/Users/pgh9985/OneDrive - Université de Moncton/Projet de mémoire (J Gaudet)/Article - projet fiabilité et validité force/R/data.csv", col_names = TRUE)
  day_1 <- read_csv("C:/Users/pgh9985/OneDrive - Université de Moncton/Projet de mémoire (J Gaudet)/Article - projet fiabilité et validité force/R/day1.csv")
  day_2 <- read_csv("C:/Users/pgh9985/OneDrive - Université de Moncton/Projet de mémoire (J Gaudet)/Article - projet fiabilité et validité force/R/day2.csv")
 

attach(data)
 

####Figures 1-4#### 

#Correlation calculation for display in figures 1 to 4    
#Correlation session 1 lowerbody;
  cor.test(HX711_low_1, Handheld_low_1, methode="pearson")
  cor.test(SC_low_1, Handheld_low_1, methode="pearson")
  cor.test(HX711_low_1, SC_low_1, methode="pearson")
#Correlation session 2 lowerbody; 
  cor.test(HX711_low_2, Handheld_low_2, methode="pearson")
  cor.test(SC_low_2, Handheld_low_2, methode="pearson")
  cor.test(HX711_low_2, SC_low_2, methode="pearson")
#Correlation session 1  upperbody;
  cor.test(SC_up_1, Handheld_up_1, methode="pearson")
  cor.test(HX711_up_1, SC_up_1, methode="pearson")
  cor.test(HX711_up_1, Handheld_up_1, methode="pearson")
#Correlation session 2 upperbody
  cor.test(HX711_up_2, Handheld_up_2, methode="pearson")
  cor.test(SC_up_2, Handheld_up_2, methode="pearson")
  cor.test(HX711_up_2, SC_up_2, methode="pearson")
  
#Scatterplots for figures 1 to 4 
  L_1_HD_HX711<- ggplot(data, aes(x=HX711_low_1, y=Handheld_low_1))+ 
      geom_point(aes(),  size=6, colour = "#E0A725", shape=16)+ 
      geom_smooth(method="lm", fill = "#02488C", size = 2.5)+
      xlab("Lower limb muscular strength measured with HX711 microcontroller-based load cell (kg)") + 
      ylab("Lower limb muscular strength measured with handheld dynamometer (kg)")+ 
      annotate("text", x = 30, y = 12, size = 6, label = "r(29) = 0.9709, 95% C.I. [0.938, 0.986], p<0.001.")+
      theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  L_1_HX711_SC<-ggplot(data, aes(x=SC_low_1, y=HX711_low_1))+ 
    geom_point(aes(),  size=6, colour = "#E0A725", shape=16)+
    geom_smooth(method="lm", fill = "#02488C", size=2.5)+
    xlab("Lower limb muscular strength measured with HX711 microcontroller-based load cell (kg)") + 
    ylab("Lower limb muscular strength measured with signal conditioner (kg)")+ 
    annotate("text", x = 30, y = 12, size = 6, label = "r(29) = 0.9867, 95% C.I. [0.971, 0.994], p<0.001.")+
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  L_2_HD_HX711<-ggplot(data, aes(x=HX711_low_2, y=Handheld_low_2))+ 
    geom_point(aes(),  size=6, colour = "#E0A725", shape=16)+
    geom_smooth(method="lm", fill = "#02488C", size=2.5)+
    annotate("text", x = 30, y = 12, size = 6, label = "r(29) = 0.9846, 95% C.I. [0.967, 0.993], p<0.001.")+
    xlab("Lower limb muscular strength measured with HX711 microcontroller-based load cell (kg)") + 
    ylab("Lower limb muscular strength measured with handheld dynamometer (kg)")+ 
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  L_2_HX711_SC<-ggplot(data, aes(x=SC_low_2, y=HX711_low_2))+ 
    geom_point(aes(),  size=6, colour = "#E0A725", shape=16)+
    geom_smooth(method="lm", fill = "#02488C", size=2.5)+
    annotate("text", x = 30, y = 12, size = 6, label = "r(29) = 0.9916, 95% C.I. [0.982, 0.996], p<0.001.")+
    xlab("Lower limb muscular strength measured with HX711 microcontroller-based load cell (kg)") + 
    ylab("Lower limb muscular strength measured with signal conditioner (kg)")+ 
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  U_1_HX711_SC<-ggplot(data, aes(x=SC_up_1, y=HX711_up_1))+ 
    geom_point(aes(),  size=6, colour = "#E0A725", shape=16)+
    geom_smooth(method="lm", fill = "#02488C", size=2.5)+
    annotate("text", x = 13, y = 5, size = 6, label = "r(29) = 0.9778, 95% C.I. [0.953, 0.990], p<0.001.")+
    xlab("Upper limb muscular strength measured with HX711 microcontroller-based load cell (kg)") + 
    ylab("Upper limb muscular strength measured with signal conditioner (kg)")+ 
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  U_1_HD_HX711<-ggplot(data, aes(x=HX711_up_1, y=Handheld_up_1))+ 
    geom_point(aes(),  size=6, colour = "#E0A725", shape=16)+
    geom_smooth(method="lm", fill = "#02488C", size=2.5)+
    annotate("text", x = 13, y = 5, size = 6, label = "r(29) = 0.9711, 95% C.I. [0.939, 0.987], p<0.001.")+
    xlab("Upper limb muscular strength measured with HX711 microcontroller-based load cell (kg)") + 
    ylab("Upper limb muscular strength measured with handheld dynamometer (kg)")+ 
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  U_2_HD_HX711<-ggplot(data, aes(x=HX711_up_2, y=Handheld_up_2))+ 
    geom_point(aes(),  size=6, colour = "#E0A725", shape=16)+ 
    geom_smooth(method="lm", fill = "#02488C", size=2.5)+
    annotate("text", x = 11.5, y = 4, size = 6, label = "r(29) = 0.9685, 95% C.I. [0.933, 0.985], p<0.001.")+
    xlab("Upper limb muscular strength measured with HX711 microcontroller-based load cell (kg)") + 
    ylab("Upper limb muscular strength measured with handheld dynamometer (kg)")+ 
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  U_2_HX711_SC<-ggplot(data, aes(x=SC_up_2, y=HX711_up_2))+ 
    geom_point(aes(),  size=6, colour = "#E0A725", shape=16)+ 
    geom_smooth(method="lm", fill = "#02488C", size=2.5)+
    annotate("text", x = 15, y = 5, size = 6, label = "r(29) = 0.9466, 95% C.I. [0.888, 0.975], p<0.001.")+
    xlab("Upper limb muscular strength measured with HX711 microcontroller-based load cell (kg)") + 
    ylab("Upper limb muscular strength measured with signal conditioner (kg)")+ 
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  

# Figure outputs  
#  ggsave ("Low_1_HD_HX711.pdf", plot =L_1_HD_HX711, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#  ggsave ("Low_1_HX711_SC.pdf", plot =L_1_HX711_SC, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#  ggsave ("Low_2_HD_HX711.pdf", plot =L_2_HD_HX711, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#  ggsave ("Low_2_HX711_SC.pdf", plot =L_2_HX711_SC, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#  ggsave ("Up_1_HX711_SC.pdf", plot =U_1_HX711_SC, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#  ggsave ("Up_1_HD_HX711.pdf", plot =U_1_HD_HX711, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#  ggsave ("Up_2_HD_HX711.pdf", plot =U_2_HD_HX711, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#  ggsave ("Up_2_HX711_SC.pdf", plot =U_2_HX711_SC, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )

####Figures 5-8####
  
#Tukey Mean difference plots for figures 5 to 8
  
  BA_SC_HX711_LOW1<-blandr.plot.ggplot(blandr.statistics(HX711_low_1, SC_low_1), ciDisplay = FALSE , ciShading = FALSE )+
    geom_point(aes(),  size=3, colour = "black", shape=16)+
    xlab("Mean values (kg)") + ylab("Differences between devices (kg)")+ 
    ggtitle("")+
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  BA_HD_HX711_LOW1<-blandr.plot.ggplot(blandr.statistics(HX711_low_1, Handheld_low_1), ciDisplay = FALSE , ciShading = FALSE )+
    geom_point(aes(),  size=3, colour = "black", shape=16)+
    xlab("Mean values (kg)") + ylab("Differences between devices (kg)")+ 
    ggtitle("")+
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  BA_SC_HX711_UP1<-blandr.plot.ggplot(blandr.statistics(HX711_up_1, SC_up_1), ciDisplay = FALSE , ciShading = FALSE )+
    geom_point(aes(),  size=3, colour = "black", shape=16)+
    xlab("Mean values (kg)") + ylab("Differences between devices (kg)")+ 
    ggtitle("")+
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  BA_HD_HX711_UP1<-blandr.plot.ggplot(blandr.statistics(HX711_up_1, Handheld_up_1), ciDisplay = FALSE , ciShading = FALSE )+
    geom_point(aes(),  size=3, colour = "black", shape=16)+
    xlab("Mean values (kg)") + ylab("Differences between devices (kg)")+ 
    ggtitle("")+
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
    
  BA_SC_HX711_LOW2<-blandr.plot.ggplot(blandr.statistics(HX711_low_2, SC_low_2), ciDisplay = FALSE , ciShading = FALSE )+
    geom_point(aes(),  size=3, colour = "black", shape=16)+
    xlab("Mean values (kg)") + ylab("Differences between devices (kg)")+ 
    ggtitle("")+
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  BA_HD_HX711_LOW2<-blandr.plot.ggplot(blandr.statistics(HX711_low_2, Handheld_low_2), ciDisplay = FALSE , ciShading = FALSE )+
    geom_point(aes(),  size=3, colour = "black", shape=16)+
    xlab("Mean values (kg)") + ylab("Differences between devices (kg)")+ 
    ggtitle("")+
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  BA_SC_HX711_UP2<-blandr.plot.ggplot(blandr.statistics(HX711_up_2, SC_up_2), ciDisplay = FALSE , ciShading = FALSE )+
    geom_point(aes(),  size=3, colour = "black", shape=16)+
    xlab("Mean values (kg)") + ylab("Differences between devices (kg)")+ 
    ggtitle("")+
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  BA_HD_HX711_UP2<-blandr.plot.ggplot(blandr.statistics(HX711_up_2, Handheld_up_2), ciDisplay = FALSE , ciShading = FALSE )+
    geom_point(aes(),  size=3, colour = "black", shape=16)+
    xlab("Mean values (kg)") + ylab("Differences between devices (kg)")+ 
    ggtitle("")+
    theme(panel.background = element_rect(fill="white"),
          axis.title.x = element_text(color="black", size=11, face="bold"),
          axis.title.y = element_text(color="black", size=11, face="bold"),
          axis.line = element_line(colour = "black", size = 1.1))
  
  
# Figure outputs  
#ggsave ("BA_SC_HX711_LOW1.pdf", plot =BA_SC_HX711_LOW1, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#ggsave ("BA_HD_HX711_LOW1.pdf", plot =BA_HD_HX711_LOW1, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#ggsave ("BA_SC_HX711_UP1.pdf", plot =BA_SC_HX711_UP1, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#ggsave ("BA_HD_HX711_UP1.pdf", plot =BA_HD_HX711_UP1, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#ggsave ("BA_SC_HX711_LOW2.pdf", plot =BA_SC_HX711_LOW2, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#ggsave ("BA_HD_HX711_LOW2.pdf", plot =BA_HD_HX711_LOW2, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#ggsave ("BA_SC_HX711_UP2.pdf", plot =BA_SC_HX711_UP2, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )
#ggsave ("BA_HD_HX711_UP2.pdf", plot =BA_HD_HX711_UP2, width = 9, height = 6.5, units = "in", dpi = 300, bg = "transparent" )


####Tables 1-3####

#Table 1 (Session 1) summary of mean difference comparisons between instruments
  hx.sc_low1 <- data.frame( HX711_low_1 , SC_low_1 )
  hx.sc_low1 <-blandr::jamoviBAanalysis( data= hx.sc_low1, method1="HX711_low_1" , method2="SC_low_1" )
  hx.sc_low1 <- hx.sc_low1$table$asDF
  hx.sc_low1 = data.table(hx.sc_low1, keep.rownames = TRUE)
  colnames(hx.sc_low1) <- c("","" ,"Estimate", "Lower CI", "Upper CI")
  hx.sc_low1[, Condition := "HX711 load cell vs signal conditioner, lower limb"]
  
  hx.hh_low1 <- data.frame( HX711_low_1 , Handheld_low_1 )
  hx.hh_low1 <-blandr::jamoviBAanalysis( data= hx.hh_low1, method1="HX711_low_1" , method2="Handheld_low_1" )
  hx.hh_low1 <- hx.hh_low1$table$asDF
  hx.hh_low1 = data.table(hx.hh_low1, keep.rownames = TRUE)
  hx.hh_low1[, Condition := "HX711 load cell vs Handhel dynamometer, lower limb"]
  
  hx.sc_up1 <- data.frame( HX711_up_1 , SC_up_1 )
  hx.sc_up1 <-blandr::jamoviBAanalysis( data= hx.sc_up1, method1="HX711_up_1" , method2="SC_up_1" )
  hx.sc_up1 <- hx.sc_up1$table$asDF
  hx.sc_up1 = data.table(hx.sc_up1, keep.rownames = TRUE)
  hx.sc_up1[, Condition := "HX711 load cell vs signal conditioner, upper limb"]
  
  hx.hh_up1 <- data.frame( HX711_up_1 , Handheld_up_1 )
  hx.hh_up1 <-blandr::jamoviBAanalysis( data= hx.hh_up1, method1="HX711_up_1" , method2="Handheld_up_1" )
  hx.hh_up1 <- hx.hh_up1$table$asDF
  hx.hh_up1 = data.table(hx.hh_up1, keep.rownames = TRUE)
  hx.hh_up1[, Condition := "HX711 load cell vs Handheld dynamometer, upper limb"]
  
  table1 <- rbind(hx.sc_low1,hx.hh_low1, hx.sc_up1,hx.hh_up1,  use.names=FALSE)
  table1 <- select(table1 , c(6,2,3,4,5)) 
  table1 <- data.table(table1)
  table1

#Table 2 (Session 2) summary of mean difference comparisons between instruments
  hx.sc_low2 <- data.frame( HX711_low_2 , SC_low_2 )
  hx.sc_low2 <-blandr::jamoviBAanalysis( data= hx.sc_low2, method1="HX711_low_2" , method2="SC_low_2" )
  hx.sc_low2 <- hx.sc_low2$table$asDF
  hx.sc_low2 = data.table(hx.sc_low2, keep.rownames = TRUE)
  colnames(hx.sc_low2) <- c("","" ,"Estimate", "Lower CI", "Upper CI")
  hx.sc_low2[, Condition := "HX711 load cell vs signal conditioner, lower limb"]
  
  hx.hh_low2 <- data.frame( HX711_low_2 , Handheld_low_2 )
  hx.hh_low2 <-blandr::jamoviBAanalysis( data= hx.hh_low2, method1="HX711_low_2" , method2="Handheld_low_2" )
  hx.hh_low2 <- hx.hh_low2$table$asDF
  hx.hh_low2 = data.table(hx.hh_low2, keep.rownames = TRUE)
  hx.hh_low2[, Condition := "HX711 load cell vs Handhel dynamometer, lower limb"]
  
  hx.sc_up2 <- data.frame( HX711_up_2 , SC_up_2 )
  hx.sc_up2 <-blandr::jamoviBAanalysis( data= hx.sc_up2, method1="HX711_up_2" , method2="SC_up_2" )
  hx.sc_up2 <- hx.sc_up2$table$asDF
  hx.sc_up2 = data.table(hx.sc_up2, keep.rownames = TRUE)
  hx.sc_up2[, Condition := "HX711 load cell vs signal conditioner, upper limb"]
  
  hx.hh_up2 <- data.frame( HX711_up_2 , Handheld_up_2 )
  hx.hh_up2 <-blandr::jamoviBAanalysis( data= hx.hh_up2, method1="HX711_up_2" , method2="Handheld_up_2" )
  hx.hh_up2 <- hx.hh_up2$table$asDF
  hx.hh_up2 = data.table(hx.hh_up2, keep.rownames = TRUE)
  hx.hh_up2[, Condition := "HX711 load cell vs Handheld dynamometer, upper limb"]
  
  table2 <- rbind(hx.sc_low2,hx.hh_low2, hx.sc_up2,hx.hh_up2,  use.names=FALSE)
  table2 <-select(table2 , c(6,2,3,4,5)) 
  table2 <-data.table(table2)
  table2

#Table 3 inter-day reliability 

#Create a subset of the data with average values only
  HX711_1 <- data %>% dplyr:: select(2, 4)
  HX711_2 <- data %>% dplyr:: select (3, 5)
#gather data and converts into long data
  HX711_1 <- gather(HX711_1, measurement, factor_key=TRUE)
  HX711_2 <- gather(HX711_2, measurement, factor_key=TRUE)
#converts to numeric?
  HX711_1$measurement <- as.numeric(HX711_1$measurement)
  HX711_2$measurement <- as.numeric(HX711_2$measurement)
#combines the data
  HX711 <- cbind(HX711_1, HX711_2)

#Create a subset of the data with average values only
  Handheld_1 <- data %>% dplyr:: select(6, 8)
  Handheld_2 <- data %>% dplyr:: select (7, 9)
#gather data and converts into long data
  Handheld_1 <- gather(Handheld_1, measurement, factor_key=TRUE)
  Handheld_2 <- gather(Handheld_2, measurement, factor_key=TRUE)
#converts to numeric
  Handheld_1$measurement <- as.numeric(Handheld_1$measurement)
  Handheld_2$measurement <- as.numeric(Handheld_2$measurement)
#combines the data
  Handheld <- cbind(Handheld_1, Handheld_2)

#Create a subset of the data with average values only
  SC_1 <- data %>% dplyr:: select(10, 12)
  SC_2 <- data %>% dplyr:: select (11, 13)
#gather data and converts into long data
  SC_1 <- gather(SC_1, measurement, factor_key=TRUE)
  SC_2 <- gather(SC_2, measurement, factor_key=TRUE)
#converts to numeric
  SC_1$measurement <- as.numeric(SC_1$measurement)
  SC_2$measurement <- as.numeric(SC_2$measurement)
#combines the data
  SC<-cbind(SC_1, SC_2)

#inter-day reliability: ICC calculation and table creation
#HX711 ICC
  icc_results_HX711 <- icc(HX711[,c(2,4)], model = "twoway", type = "agreement", unit = "average")
  DT = data.table(icc_results_HX711, keep.rownames = TRUE)
  DT = data.table(t(DT))
  colnames(DT) <- c("Subjects", "Raters", "Model", "Type", "Unit", 
                  "ICC.names", "Value", "Null.hypothesis", "F-value",
                  "DF1", "DF2", "p-value", "conf.level", "lbound", "ubound")
  DT[, Tool := "HX711 Loadcell"]

#Handheld ICC
  icc_results_Handheld <- icc(Handheld[,c(2,4)], model = "twoway", type = "agreement", unit = "average")
  icc_results_Handheld=data.table(icc_results_Handheld, keep.rownames = TRUE)
  icc_results_Handheld=data.table(t(icc_results_Handheld))
  icc_results_Handheld[, Tool := "Handheld dynamometer"] 

#SC ICC
  icc_results_SC <- icc(SC[,c(2,4)], model = "twoway", type = "agreement", unit = "average")
  icc_results_SC=data.table(icc_results_SC, keep.rownames = TRUE)
  icc_results_SC=data.table(t(icc_results_SC))
  icc_results_SC[, Tool := "Signal conditioner"]  


  DT<- rbind(DT,icc_results_Handheld, icc_results_SC, use.names=FALSE)



  
####INTRA-SESSION ICC####
#Session 1 INTRA-SESSION ICC
  #Create a subset of the data for session1 HX711
  HX711_1_1 <- day_1 %>% dplyr:: select(5,17)
  HX711_1_2 <- day_1 %>% dplyr:: select (6,18)
  HX711_1_3 <- day_1 %>% dplyr:: select (7,19)
  HX711_1_4 <- day_1 %>% dplyr:: select (8,20)
  #gather data and converts into long data
  HX711_1_1 <- gather(HX711_1_1, measurement, factor_key=TRUE)
  HX711_1_2 <- gather(HX711_1_2, measurement, factor_key=TRUE)
  HX711_1_3 <- gather(HX711_1_3, measurement, factor_key=TRUE)
  HX711_1_4 <- gather(HX711_1_4, measurement, factor_key=TRUE)
  #converts to numeric?
  HX711_1_1$measurement <- "trial 1"
  HX711_1_2$measurement <- "trail 2"
  HX711_1_3$measurement <- "trail 3"
  HX711_1_4$measurement <- "trail 4"
  #combining the data
  HX711_session1<-cbind(HX711_1_1, HX711_1_2,HX711_1_3,HX711_1_4)
  
  #Create a subset of the data for session1 HHD
  HHD_1_1 <- day_1 %>% dplyr:: select(1,13)
  HHD_1_2 <- day_1 %>% dplyr:: select (2,14)
  HHD_1_3 <- day_1 %>% dplyr:: select (3,15)
  HHD_1_4 <- day_1 %>% dplyr:: select (4,16)
  #gather data and converts into long data
  HHD_1_1 <- gather(HHD_1_1, measurement, factor_key=TRUE)
  HHD_1_2 <- gather(HHD_1_2, measurement, factor_key=TRUE)
  HHD_1_3 <- gather(HHD_1_3, measurement, factor_key=TRUE)
  HHD_1_4 <- gather(HHD_1_4, measurement, factor_key=TRUE)
  #converts to numeric?
  HHD_1_1$measurement <- "trial 1"
  HHD_1_2$measurement <- "trail 2"
  HHD_1_3$measurement <- "trail 3"
  HHD_1_4$measurement <- "trail 4"
  #combines the data
  HHD_session1<-cbind(HHD_1_1, HHD_1_2,HHD_1_3,HHD_1_4)
  
  #Create a subset of the data for session1 SC
  SC_1_1 <- day_1 %>% dplyr:: select(9,21)
  SC_1_2 <- day_1 %>% dplyr:: select (10,22)
  SC_1_3 <- day_1 %>% dplyr:: select (11,23)
  SC_1_4 <- day_1 %>% dplyr:: select (12,24)
  #gather data and converts into long data
  SC_1_1 <- gather(SC_1_1, measurement, factor_key=TRUE)
  SC_1_2 <- gather(SC_1_2, measurement, factor_key=TRUE)
  SC_1_3 <- gather(SC_1_3, measurement, factor_key=TRUE)
  SC_1_4 <- gather(SC_1_4, measurement, factor_key=TRUE)
  #converts to numeric?
  SC_1_1$measurement <- "trial 1"
  SC_1_2$measurement <- "trail 2"
  SC_1_3$measurement <- "trail 3"
  SC_1_4$measurement <- "trail 4"
  #combining the data
  SC_session1<-cbind(SC_1_1, SC_1_2,SC_1_3,SC_1_4)
  
  #intra-day reliability: ICC calculation and table creation
  #HX711 ICC
  ICC_HX711_1<-icc(HX711_session1[,c(2,4,6,8)], model = "twoway", type = "agreement", unit = "single")
  ICC_HX711_1 = data.table(ICC_HX711_1, keep.rownames = TRUE)
  ICC_HX711_1 = data.table(t(ICC_HX711_1))
  colnames(ICC_HX711_1) <- c("Subjects", "Raters", "Model", "Type", "Unit", 
                             "ICC.names", "Value", "Null.hypothesis", "F-value",
                             "DF1", "DF2", "p-value", "conf.level", "lbound", "ubound")
  ICC_HX711_1[, Tool := "HX711 Loadcell"]
  
  #Handheld ICC
  ICC_HHD_1<-icc( HHD_session1[,c(2,4,6,8)], model = "twoway", type = "agreement", unit = "single")
  ICC_HHD_1=data.table(ICC_HHD_1, keep.rownames = TRUE)
  ICC_HHD_1=data.table(t(ICC_HHD_1))
  ICC_HHD_1[, Tool := "Handheld dynamometer"] 
  
  #SC ICC
  ICC_SC_1<-icc(SC_session1[,c(2,4,6,8)], model = "twoway", type = "agreement", unit = "single")
  ICC_SC_1=data.table(ICC_SC_1, keep.rownames = TRUE)
  ICC_SC_1=data.table(t(ICC_SC_1))
  ICC_SC_1[, Tool := "Signal conditioner"]  
  
  
  ICC_1<- rbind(ICC_HX711_1, ICC_HHD_1, ICC_SC_1, use.names=FALSE)
  
  
#Session 2 INTRA-SESSION ICC
  #Create a subset of the data for session1 HX711
  HX711_2_1 <- day_2 %>% dplyr:: select(5,17)
  HX711_2_2 <- day_2 %>% dplyr:: select (6,18)
  HX711_2_3 <- day_2 %>% dplyr:: select (7,19)
  HX711_2_4 <- day_2 %>% dplyr:: select (8,20)
  #gather data and converts into long data
  HX711_2_1 <- gather(HX711_2_1, measurement, factor_key=TRUE)
  HX711_2_2 <- gather(HX711_2_2, measurement, factor_key=TRUE)
  HX711_2_3 <- gather(HX711_2_3, measurement, factor_key=TRUE)
  HX711_2_4 <- gather(HX711_2_4, measurement, factor_key=TRUE)
  #converts to numeric?
  HX711_2_1$measurement <- "trial 1"
  HX711_2_2$measurement <- "trail 2"
  HX711_2_3$measurement <- "trail 3"
  HX711_2_4$measurement <- "trail 4"
  #combining the data
  HX711_session2<-cbind(HX711_2_1, HX711_2_2,HX711_2_3,HX711_2_4)
  
  #Create a subset of the data for session1 HHD
  HHD_2_1 <- day_2 %>% dplyr:: select(1,13)
  HHD_2_2 <- day_2 %>% dplyr:: select (2,14)
  HHD_2_3 <- day_2 %>% dplyr:: select (3,15)
  HHD_2_4 <- day_2 %>% dplyr:: select (4,16)
  #gather data and converts into long data
  HHD_2_1 <- gather(HHD_2_1, measurement, factor_key=TRUE)
  HHD_2_2 <- gather(HHD_2_2, measurement, factor_key=TRUE)
  HHD_2_3 <- gather(HHD_2_3, measurement, factor_key=TRUE)
  HHD_2_4 <- gather(HHD_2_4, measurement, factor_key=TRUE)
  #converts to numeric?
  HHD_2_1$measurement <- "trial 1"
  HHD_2_2$measurement <- "trail 2"
  HHD_2_3$measurement <- "trail 3"
  HHD_2_4$measurement <- "trail 4"
  #combining the data
  HHD_session2<-cbind(HHD_2_1, HHD_2_2,HHD_2_3,HHD_2_4)
  
  #Create a subset of the data for session1 SC
  SC_2_1 <- day_2 %>% dplyr:: select(9,21)
  SC_2_2 <- day_2 %>% dplyr:: select (10,22)
  SC_2_3 <- day_2 %>% dplyr:: select (11,23)
  SC_2_4 <- day_2 %>% dplyr:: select (12,24)
  #gather data and converts into long data
  SC_2_1 <- gather(SC_2_1, measurement, factor_key=TRUE)
  SC_2_2 <- gather(SC_2_2, measurement, factor_key=TRUE)
  SC_2_3 <- gather(SC_2_3, measurement, factor_key=TRUE)
  SC_2_4 <- gather(SC_2_4, measurement, factor_key=TRUE)
  #converts to numeric?
  SC_2_1$measurement <- "trial 1"
  SC_2_2$measurement <- "trail 2"
  SC_2_3$measurement <- "trail 3"
  SC_2_4$measurement <- "trail 4"
  #combining the data
  SC_session2<-cbind(SC_2_1, SC_2_2,SC_2_3,SC_2_4)
  
  
  
  #intra-day reliability: ICC calculation and table creation
  #HX711 ICC
  ICC_HX711_2<-icc(HX711_session2[,c(2,4,6,8)], model = "twoway", type = "agreement", unit = "single")
  ICC_HX711_2 = data.table(ICC_HX711_2, keep.rownames = TRUE)
  ICC_HX711_2 = data.table(t(ICC_HX711_2))
  colnames(ICC_HX711_2) <- c("Subjects", "Raters", "Model", "Type", "Unit", 
                             "ICC.names", "Value", "Null.hypothesis", "F-value",
                             "DF1", "DF2", "p-value", "conf.level", "lbound", "ubound")
  ICC_HX711_2[, Tool := "HX711 Loadcell"]
  
  #Handheld ICC
  ICC_HHD_2<-icc( HHD_session2[,c(2,4,6,8)], model = "twoway", type = "agreement", unit = "single")
  ICC_HHD_2=data.table(ICC_HHD_2, keep.rownames = TRUE)
  ICC_HHD_2=data.table(t(ICC_HHD_2))
  ICC_HHD_2[, Tool := "Handheld dynamometer"] 
  
  #SC ICC
  ICC_SC_2<-icc(SC_session2[,c(2,4,6,8)], model = "twoway", type = "agreement", unit = "single")
  ICC_SC_2=data.table(ICC_SC_2, keep.rownames = TRUE)
  ICC_SC_2=data.table(t(ICC_SC_2))
  ICC_SC_2[, Tool := "Signal conditioner"]  
  
  
  ICC_2<- rbind(ICC_HX711_2, ICC_HHD_2, ICC_SC_2, use.names=FALSE)
  
  