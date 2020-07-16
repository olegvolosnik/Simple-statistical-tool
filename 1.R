#!/usr/bin/env Rscript


library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr) 
library(xlsx)
library(car)
library(dunn.test)
library(FSA)
library(ggpubr)
library(ggplot2)


args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Please enter at least 1 arg.")
}
data <- read.csv2(file=args[1], header=TRUE)



# 1. Reading data from csv

data <- read.csv2("przykladoweDane-Projekt.csv", sep = ";")                                  # read from args[i]
data_new <- data[data$grupa != "KONTROLA", ]


groups <- levels(data$grupa)
gr_amount <- length(levels(data$grupa))# counting amount of group remember that 1 is always CONTROL 
col_names <- names(with(data, data[grupa == groups[1], ]))


a <- apply(data, 2, function(x) any(is.na(x)))# to check if NA in data frame
replace_NA <- 0

for(i in 1:length(a)){
  if(a[i] == "TRUE"){
    replace_NA <- 1
    break()
  }
}

tmp_vector <- c()
for(j in 1 : length(groups)){
  gr_tmp <- with(data, data[grupa == groups[j], ])
  cat(length(gr_tmp[[1]]))
  cat("\n")
  
}

# replacing NA in data frame                                                                            
if(replace_NA == 1){
  for (i in which(sapply(data, is.numeric))) {
    for (j in which(is.na(data[, i]))) {
      if(j < 26){
        tmp_z <- "CHOR1"
      }else if(j < 52){
        tmp_z <- "CHOR2"
      }else{
        tmp_z <- "KONTROLA"
      }
      line <- paste("Group: ", tmp_z, col_names[i], j, "replaced with avg in group/column: ", mean(data[data[, "grupa"] == data[j, "grupa"], i],  na.rm = TRUE), "\n")
      cat(line)
      #write(line,file="report_NA_replace.txt",append=TRUE)
     # cat("\n")
      data[j, i] <- mean(data[data[, "grupa"] == data[j, "grupa"], i],  na.rm = TRUE)
    }
  }
}



# Here is a part for automatic group name
# rule 1: must be group with name KONTROLA - representing control group.


#with kontrola_pointer we know all other groups are 
for (i in 1:length(levels(data$grupa))){
  if(groups[i] == "KONTROLA"){
    kontrola_pointer <- i
    break()
  }
}
# grupa_pointer is need to make not numerical plots
for (i in 1:length(col_names)){
  if(col_names[i] == "grupa"){
    grupa_pointer <- i
    break
  }
}




















# 2. Writing in two formats summary for each group

for(i in 1:length(levels(data$grupa))){
  df_tmp <- summary(with(data, data[grupa == groups[i], ]))
  write.xlsx(df_tmp, file="summary1.1.xlsx", sheetName=groups[i], append=TRUE, row.names=FALSE,)
  write.table(df_tmp, 'summary1.2.csv', sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE, append = TRUE)
}


# JUST FOR SHORTER EXAMPLE. LATER NO HARD CHOR[1,2,...]!!!
#KONTROLA <- with(data, data[grupa == groups[kontrola_pointer], ])
#CHOR1 <- with(data, data[grupa == "CHOR1", ])
#CHOR2 <- with(data, data[grupa == "CHOR2", ])



for( i in 1 : length(col_names)){
  par(mfrow=c(length(groups),1))
  for(j in 1 : length(groups)){
    gr_tmp <- with(data, data[grupa == groups[j], ])
    if(is.numeric(gr_tmp[[i]])){
      hist(gr_tmp[[i]], main = groups[j], sub = col_names[i])
    }else{
      plot(gr_tmp[[i]], main = groups[j], sub = col_names[i])
    }
  }
  dev.copy(png,paste0('my_plot_', i, '.png'))
  dev.off()
}

par(mfrow=c(1,1)) # merge plot screen to monolith






















# 3. Analizy porwnawcze

# rozklad normalny
matrix_rozklad_normalny <- matrix(nrow = length(groups), ncol = length(col_names))

for( i in 1 : length(col_names)){
  for(j in 1 : length(groups)){
    gr_tmp <- with(data, data[grupa == groups[j], ])
    if(is.numeric(gr_tmp[[i]])){
      if(shapiro.test(gr_tmp[[i]])$p.value > 0.05){
        cat(paste("\nrozkladem normalny (+) - ", col_names[i], groups[j] ))
        matrix_rozklad_normalny[j,i] <- 1
        
      }else{
        cat(paste("\nrozkladem normalny (-) - ", col_names[i], groups[j] ))
        matrix_rozklad_normalny[j,i] <- 0
      }
    }
  }
}
matrix_rozklad_normalny # NA in some columens because they are not numeric


# jednorodnosci wariancji
matrix_jednorodnosci_wariancji <- matrix(nrow = length(groups), ncol = length(col_names))
for( i in 1 : length(col_names)){
  for(j in 1 : length(groups)){
    gr_tmp <- with(data, data[grupa == groups[j], ])
    if(is.numeric(gr_tmp[[i]])){
      if(leveneTest(data[,i] ~ grupa, data = data)$`Pr(>F)`[1] > 0.05){
        cat(paste("\njednorodnsci wariancji (+) - ", col_names[i], groups[j] ))
        matrix_jednorodnosci_wariancji[j,i] <- 1
        
      }else{
        cat(paste("\njednorodnosci wariancji (-) - ", col_names[i], groups[j] ))
        matrix_jednorodnosci_wariancji[j,i] <- 0
      }
    }
  }
}
matrix_jednorodnosci_wariancji

####################################################################################
####################################################################################
####################################################################################
####################################################################################
nr_Tukey <- 0 #counter for post hoc Tukey
nr_Dunn <- 0 #counter for post hoc Dunn
####################################################################################
Kruskala_Wallisa_phDunna <- function(i){
  pvalueKWtest <- kruskal.test(data[[i]] ~ grupa, data = data)$p.value
  if(pvalueKWtest < 0.05){
    
    line <- paste(col_names[i], pvalueKWtest, "< 0.05 - sa roznice pomiedzy grupami\npost hoc Dunna:")
    write(line,file="groups_dif_report.txt",append=TRUE)
    
    line <- paste0("Dunn test outputs are in file dunnTest",i,".csv. \nOrder of columns: Comparison   Z   P.unadj   P.adj\n\n")
    write(line,file="groups_dif_report.txt",append=TRUE)
    
    df <- dunnTest(data[[i]], data$grupa)$res
    write.table(df, paste0("dunnTest",i,".csv"), sep = ",", col.names = file.exists(paste0("dunnTest",i,".csv")), append = F)
    
  }else{
    
    line <- paste(col_names[i], pvalueKWtest, "> 0.05 - brak roznic pomiedzy grupami\n")
    write(line,file="groups_dif_report.txt",append=TRUE)
  }
}
####################################################################################
ANOVA_phTukeya <- function(i){
  pvalueAOVtest <- summary(aov(data[[i]] ~ grupa, data = data))[[1]][["Pr(>F)"]][[1]]
  if(pvalueAOVtest < 0.05){
    
    line <- paste(col_names[i], pvalueAOVtest, "< 0.05 - sa roznice pomiedzy grupami\npost hoc Tukeya:")         #???
    write(line,file="groups_dif_report.txt",append=TRUE)
    
    line <- paste0("TukeyHSD test outputs are in file TukeyHSD",i,".csv. \nOrder of columns: Comparison   diff   lwr   p adj\n\n")
    write(line,file="groups_dif_report.txt",append=TRUE)
    
    df <-as.data.frame(TukeyHSD(aov(data[[4]] ~ grupa, data = data))$grupa)
    write.table(df, paste0("TukeyHSD",i,".csv"), sep = ",", col.names = !file.exists(paste0("TukeyHSD",i,".csv")), append = F)
    
  }else{
    line <- paste(col_names[i], pvalueAOVtest, "> 0.05 - brak roznic pomiedzy grupami\n")
    write(line,file="groups_dif_report.txt",append=TRUE)
  }
}
####################################################################################
Wilcoxon <- function(i){
  WilcoxonTest <- wilcox.test(data[[i]] ~ grupa, data = data)
  if(WilcoxonTest$p.value < 0.05){
    
    line <- paste(col_names[i], WilcoxonTest$p.value, "< 0.05 - sa roznice pomiedzy grupami\n")
    write(line,file="groups_dif_report.txt",append=TRUE)
    
  }else{
    
    line <- paste(col_names[i], WilcoxonTest$p.value, "> 0.05 - brak roznic pomiedzy grupami\n")
    write(line,file="groups_dif_report.txt",append=TRUE)
  }
}
####################################################################################
Welcha <- function(i){
  WelchaTest <- t.test(data[[i]] ~ grupa, data = data, var.equal = FALSE)
  if(WelchaTest$p.value < 0.05){
    
    line <- paste(col_names[i], WelchaTest$p.value, "< 0.05 - sa roznice pomiedzy grupami\n")
    write(line,file="groups_dif_report.txt",append=TRUE)
    
  }else{
    
    line <- paste(col_names[i], WelchaTest$p.value, "> 0.05 - brak roznic pomiedzy grupami\n")
    write(line,file="groups_dif_report.txt",append=TRUE)
  }
}
####################################################################################
t_Student <- function(i){
  Studenttest <- t.test(data[[i]] ~ grupa, data = data, var.equal = TRUE)
  if(Studenttest$p.value < 0.05){
    
    line <- paste(col_names[i], Studenttest$p.value, "< 0.05 - sa roznice pomiedzy grupami\n")
    write(line,file="groups_dif_report.txt",append=TRUE)
    
  }else{
    
    line <- paste(col_names[i], Studenttest$p.value, "> 0.05 - brak roznic pomiedzy grupami\n")
    write(line,file="groups_dif_report.txt",append=TRUE)
  }
}
####################################################################################
chisq_test <- function(i){
  line <- paste0(col_names[i], " is not numeric -  so  you can fund plot in Rplots.pdf as Red and Blue plots, after histograms.\n")
  write(line,file="groups_dif_report.txt",append=TRUE)
  uniq<-unique(data[[i]],incomparables = FALSE)
  chisq.test(data[[grupa_pointer]], data[[i]])
  pvalueChisq <- chisq.test(data[[grupa_pointer]], data[[i]])$p.value
  barplot(table(data[[i]], data[[grupa_pointer]]),
          title = paste0("grupa_", col_names[i]),
          sub = paste0("grupa_", col_names[i]),
          ylim = c(0,25),
          beside = TRUE,
          col = c("#FF0000FF", "#0066FFFF"),
          ylab = "grupa",
          xlab = col_names[i],
          legend = uniq
  )
  text(7.2, 17, paste("p-value", round(pvalueChisq, digits = 3)))
}
####################################################################################
####################################################################################
####################################################################################
####################################################################################
for( i in 1 : length(col_names)){
    if(is.na(matrix_jednorodnosci_wariancji[1,i]) == FALSE){
      if(length(groups)>2){
        
        sum_tmp <- 0
        for(j in 1:length(groups)){ sum_tmp = sum_tmp + matrix_rozklad_normalny[j,i] } 
        
        if(sum_tmp < length(groups)){
          cat("\n test Kruskala-Wallisa (post hoc Dunna)")
          Kruskala_Wallisa_phDunna(i)
          
          
        }else{
          sum_tmp <- 0
          for(j in 1:length(groups)){ sum_tmp = sum_tmp + matrix_jednorodnosci_wariancji[j,i] } 
        
          if(sum_tmp < length(groups)){
            cat("\n test Kruskala-Wallisa (post hoc Dunna)")
            Kruskala_Wallisa_phDunna(i)
            
          }else{
            cat("\n test ANOVA (post hoc Tukeya)")
            ANOVA_phTukeya(i)
          }
        }
        
          
      }else{
        
        sum_tmp <- 0
        for(j in 1:length(groups)){ sum_tmp = sum_tmp + matrix_rozklad_normalny[j,i] }
        if(sum_tmp < length(groups)){
          cat("\n test Wilcoxona (Manna-Whitneya)")
          Wilcoxon(i)
          
        }else{
          sum_tmp <- 0
          for(j in 1:length(groups)){ sum_tmp = sum_tmp + matrix_jednorodnosci_wariancji[j,i] } 
          if(sum_tmp < length(groups)){
            cat("\n test Welcha")
            Wlecha(i)
            
          }else{
            cat("\n test t-Studenta (dla gr. niezależnych)")
            t_Student(i)
          }
        }
      }
    }else{
      if(i != grupa_pointer ){
        chisq_test(i)
        dev.copy(png,paste0('my_plot_grupa_', col_names[i], '.png'))
        dev.off()
      }
    }
}


















#check p-value >< 0.05
data
# 4. Correlation 
Pearson_fun1 <- function(cor_Pearson, i, j, k){
  
  if(cor_Pearson$estimate > 0){
    line <- paste(groups[i], col_names[j], col_names[k], round(cor_Pearson$estimate, digits = 4), "r > 0 korelacja dodatnia ")
  }else if(cor_Pearson$estimate < 0){
    line <- paste(groups[i], col_names[j], col_names[k], round(cor_Pearson$estimate, digits = 4), "r < 0 korelacja ujemna ")
  }else{
    line <- paste(groups[i], col_names[j], col_names[k], round(cor_Pearson$estimate, digits = 4), "r = 0 brak korelacji ")
  }
  write(line,file="report_correlation.txt",append=TRUE)
  
  if(cor_Pearson$estimate >= -1 && cor_Pearson$estimate <= -0.7 ){
    line <- paste("(bardzo silna korelacja ujemna)", "\n")
  }else if(cor_Pearson$estimate > -0.7 && cor_Pearson$estimate <= -0.5){
    line <- paste("(silna korelacja ujemna)", "\n")
  }else if(cor_Pearson$estimate > -0.5 && cor_Pearson$estimate <= -0.3){
    line <- paste("(korelacja ujemna o srednim natezeniu)", "\n")
  }else if(cor_Pearson$estimate > -0.3 && cor_Pearson$estimate <= -0.2){
    line <- paste("(slaba korelacja ujemna)", "\n")
  }else if(cor_Pearson$estimate > -0.2 && cor_Pearson$estimate <= 0.2){
    line <- paste("(brak korelacji)", "\n")
  }else if(cor_Pearson$estimate > 0.2 && cor_Pearson$estimate <= 0.3){
    line <- paste("(slaba korelacja dodatnia)", "\n")
  }else if(cor_Pearson$estimate > 0.3 && cor_Pearson$estimate <= 0.5){
    line <- paste("(korelacja dodatnia o srednim natezeniu)", "\n")
  }else if(cor_Pearson$estimate > 0.5 && cor_Pearson$estimate <= 0.7){
    line <- paste("(silna korelacja dodatnia)", "\n")
  }else if(cor_Pearson$estimate > 0.7 && cor_Pearson$estimate <= 1){
    line <- paste("(bardzo silna korelacja dodatnia)", "\n")
  } 
  write(line,file="report_correlation.txt",append=TRUE)
}
Pearson_fun2 <- function(gr_tmp, i, j, k){
  a <- ggscatter(gr_tmp, x = col_names[k], y = col_names[j],
                 add = "reg.line", conf.int = TRUE,
                 cor.coef = TRUE, cor.method = "pearson",
                 color = "grupa", fill = "grupa",
                 palette = c("#0000FF"),
                 ylab = col_names[j],
                 xlab = col_names[k]
  )
  ggsave(a, file=paste0("correlation_", groups[i],"_", col_names[j], "_", col_names[k], ".png"), width = 20, height = 20, units = "cm")
  #dev.copy(png,paste0("correlation_", groups[i],"_", col_names[j], "_", col_names[k], ".png"))
  #dev.off()
}




for( i in 1 : length(groups)){
  gr_tmp <- with(data, data[grupa == groups[i], ])
  for(j in 1 : length(col_names)){
    if(is.numeric(gr_tmp[[j]]))
      for(k in 1 : length(col_names)){
        if(k>j && is.numeric(gr_tmp[[k]])){
          cor_Pearson <- cor.test(gr_tmp[[j]], gr_tmp[[k]], method = "pearson")
          if(cor_Pearson$p.value < 0.05){
            Pearson_fun1(cor_Pearson, i, j , k)
            # Saving plots
            Pearson_fun2(gr_tmp, i,j,k)
          }
        }
    }
  }
}







