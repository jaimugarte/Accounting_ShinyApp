# Input file format:
# data <- data_cleaning("/cloud/project/datasets/example_bank_movements.xlsx")
plot_1 <- function(data){
  library(ggplot2)
  library(zoo)
  library(plotly)
  library(stats)
  library(scales)
  # profits by month
  row.names(data) <- 1:nrow(data)
  data$dia <- as.Date(data$`FECHA VALOR`, "%d/%m/%Y")
  data$`FECHA VALOR` <- as.Date(zoo::as.yearmon(data$`FECHA VALOR`, "%d/%m/%Y"))
  data$SALDO <- as.numeric(data$SALDO)
  factor_col <- c()
  for(i in 1:nrow(data)){
    x <- data$`IMPORTE EUR`[i]
    if(x > 0) factor_col[i] <- "Income"
    else factor_col[i] <- "Expenses"
  }
  data <- cbind(data,factor_col)
  data$factor_col <- as.factor(data$factor_col)
  # graph by month:
  p1<- ggplot(data = data)+
    ggplot2::stat_summary(fun.y = sum, # adds up all observations for the month
                 geom = "bar", position = "dodge",
                 aes(`FECHA VALOR`, abs(`IMPORTE EUR`),fill=factor_col,position = "dodge")) + # or "line"
    ggplot2::scale_x_date(
      labels = date_format("%b"),
      breaks = "1 month")+ # custom x-axis labels
    ggplot2::stat_summary(fun.y = sum, # adds up all observations for the month
                 geom = "smooth",aes(`FECHA VALOR`, `IMPORTE EUR`))+
    geom_line(aes(dia,SALDO))+
    xlab("Month")+ylab("Euros")
  return(p1)
}

plot_2 <- function(data){
  library(ggplot2)
  library(zoo)
  library(plotly)
  library(scales)
  data$`FECHA VALOR` <- as.Date(zoo::as.yearmon(data$`FECHA VALOR`, "%d/%m/%Y"))
  data<-data[data$`IMPORTE EUR`>0,]
  categories <- data.frame(
    Category = c("Food", "Payroll", "Residence fee","Maintenance", "Social Security" ),
    key = c("COMIDAPAL","TRANSFERENCIA A FAVOR","TRANSFERENCIA DE","POLSA","TGSS")
  )
  for(i in 1:nrow(data)){
    c <- 0
    for(k in 1:nrow(categories)){
      if(grepl(as.character(categories$key[k]),as.character(data$CONCEPTO[i])) == T){
        data$class[i] <- as.character(categories$Category[k])
        c <- 1
      } 
      if(c == 0) data$class[i] <- "Other"
    }
  }
  # graph by month:
  p2<- ggplot(data = data)+
    stat_summary(fun.y = sum, # adds up all observations for the month
                 geom = "bar", position = "dodge",
                 aes(`FECHA VALOR`, `IMPORTE EUR` ,fill=class,position = "dodge")) + # or "line"
    scale_x_date(
      labels = date_format("%b"),
      breaks = "1 month")+ # custom x-axis labels
    stat_summary(fun.y = sum, # adds up all observations for the month
                 geom = "smooth",aes(`FECHA VALOR`, `IMPORTE EUR`))+
    xlab("Month")+ylab("Euros")
  return(p2)
}

plot_3 <- function(data){
  library(ggplot2)
  library(zoo)
  library(plotly)
  library(scales)
  data$`FECHA VALOR` <- as.Date(zoo::as.yearmon(data$`FECHA VALOR`, "%d/%m/%Y"))
  data<-data[data$`IMPORTE EUR`<0,]
  categories <- data.frame(
    Category = c("Food", "Payroll", "Residence fee","Maintenance", "Social Security" ),
    key = c("COMIDAPAL","TRANSFERENCIA A FAVOR","TRANSFERENCIA DE","POLSA","TGSS")
  )
  for(i in 1:nrow(data)){
    c <- 0
    for(k in 1:nrow(categories)){
      if(grepl(as.character(categories$key[k]),as.character(data$CONCEPTO[i])) == T){
        data$class[i] <- as.character(categories$Category[k])
        c <- 1
      } 
      if(c == 0) data$class[i] <- "Other"
    }
  }
  # graph by month:
  p3<- ggplot(data = data)+
    stat_summary(fun.y = sum, # adds up all observations for the month
                 geom = "bar", position = "dodge",
                 aes(`FECHA VALOR`, abs(`IMPORTE EUR`) ,fill=class,position = "dodge")) + # or "line"
    scale_x_date(
      labels = date_format("%b"),
      breaks = "1 month")+ # custom x-axis labels
    stat_summary(fun.y = sum, # adds up all observations for the month
                 geom = "smooth",aes(`FECHA VALOR`, abs(`IMPORTE EUR`)))+
    xlab("Month")+ylab("Euros")
  return(p3)
}
