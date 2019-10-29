dat <- read.csv("./data/processed_dat_UMD.csv")
cols <- c("#49C8B1",	"#6A5878",	"#F4683E")
varlist <- c("FoodProvided","Food_lbs","Clothing","Diapers","SchoolKit","HygieneKit")

library(rlang)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(png)
library(shiny)
library(shinydashboard)
library(units)
library(zoo)
library(lubridate)
library(multcomp)
library(flextable)
library(summarytools) 
library(plotly)

numtostr <- function(num) {
  ifelse(num==1,"one",
   ifelse(num==2,"two",
     ifelse(num==3,"three",
      ifelse(num==4,"four",
       ifelse(num==5,"five",
        ifelse(num==6,"six",
         ifelse(num==7,"seven",
          ifelse(num==8,"eight",
           ifelse(num==9,"nine",
            ifelse(num==10,"ten",
             ifelse(num==11,"eleven",
              ifelse(num==12,"twelve",
               ifelse(num==13,"thirteen",
                ifelse(num==14,"fourteen",
                 ifelse(num==15,"fifteen",
                  ifelse(num==16,"sixteen",
                   ifelse(num==17,"seventeen",
                    ifelse(num==18,"eighteen",
                     ifelse(num==19,"nineteen",
                      ifelse(num==20,"twenty", NULL))))
                         ))))))))))))))))
}

# make a theme to remove the outer box
blank_theme <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=16, face="bold")
)

# Q1) Function for Pie Chart
pie.func <- function(data=dat,visits=3) {
  data %>%
    group_by(ClientFileNum) %>%
    count(Year) %>%
    group_by(ClientFileNum) %>%
    count() -> ead # count 
  
  ead$freq <- ifelse(ead$n <= visits, 
                      paste0("le",visits),
                      paste0(">",visits)) # divide into 2 groups. 1)more than n-visits, 2) l.e. n-visits
  ead %>% group_by(freq) %>% count() -> ead.1
  ead.1$p <- round(ead.1$n / sum(ead.1$n),3)*100
  
  # generate a barplot to make pie chart
  bp <- ggplot(ead.1, aes(x="",y=p, fill=freq))+
    geom_bar(width = 1, stat = "identity") +
    theme_bw() +
    # labs(title=paste0("Proportion of Clients with \nmore than ", numtostr(visits), " visits")) +
    scale_fill_manual(name = "Number of Visited Years", 
                      labels = c(paste0("more than ",visits," visits"),
                                 paste0("less or equal to ",visits," visits")),
                      values = cols[c(3, 2)]) +
    theme(legend.position="bottom")
  
  # Geneate into pie chart
  pie <- bp + 
    coord_polar("y", start=0) + 
    theme(axis.text.x = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 15, face = "bold")) +
    blank_theme + 
    geom_text(aes(label = paste0(ead.1$p, "%")), position = position_stack(vjust = 0.5))
    # geom_text(x=c(1.2,1.3), y=c(-6,10),label=scales::percent(ead.1$p/100))
  
  return(pie)
}

# Q2) Recent Higher demand
trendbar.func <- function(data=dat, variable) {
  set.seed(1)
  colpal <- brewer.pal(6, "Set2")
  if (variable=="Food(lbs)") {
    actvar <- "Food_lbs"
    varname <- "Food Amount"
    varlab <- "Food (lbs)"
    colsel <- colpal[1]
  } else if (variable=="Food Provided") {
    actvar <- "FoodProvided"
    varname <- "Food Provided Incidence"
    varlab <- "Food Provided(#)"
    colsel <- colpal[2]
  } else if (variable=="School Kit") {
    actvar <- "SchoolKit"
    varname <- "School Kit" 
    varlab <- "School Kit(#)"
    colsel <- colpal[3]
  } else if (variable=="Hygiene Kit") {
    actvar <- "HygieneKit"
    varname <- "Hygiene Kit"
    varlab <- "Hygiene Kit(#)"
    colsel <- colpal[4]
  } else if (variable=="Clothing") {
    actvar <- variable
    varname <- variable
    varlab <- paste0(variable, "(#)")
    colsel <- colpal[5]
  } else if (variable=="Diapers"){
    actvar <- variable
    varname <- variable
    varlab <- paste0(variable, "(#)")
    colsel <- colpal[6]
  } else {
    actvar <- variable
    varname <- variable
    varlab <- paste0(variable, "(#)")
    colsel <- colpal[6]
  }
  
  dat <- data %>% arrange(Date) # Order the dataset by time order
  dat.2 <- dat %>% dplyr::select(4:11) # select only goods/services and years
  colpal <- brewer.pal(6, "Set2") # generate color palette for the plotss
  
  q2 <- dat.2 %>% 
    group_by(Year) %>%
    summarise(average = mean(!!sym(actvar)))
  
  ggplot(dat=q2, aes(x=Year, y=average, fill=colsel)) +
    geom_bar(stat="identity") +
    labs(title=paste0("Average ", varname, " by Year"), x="Year",y= variable) +
    theme_bw() + 
    scale_fill_manual(name = "", labels = varlab, values=colsel) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
    theme(axis.text.x = element_text(angle = 60)) -> pFp

  return(pFp)
}

# Q3) 
q3.func <- function(data=dat) { 
  data %>% 
    mutate(YearMonth = strftime(Date, "%Y-%m")) %>%
    group_by(YearMonth) %>%
    summarise(BusTicket = mean(BusTicketCount, na.rm=T),
              FoodProvided = mean(FoodProvided, na.rm=T),
              Clothing = mean(Clothing, na.rm=T),
              Diapers = mean(Diapers, na.rm=T),
              SchoolKit = mean(SchoolKit, na.rm=T),
              HygieneKit = mean(HygieneKit, na.rm=T)) %>%
    reshape2::melt(., by = c("YearMonth")) %>%
    mutate(Year = substr(YearMonth,1,4) %>% as.numeric)-> q3
  
  colnames(q3) <- c("YearMonth", "Category", "CaseNum","Year")
  
  q3 <- q3 %>% mutate(YMD = as.Date(paste0(q3$YearMonth, "-01")),
                      Season = ifelse(month(YMD) >= 3 & month(YMD) <= 5, "Spring",
                                      ifelse(month(YMD) >= 6 & month(YMD) <= 8, "Summer", 
                                             ifelse(month(YMD) >= 9 & month(YMD) <= 11, "Fall", "Winter"))))
  q3$Season <- factor(q3$Season, level = c("Spring", "Summer", "Fall", "Winter"))
  
  # Define the four seasons by month:
  # - Mar Apr May: Spring
  # - Jun Jul Aug: Summer
  # - Sep Oct Nov: Fall
  # - Dec Jan Feb: Winter
  
  q3.1 <- q3 %>% 
    group_by(Season,Category) %>% 
    summarise(CaseNum = mean(CaseNum)) %>% 
    arrange(Category) %>%
    as.data.frame()
  
  ggplot(dat = q3.1, aes(x = Season, y = CaseNum, group=Category, colour=Category)) +
    geom_line() +
    geom_point() +
    labs(title = "Trend of Categories by Season", x="Season", y = "Average Number of Cases Filed (per year)", colour="Category") +
    theme_bw() + scale_fill_brewer(palette="Set2") + 
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) 
}


## q4
q4.func <- function(data=dat, yr) {
  q4 <- data %>% 
    dplyr::select(Date, ClientFileNum, FinSupport) %>%
    dplyr::mutate(Year = year(Date)) %>%
    group_by(Year) %>%
    summarise(cnt = n()) 
  
  q4p1 <- ggplot(q4, aes(x= Year, y = cnt))+
    geom_bar(stat="identity", fill = cols[1]) + 
    theme_bw() + 
    geom_vline(xintercept = yr, color="red") +
    labs(title = "Number of Cases Filed Over Time", x = "Year", y = "Number of Cases Filed") +
    scale_fill_brewer(palette="Set2") + 
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

  # q4p2 <- ggplot(q4, aes(x= Year, y = FinSupport, group = Year)) + theme_minimal() +
  #   geom_bar(stat="identity", fill = brewer.pal(3, "Set2")[3]) + theme_bw() + 
  #   labs(title = "Amount of Dollars in Financial Support Over Time", x = "Year", y = "Dollars") +
  #   scale_fill_brewer(palette="Set2") + 
  #   theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

  # gridExtra::grid.arrange(q4p1, q4p2) -> ppp
  # ppp
  
  q4p1
}

## q5
q5.func <- function(data=dat, date) {
  
  q5 <- data %>%
    dplyr::select(Date, ClientFileNum, FinSupport) %>%
    dplyr::mutate(Year = year(Date)) %>%
    group_by(Year,.drop = FALSE) %>%
    summarise(cnt = n())
    
  q5 <- q5 %>%
    add_row(Year=setdiff(1990:2019, q5$Year), cnt=rep(0,length(setdiff(1990:2019, q5$Year)))) %>%
    arrange(Year)
  
  if (q5[q5$Year==date[1],2]==0) {
    result <- paste0("Increase rate in demand of service cannot be calculated due to missing information in year ", date[1]) 
    
  } else {
    rate <- ((q5[which(q5$Year==date[2]),2] - q5[which(q5$Year==date[1]),2])/q5[q5$Year==date[1],2]) %>% round(3)
    rate <- as.numeric(rate)
    result<- paste0("Demand for service from UMD has increased by ", rate, " times from ", date[1], " to ", date[2],".")
  }
  
  print(result)
}

q6.func <- function(data=dat, newyear, compareyear) {
  q6 <- data %>%
    dplyr::select(Date, ClientFileNum, FinSupport) %>%
    dplyr::mutate(Year = year(Date)) %>%
    group_by(Year,.drop = FALSE) %>%
    summarise(cnt = n()) %>%
    filter(Year!=2019)
  
  q6 <- q6 %>%
    add_row(Year=setdiff(1990:2018, q6$Year), cnt=rep(0,length(setdiff(1990:2018, q6$Year)))) %>%
    arrange(Year)
  
  lmr <- lm(data=q6, cnt ~ Year)
  predcnt <- round(lmr$coefficients[2]*newyear + lmr$coefficients[1])
  
  if (q6[q6$Year==compareyear,2]==0) {
    result <- paste0("Increase rate in demand of service cannot be calculated due to missing information in year ", compareyear) 
  } else {
    rate <- ((predcnt - q6[which(q6$Year==compareyear),2])/q6[q6$Year==compareyear,2]) %>% round(3)
    rate <- as.numeric(rate)
    result <- paste0("Demand for service from UMD is expected to be ", rate, " times of ", compareyear, " in ", newyear,".")
  }
  
  return(list(predcnt, result))
}

