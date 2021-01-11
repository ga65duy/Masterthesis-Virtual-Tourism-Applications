library(HH)
#library(likert)
library(plyr)
library(readxl)
library(tidyverse)

d <- read_excel("D:/Masterarbeit/Umfrage/Virtual Tourism Survey.csv/Virtual_Tourism_Survey_Renamed117.xlsx")

# Select the questions here
data_wide <- d %>% select(zugspitzeInteractionClear,
                          zugspitzeInformationAppropriate,
                          zugspitzeInformationConfusing,
                          zugspitzeNavOptions,
                          libertyInteractionClear,
                          libertyInformationAppropriate,
                          libertyInformationConfusing,
                          libertyNavOptions
) %>%
  mutate_all(factor, levels=c("1", "2", "3", "4", "5"))

#Create groups
group_labels <- rep(c( "Statue of Liberty", "Zugspitze360"), each=4)

# Rename here
from <- c("zugspitzeInteractionClear",
          "zugspitzeInformationAppropriate",
          "zugspitzeInformationConfusing",
          "zugspitzeNavOptions",
          "libertyInteractionClear",
          "libertyInformationAppropriate",
          "libertyInformationConfusing",
          "libertyNavOptions")
to <- c("2-1- Clear interaction",
        "2-2- Appropriate information",
        "2-3- Confusing information",
        "2-4- Right number of navigations",
        "1-1- Clear interaction",
        "1-2- Appropriate information",
        "1-3- Confusing information",
        "1-4- Right number of navigations")

# R melting data from wide to long format
data_long <- data_wide %>% gather(question, Rating) 
data_cont <- as.data.frame.matrix(table(data_long))
data_cont <- cbind(question = rownames(data_cont), data_cont)
data_cont <- cbind(data_cont, group=group_labels)
rownames(data_cont) <- NULL
data_cont$question <- plyr::mapvalues(data_cont$question, from, to)
data_cont <- data_cont[order(data_cont$question),]

#rownames(data_cont) <- plyr::mapvalues(rownames(data_cont), from, to)

title <- "Applications"
p <- likert(question ~ .| group,
            data=data_cont,
            scales=list(y=list(relation="free", cex=1.2),
                        x=list(draw=FALSE)),
            strip.left=strip.custom(bg="gray97"),
            strip=FALSE,
            par.strip.text=list(cex=1.2, lines=2),
            ylab="",
            xlab="",
            layout=c(1,2),
            as.percent="noRightAxis",
            main=title,
            panel=function(...) {
              panel.barchart(...) 
              tmp <- list(...)
              print(tmp)
              tmp <- data.frame(x=tmp$x, y=tmp$y)
              # calculate positions of text labels
              df <- ddply(tmp, .(y),
                          function(x) {
                            percentages = c(5,10,15,30,45)
                            pos = c(-20,-10,5,30,50)
                            
                            x_sorted = sort(x$x)
                            rating_three_percentage = sum(abs(x$x[1]), abs(x$x[4]))
                            rating_three_position = 0
                            
                            rating_four_five_percentage = x$x[5:6]
                            rating_four_five_position = cumsum(x$x[4:6])[1:2] + (x$x[5:6]/2)
                            
                            rating_one_two_percentage = x$x[2:3]
                            rating_one_two_position = cumsum(x$x[1:3])[1:2] + (x$x[2:3]/2)
                            
                            percentages[1:2] = rating_one_two_percentage
                            pos[1:2] = rating_one_two_position
                            
                            percentages[3] = rating_three_percentage
                            pos[3] = rating_three_position
                            
                            percentages[4:5] = rating_four_five_percentage
                            pos[4:5] = rating_four_five_position
                            
                            data.frame(x=percentages, pos=pos)
                          })
              panel.text(x=df$pos, y=df$y,
                         label=ifelse(abs(df$x)>= 7, paste0(sprintf("%.0f", abs(df$x)),"%"),""),
                         cex=1.0)
              
            })
plot(p)

