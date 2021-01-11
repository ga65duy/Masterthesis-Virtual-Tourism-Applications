library(readxl)
#install.packages("xlsx")
library(xlsx)
d <- read_excel("D:/Masterarbeit/Umfrage/Virtual Tourism Survey.csv/Virtual_Tourism_Survey_Renamed117.xlsx")
#View(Virtual_Tourism_Survey)

cor.test(d$arrowOrientation,d$navigationbarOrientation, method ="spearman")

#Statue complete
statements <- c("libertyInteractionClear",
                "libertyInformationAppropriate",
                "libertyInformationConfusing",
                "libertyNavOptions",
                "navigationbarQuickly",
                "navigationbarEasy",
                "navigationbarUseful",
                "navigationbarOrientation",
                "mapQuickly",
                "mapEasy",
                "mapUseful",
                "mapOrientation",
                "blinkingQuickly",
                "blinkingEasy",
                "blinkingUseful",
                "blinkingOrientation"
                
)
res_matrix <- matrix(0L, nrow=length(statements), ncol=length(statements), dimnames=list(statements, statements))
res_df <- as.data.frame.matrix(res_matrix)
res_df_p <- as.data.frame.matrix(res_matrix)
for(row in 1:nrow(res_matrix)) {
  for(col in 1:ncol(res_matrix)) {
    s1 = d[[statements[row]]]
    s2 = d[[statements[col]]]
    res <- cor.test(s1, s2, method ="spearman")
    res_df[row, col] <- res$estimate[[1]]
    res_df_p[row, col] <- res$p.value
    #res_df[row, col][[1]] <- list(c(res$estimate[[1]], res$p.value))
  }
}
write.xlsx(res_df,"D:/MasterarbeitWörndl/Umfrage/Virtual Tourism Survey.csv/Korrelation.xlsx", sheetName = "StatueRho", append = TRUE)
write.xlsx(res_df_p,"D:/MasterarbeitWörndl/Umfrage/Virtual Tourism Survey.csv/Korrelation.xlsx",sheetName = "StauteP-value", append = TRUE)

#Apps Zugspitze/Statue general verglichen
statements <- c("libertyInteractionClear",
                "libertyInformationAppropriate",
                "libertyInformationConfusing",
                "libertyNavOptions",
               "zugspitzeNavOptions",
                "zugspitzeInteractionClear",
                "zugspitzeInformationAppropriate",
                "zugspitzeInformationConfusing"
              
)
res_matrix <- matrix(0L, nrow=length(statements), ncol=length(statements), dimnames=list(statements, statements))
res_df <- as.data.frame.matrix(res_matrix)
res_df_p <- as.data.frame.matrix(res_matrix)
for(row in 1:nrow(res_matrix)) {
  for(col in 1:ncol(res_matrix)) {
    s1 = d[[statements[row]]]
    s2 = d[[statements[col]]]
    res <- cor.test(s1, s2, method ="spearman")
    res_df[row, col][[1]] <- res$estimate[[1]]
    res_df_p[row, col][[1]] <- res$p.value
  }
}
write.xlsx(res_df,"D:/MasterarbeitWörndl/Umfrage/Virtual Tourism Survey.csv/KorrelationAppNavi.xlsx", sheetName = "AllApps", append = TRUE)
write.xlsx(res_df_p,"D:/MasterarbeitWörndl/Umfrage/Virtual Tourism Survey.csv/KorrelationAppNavi.xlsx",sheetName = "AllAppsP-value", append = TRUE)


#All navoptions compared
statements <- c("navigationbarQuickly",
                "navigationbarEasy",
                "navigationbarUseful",
                "navigationbarOrientation",
                "mapQuickly",
                "mapEasy",
                "mapUseful",
                "mapOrientation",
                "blinkingQuickly",
                "blinkingEasy",
                "blinkingUseful",
                "blinkingOrientation",
                "arrowQuickly",
                "arrowEasy",
                "arrowUseful",
                "arrowOrientation", 
                "listQuickly",
                "listEasy",
                "listUseful",
                "listOrientation"
                
)
res_matrix <- matrix(0L, nrow=length(statements), ncol=length(statements), dimnames=list(statements, statements))
res_df <- as.data.frame.matrix(res_matrix)
res_df_p <- as.data.frame.matrix(res_matrix)
for(row in 1:nrow(res_matrix)) {
  for(col in 1:ncol(res_matrix)) {
    s1 = d[[statements[row]]]
    s2 = d[[statements[col]]]
    res <- cor.test(s1, s2, method ="spearman")
    res_df[row, col][[1]] <- res$estimate[[1]]
    res_df_p[row, col][[1]] <- res$p.value
  }
}
write.xlsx(res_df,"D:/MasterarbeitWörndl/Umfrage/Virtual Tourism Survey.csv/KorrelationAppNavi.xlsx", sheetName = "AllNavis", append = TRUE)
write.xlsx(res_df_p,"D:/MasterarbeitWörndl/Umfrage/Virtual Tourism Survey.csv/KorrelationAppNavi.xlsx",sheetName = "AllNavisP-value", append = TRUE)


#Zugspitze complete
statements <- c("zugspitzeNavOptions",
                "zugspitzeInteractionClear",
                "zugspitzeInformationAppropriate",
                "zugspitzeInformationConfusing",
                "arrowQuickly",
                "arrowEasy",
                "arrowUseful",
                "arrowOrientation", 
                "listQuickly",
                "listEasy",
                "listUseful",
                "listOrientation"
                
)
res_matrix <- matrix(0L, nrow=length(statements), ncol=length(statements), dimnames=list(statements, statements))
res_df <- as.data.frame.matrix(res_matrix)
res_df_p <- as.data.frame.matrix(res_matrix)
for(row in 1:nrow(res_matrix)) {
  for(col in 1:ncol(res_matrix)) {
    s1 = d[[statements[row]]]
    s2 = d[[statements[col]]]
    res <- cor.test(s1, s2, method ="spearman")
    res_df[row, col][[1]] <- res$estimate[[1]]
    res_df_p[row, col][[1]] <- res$p.value
  }
}
write.xlsx(res_df,"D:/MasterarbeitWörndl/Umfrage/Virtual Tourism Survey.csv/Korrelation.xlsx", sheetName = "ZugRho", append = TRUE)
write.xlsx(res_df_p,"D:/MasterarbeitWörndl/Umfrage/Virtual Tourism Survey.csv/Korrelation.xlsx",sheetName = "ZugP-value", append = TRUE)



#STATUE general
statements <- c("libertyInteractionClear",
                "libertyInformationAppropriate",
                "libertyInformationConfusing",
                "libertyNavOptions"
                )
res_matrix <- matrix(0L, nrow=length(statements), ncol=length(statements), dimnames=list(statements, statements))
res_df <- as.data.frame.matrix(res_matrix)
for(row in 1:nrow(res_matrix)) {
  for(col in 1:ncol(res_matrix)) {
    s1 = d[[statements[row]]]
    s2 = d[[statements[col]]]
    res <- cor.test(s1, s2, method ="spearman")
    res_df[row, col][[1]] <- list(c(res$estimate[[1]], res$p.value))
  }
}


#Statue navigation NavigationBar and all
statements <- c("libertyNavOptions",
                "libertyInteractionClear",
                "libertyInformationAppropriate",
                "libertyInformationConfusing",
                "navigationbarQuickly",
                "navigationbarEasy",
                "navigationbarUseful",
                "navigationbarOrientation"
                
)
res_matrix <- matrix(0L, nrow=length(statements), ncol=length(statements), dimnames=list(statements, statements))
res_df <- as.data.frame.matrix(res_matrix)
for(row in 1:nrow(res_matrix)) {
  for(col in 1:ncol(res_matrix)) {
    s1 = d[[statements[row]]]
    s2 = d[[statements[col]]]
    res <- cor.test(s1, s2, method ="spearman")
    res_df[row, col][[1]] <- list(c(res$estimate[[1]], res$p.value))
  }
}

#Statue navigation Map
#Statue navigation NavigationBar and all
statements <- c("libertyNavOptions",
                "libertyInteractionClear",
                "libertyInformationAppropriate",
                "libertyInformationConfusing",
                "mapQuickly",
                "mapEasy",
                "mapUseful",
                "mapOrientation"
                
)
res_matrix <- matrix(0L, nrow=length(statements), ncol=length(statements), dimnames=list(statements, statements))
res_df <- as.data.frame.matrix(res_matrix)
for(row in 1:nrow(res_matrix)) {
  for(col in 1:ncol(res_matrix)) {
    s1 = d[[statements[row]]]
    s2 = d[[statements[col]]]
    res <- cor.test(s1, s2, method ="spearman")
    res_df[row, col][[1]] <- list(c(res$estimate[[1]], res$p.value))
  }
}


#Statue navigation Blinking
#Statue navigation Map
#Statue navigation NavigationBar and all
statements <- c("libertyNavOptions",
                "libertyInteractionClear",
                "libertyInformationAppropriate",
                "libertyInformationConfusing",
                "blinkingQuickly",
                "blinkingEasy",
                "blinkingUseful",
                "blinkingOrientation"
                
)
res_matrix <- matrix(0L, nrow=length(statements), ncol=length(statements), dimnames=list(statements, statements))
res_df <- as.data.frame.matrix(res_matrix)
for(row in 1:nrow(res_matrix)) {
  for(col in 1:ncol(res_matrix)) {
    s1 = d[[statements[row]]]
    s2 = d[[statements[col]]]
    res <- cor.test(s1, s2, method ="spearman")
    res_df[row, col][[1]] <- list(c(res$estimate[[1]], res$p.value))
  }
}

#Zugspitze general
#Statue navigation Map
#Statue navigation NavigationBar and all
statements <- c("zugspitzeNavOptions",
                "zugspitzeInteractionClear",
                "zugspitzeInformationAppropriate",
                "zugspitzeInformationConfusing",
                "arrowQuickly",
                "arrowEasy",
                "arrowUseful",
                "arrowOrientation"
                
)
res_matrix <- matrix(0L, nrow=length(statements), ncol=length(statements), dimnames=list(statements, statements))
res_df <- as.data.frame.matrix(res_matrix)
for(row in 1:nrow(res_matrix)) {
  for(col in 1:ncol(res_matrix)) {
    s1 = d[[statements[row]]]
    s2 = d[[statements[col]]]
    res <- cor.test(s1, s2, method ="spearman")
    res_df[row, col][[1]] <- list(c(res$estimate[[1]], res$p.value))
  }
}
#Zugspitze navigation List
#Zugspitze general
#Statue navigation Map
#Statue navigation NavigationBar and all
statements <- c("zugspitzeNavOptions",
                "zugspitzeInteractionClear",
                "zugspitzeInformationAppropriate",
                "zugspitzeInformationConfusing",
                "listQuickly",
                "listEasy",
                "listUseful",
                "listOrientation"
                
)
res_matrix <- matrix(0L, nrow=length(statements), ncol=length(statements), dimnames=list(statements, statements))
res_df <- as.data.frame.matrix(res_matrix)
for(row in 1:nrow(res_matrix)) {
  for(col in 1:ncol(res_matrix)) {
    s1 = d[[statements[row]]]
    s2 = d[[statements[col]]]
    res <- cor.test(s1, s2, method ="spearman")
    res_df[row, col][[1]] <- list(c(res$estimate[[1]], res$p.value))
  }
}