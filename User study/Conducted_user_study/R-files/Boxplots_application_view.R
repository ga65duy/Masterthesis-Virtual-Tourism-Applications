library(readxl)
Virtual_Tourism_Survey <- read_excel("D:/Masterarbeit/Umfrage/Virtual Tourism Survey.csv/Virtual Tourism Survey.xlsx")
#View(Virtual_Tourism_Survey)

y = c(1, 5)
xapp = c("Clear \n interaction","Appropriate \n information", "Confusing \n information","Right number \n of navigations") 

#Zugspitze
png(file="ZugspitzeKomplett.png")
data <- data.frame(
  Zugspitze = Virtual_Tourism_Survey$`The interaction with the app is clear and understandable._9`,
  Zugspitze2 = Virtual_Tourism_Survey$`The amount of information displayed on screen was appropriate._10`,
  Zugspitze3 = Virtual_Tourism_Survey$`The information displayed on screen is confusing._11`,
  Zugspitze4 = Virtual_Tourism_Survey$`The number of navigation options was just right._20`
)
b = boxplot(data, ylim = y, names = xapp, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Application Zugspitze360", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()


#Statue komplett übergreifend
png(file="StatueKomplettallgemein.png")
data <- data.frame(
       Statue = Virtual_Tourism_Survey$`The interaction with the app is clear and understandable.`,
       Statue2 = Virtual_Tourism_Survey$`The amount of information displayed on screen was appropriate.`,
       Staute3 = Virtual_Tourism_Survey$`The information displayed on screen is confusing.`,
       Staute4 = Virtual_Tourism_Survey$`The number of navigation options was just right.`
)
b = boxplot(data, ylim = y, names = xapp, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Application Statue of Liberty", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()

#Daten innerhalb navigation und app zusammengefasst
#Statue Navigation Bar
xnavi = c("Found \n navigation", "Easy \n navigation", "Useful \n navigation", "Kept \n orientation")
# I found the navigation type quickly
png(file="NavigationBar.png")
data<- data.frame(
  Staute = Virtual_Tourism_Survey$`I found the navigation type quickly.`,
  Staute1 = Virtual_Tourism_Survey$`It was easy to navigate to a place.`,
  Staute2 = Virtual_Tourism_Survey$`The navigation type is useful to switch places.`,
  Staute3 = Virtual_Tourism_Survey$`I kept the orientation when switching the places.`
  )
b = boxplot(data, ylim = y, names = xnavi, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Navigationbar", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()


# Statue Map
png(file="Map.png")
data<- data.frame(
  Staute = Virtual_Tourism_Survey$`I found the navigation type quickly._1`,
  Staute1 = Virtual_Tourism_Survey$`It was easy to navigate to a place._2`,
  Staute2 = Virtual_Tourism_Survey$`The navigation type is useful to switch places._3`,
  Statue3 = Virtual_Tourism_Survey$`I kept the orientation when switching the places._4`
)
b = boxplot(data, ylim = y, names = xnavi, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Map", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()


# STATUE Blinking icon
png(file="Blinking.png")
data<- data.frame(
  Statue = Virtual_Tourism_Survey$`I found the navigation type quickly._5`,
  Statue1 = Virtual_Tourism_Survey$`It was easy to navigate to a place._6`,
  Statue2 = Virtual_Tourism_Survey$`The navigation type is useful to switch places._7`,
  Statue3 = Virtual_Tourism_Survey$`I kept the orientation when switching the places._8`
)
b = boxplot(data, ylim = y, names = xnavi, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Blinking icon", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()

# Zugspitze Arrows
png(file="ZugspitzeArrows.png")
data<- data.frame(
  Zug = Virtual_Tourism_Survey$`I found the navigation type quickly._13`,
  Zug1 = Virtual_Tourism_Survey$`It was easy to navigate to another place.`,
  Zug2 = Virtual_Tourism_Survey$`The navigation type is useful to switch places._14`,
  Zug3 = Virtual_Tourism_Survey$`I kept the orientation when switching the places._15`
)
b = boxplot(data, ylim = y, names = xnavi, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Arrows", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()

# Zugspitze List
png(file="ZugspitzeList.png")
data<- data.frame(
  Zug = Virtual_Tourism_Survey$`I found the navigation type quickly._16`,
  Zug1 = Virtual_Tourism_Survey$`It was easy to navigate to another place._17`,
  Zug2 = Virtual_Tourism_Survey$`The navigation type is useful to switch places._18`,
  Zug3 = Virtual_Tourism_Survey$`I kept the orientation when switching the places._19`
)
b = boxplot(data, ylim = y, names = xnavi, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("List", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()

