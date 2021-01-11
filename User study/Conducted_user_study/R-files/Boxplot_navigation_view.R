library(readxl)
Virtual_Tourism_Survey <- read_excel("D:/Masterarbeit/Umfrage/Virtual Tourism Survey.csv/Virtual Tourism Survey.xlsx")
#View(Virtual_Tourism_Survey)

y = c(1, 5)
yapp = c("Statue of Liberty","Zugspitze360") 

#The interaction with the app is clear and understandable.
png(file="Boxplots übergreifend/ApplicationClear.png")
data <- data.frame(
  Statue = Virtual_Tourism_Survey$`The interaction with the app is clear and understandable.`,
  Zugspitze = Virtual_Tourism_Survey$`The interaction with the app is clear and understandable._9`)
boxplot(data, ylim = y, names=yapp)
mtext("Application is clear and understandable", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()

#The amount of information displayed on screen was appropriate.
png(file="Boxplots übergreifend/AppropriateInformation.png")
data <- data.frame(
  Statue = Virtual_Tourism_Survey$`The amount of information displayed on screen was appropriate.`,
  Zugspitze = Virtual_Tourism_Survey$`The amount of information displayed on screen was appropriate._10`)
boxplot(data, ylim = y, names=yapp)
mtext("Appropriate amount of information", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()

# The information displayed on screen is confusing.
png(file="Boxplots übergreifend/InfoConfusing.png")
data<- data.frame(
  Staute = Virtual_Tourism_Survey$`The information displayed on screen is confusing.`,
  Zugspitze = Virtual_Tourism_Survey$`The information displayed on screen is confusing._11`)
boxplot(data, ylim = y, names=yapp)
mtext("Displayed information is confusing", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()

# The number of navigation options was just right.
png(file="Boxplots übergreifend/NumberNavigations.png")
data<- data.frame(
  Staute = Virtual_Tourism_Survey$`The number of navigation options was just right.`,
  Zugspitze = Virtual_Tourism_Survey$`The number of navigation options was just right._20`
)
b = boxplot(data, ylim = y, names = yapp, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Right number of navigation options", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()


#NAVIGATIONSMÃ–GLICHKEITEN
xnavi = c("Navigation \n bar", "Map", "Blinking \n icon", "List", "Arrows")
# I found the navigation type quickly
png(file="Boxplots übergreifend/FoundQuick.png")
data<- data.frame(
  Staute = Virtual_Tourism_Survey$`I found the navigation type quickly.`,
  Staute2 = Virtual_Tourism_Survey$`I found the navigation type quickly._1`,
  Statue3 = Virtual_Tourism_Survey$`I found the navigation type quickly._5`,
  Zugspitze = Virtual_Tourism_Survey$`I found the navigation type quickly._13`,
  Zugspitze2 = Virtual_Tourism_Survey$`I found the navigation type quickly._16`
)
b = boxplot(data, ylim = y, names = xnavi, xaxt="n" )
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Found navigation type quickly", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()


# It was easy to navigate to a place
png(file="Boxplots übergreifend/EasyNavigation.png")
data<- data.frame(
  Staute = Virtual_Tourism_Survey$`It was easy to navigate to a place.`,
  Staute2 = Virtual_Tourism_Survey$`It was easy to navigate to a place._2`,
  Statue3 = Virtual_Tourism_Survey$`It was easy to navigate to a place._6`,
  Zugspitze = Virtual_Tourism_Survey$`It was easy to navigate to another place.`,
  Zugspitze2 = Virtual_Tourism_Survey$`It was easy to navigate to another place._17`
)
b = boxplot(data, ylim = y, names = xnavi, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Easy navigation", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()


# The navigation type is useful to switch places.
png(file="Boxplots übergreifend/NavigationtypeUsefullSwitch.png")
data<- data.frame(
  Staute = Virtual_Tourism_Survey$`The navigation type is useful to switch places.`,
  Staute2 = Virtual_Tourism_Survey$`The navigation type is useful to switch places._3`,
  Statue3 = Virtual_Tourism_Survey$`The navigation type is useful to switch places._7`,
  Zugspitze = Virtual_Tourism_Survey$`The navigation type is useful to switch places._14`,
  Zugspitze2 = Virtual_Tourism_Survey$`The navigation type is useful to switch places._18`
)
b = boxplot(data, ylim = y, names = xnavi, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Useful for switching places", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()

# I kept the orientation when switching the places.
png(file="Boxplots übergreifend/KeptOrientation.png")
data<- data.frame(
  Staute = Virtual_Tourism_Survey$`I kept the orientation when switching the places.`,
  Staute2 = Virtual_Tourism_Survey$`I kept the orientation when switching the places._4`,
  Statue3 = Virtual_Tourism_Survey$`I kept the orientation when switching the places._8`,
  Zugspitze = Virtual_Tourism_Survey$`I kept the orientation when switching the places._15`,
  Zugspitze2 = Virtual_Tourism_Survey$`I kept the orientation when switching the places._19`
)
b = boxplot(data, ylim = y, names = xnavi, xaxt="n")
axis(side = 1, at = seq_along(b$names), labels = b$names, tick = FALSE)
mtext("Kept orienatation while switching", side = 3, line = 1)
data_means <- apply(data, 2, mean) # Compute mean for each column of data
points(data_means, pch = 4) # Plot the means
dev.off()

