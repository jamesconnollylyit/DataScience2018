library(swirl)
install_course("R Programming")
swirl()

vec1 <- c("Hockey", "Football", "Baseball", "Curling", "Rugby", "Hurling", "Basketball", "Tennis", "Cricket", "Lacrosse")
vec2 <- c(vec1, "Hockey", "Lacrosse", "Hockey", "Water Polo", "Hockey", "Lacrosse")
vec2_factor <- as.factor(vec2)
vec2_factor

install.packages("Swirlify")
