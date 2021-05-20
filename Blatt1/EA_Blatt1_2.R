# Evolutionäre Algorithmen
# Blatt 1 (Abgabe 21.05.2021, 12:00 Uhr)
# Autor: Jonas Herdeg
# Datum: 17.05.2021

# Aufgabe 2

# Erzeugen der Individuen
individual_1 <- sample(0:1, 10, replace = T)
individual_2 <- sample(0:1, 10, replace = T)

iterations <- 10000

one_point_crossover <- function(indiv1, indiv2) {
  point <- sample(1:9, 1)
  
  p1_before <- indiv1[1:(point)]
  p1_after <- indiv1[(point + 1):10]
  
  p2_before <- indiv2[1:(point)]
  p2_after <- indiv2[(point + 1):10]
  
  indiv1 <- c(p1_before, p2_after)
  indiv2 <- c(p2_before, p1_after)
  
  return (list(indiv1, indiv2))
}

two_point_crossover <- function(indiv1, indiv2) {
  point1 <- sample(1:9, 1)
  point2 <- sample(1:9, 1)
  
  if (point1 == point2) {
    return (list(indiv1, indiv2))
  }
  
  if (point1 > point2) {
    help <- point1
    point1 = point2
    point2 = help
  }
  
  p1_before <- indiv1[1:(point1)]
  p1_middle <- indiv1[(point1 + 1):point2]
  p1_after <- indiv1[(point2 + 1):10]
  
  p2_before <- indiv2[1:(point1)]
  p2_middle <- indiv2[(point1 + 1):point2]
  p2_after <- indiv2[(point2 + 1):10]
  
  indiv1 <- c(p1_before, p2_middle, p1_after)
  indiv2 <- c(p2_before, p1_middle, p2_after)
  
  return (list(indiv1, indiv2))
}

uniform_crossover <- function(indiv1, indiv2) {
  child1 <- c()
  child2 <- c()
  
  for (i in 1:10) {
    coin <- sample(0:1, 1)

    if (coin) {
      child1[i] = indiv1[i]
      child2[i] = as.integer(!indiv1[i])
    } else {
      child1[i] = indiv2[i]
      child2[i] = as.integer(!indiv2[i])
    }
  }
  
  if (length(child1) < 10 || length(child2) < 10) {
    cat("Obacht!!!: 1.: ", child1, "2.: ", child2, "\n")
  }
  
  return (list(child1, child2))
}

iterate <- function(indiv1, indiv2, crossover_function, its) {
  individuals1 <- list(indiv1)
  individuals2 <- list(indiv2)
  
  for (i in 1:its) {
    childs <- crossover_function(individuals1[[i]], individuals2[[i]])
    
    individuals1[[i+1]] <- childs[[1]]
    individuals2[[i+1]] = childs[[2]]
    
  }
  return(c(individuals1, individuals2))
}

count_unique <- function(all_individuals) {
  return (length(unique(all_individuals)))
}

cat( iterations, "Iterationen mit folgenden Start-Individuen\n")
cat("Individuum 1: ", individual_1, "\n")
cat("Individuum 2: ", individual_2, "\n")
cat("---------------------\n")

results <- iterate(individual_1, individual_2, one_point_crossover, iterations)
cat(count_unique(results), " verschiedene Individuen mit One-Point-Crossover!\n")

cat("---------------------\n")

results <- iterate(individual_1, individual_2, two_point_crossover, iterations)
cat(count_unique(results), " verschiedene Individuen mit Two-Point-Crossover!\n")

cat("---------------------\n")

results <- iterate(individual_1, individual_2, uniform_crossover, iterations)
cat(count_unique(results), " verschiedene Individuen mit Uniform-Crossover!\n")

# Beobachtung
# One-Point und Two-Point Crossover hängen sehr stark von Initialen Individuen ab.
# Je mehr Positionen identisch sind, desto weniger verschiedene Individuen

