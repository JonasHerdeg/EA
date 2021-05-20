# Evolutionäre Algorithmen
# Blatt 1 (Abgabe 21.05.2021, 12:00 Uhr)
# Autor: Jonas Herdeg
# Datum: 17.05.2021

# Aufgabe 1

# Funktion und Ableitung g(x)
g <- function(x) {
  return (x)
}

g_der <- function(x) {
  return (1)
}

# Funktion und Ableitung k(x)
k <- function(x) {
  return (sin(x))
}

k_der <- function(x) {
  return (cos(x))
}

# Funktion und Ableitung h(x)
h <- function(x) {
  return (x * sin(x))
}

h_der <- function(x) {
  return (sin(x) + x * cos(x))
}

# Funktion und Ableitung l(x) 
l <- function(x) {
  return (2 + cos(x) + sin(x)) 
}

l_der <- function(x) {
  return (cos(x) - sin(x)) 
}

# Parameter für die Berechnungen
functions <- c(g, k, h, l)
derivations <- c(g_der, k_der, h_der, l_der)
eta_values <- c(0.01, 0.1, 0.25)
start_values <- c(0, 5, 10)
iterations <- 100

# Teilaufgabe a)

# Berechnet den Gradientenaufstieg
gradientenaufstieg <- function(x , eta, func) {
  if (!is.function(func)) stop("argument func is not a function")
  return (x + eta * func(x))
}

# Führt den Gradientenaufstieg für die gegebenen Konfigurationen durch
iterate <- function(derivation, etas, its, starters) {
  
  if (!is.function(derivation)) stop("argument derivation is not a function")
  
  for (start_value in starters) {
    for (eta_value in etas) {
      x_current <- start_value
      for (i in 1:its) {
        x_current <- gradientenaufstieg(x_current, eta_value, derivation)
      }
      print_information(start_value, eta_value, its, x_current)
    }
  }
}

print_information <- function(start_x, eta, its, result) {
  cat("Werte fuer Startposition: ", start_x, ", Eta-Wert: ", eta, ", bei ", its, " Iterationen: ", result, "\n")
}

cat("f(x) = g(x)\n")
cat("-------------------------\n")
iterate(g_der, eta_values, iterations, start_values)

cat("\n")
cat("f(x) = k(x)\n")
cat("-------------------------\n")
iterate(k_der, eta_values, iterations, start_values)

cat("\n")
cat("f(x) = h(x)\n")
cat("-------------------------\n")
iterate(h_der, eta_values, iterations, start_values)

cat("\n")
cat("f(x) = l(x)\n")
cat("-------------------------\n")
iterate(l_der, eta_values, iterations, start_values)

cat("\n--------------------------------------------\n\n")

# Teilaufgabe b)
# Initiale Population mit zufälligem Wert mit größe 10
population_size = 10
population <- runif(population_size, 0.0, 10.0)

# Grenzen der Fitnessfunktion
bounds <- c(0, 10)

# Deltas
deltas <- c(0.01, 0.1, 0.25)

mutate_children <- function(child_pop, delta, upper, lower) {
  
  for (i in 1:length(child_pop)) {
    child_pop[i] = child_pop[i] + sample(c(1, (-1)), 1) * delta
    if (child_pop[i] > upper) child_pop[i] = upper
    if (child_pop[i] < lower) child_pop[i] = lower
  }
  
  return (child_pop)
}

mutate_children_gauss <- function(child_pop, upper, lower) {
  
  for (i in 1:length(child_pop)) {
    child_pop[i] = child_pop[i] + sample(c(1, (-1)), 1) * rnorm(1, 0.2, 0.2)
    if (child_pop[i] > upper) child_pop[i] = upper
    if (child_pop[i] < lower) child_pop[i] = lower
  }
  
  return (child_pop)
}

select_fittest <- function(pop, child_pop, fitness_function, pop_size) {
  pop <- c(pop, child_pop)
  
  individual_fitness <- c()
  for (i in 1:(pop_size*2)) {
    individual_fitness[i] = fitness_function(pop[i])
  }
  
  indices <- which(individual_fitness >= sort(individual_fitness, decreasing = T)[pop_size], arr.ind = TRUE)
  return (pop[indices])
}

iterate_generations <- function(pop, deltas, func, iterations, pop_size, bounds) {
  initial_pop <- pop
  for (delta in deltas) {
    pop <- initial_pop
    for (i in 1:iterations) {
      child_pop <- mutate_children(pop, delta, bounds[2], bounds[1])
      pop <- select_fittest(pop, child_pop, func, pop_size)
    }
    print_information_b(delta, pop, bounds, iterations)
  }
  
  pop <- initial_pop
  for (i in iterations) {
    child_pop <- mutate_children_gauss(pop, bounds[2], bounds[1])
    pop <- select_fittest(pop, child_pop, func, pop_size)
  }
  print_information_b("Gauss", pop, bounds, iterations)
}

print_information_b <- function(delta, pop, bounds, its) {
  cat("Werte fuer Delta: ", delta, ", Untergrenze: ", bounds[1], ", Obergrenze", bounds[2], ", bei ", its, " Iterationen: ", pop, "\n\n")
}

cat("f(x) = g(x)\n")
iterate_generations(population, deltas, g, 100, 10, bounds)

cat("\n")
cat("f(x) = k(x)\n")
cat("-------------------------\n")
iterate_generations(population, deltas, k, 100, 10, bounds)

cat("\n")
cat("f(x) = h(x)\n")
cat("-------------------------\n")
iterate_generations(population, deltas, h, 100, 10, bounds)

cat("\n")
cat("f(x) = l(x)\n")
cat("-------------------------\n")
iterate_generations(population, deltas, l, 100, 10, bounds)

# Teilaufgabe c)
# Beobachtungen:
# Beide Varianten verhalten sich sehr ähnlich, da sie sich für jede Funktion einem lokalen Maximum annähern.
# Bei der evolutionären Variante (Teilaufgabe b) kann es für die Funktionen k(x), h(x) und l(x) sein, dass verschiedene lokale Maxima der Fitnessfunktion von den Individuen repräsentiert werden.
# Die eta / delta Werte haben einen Einfluss darauf, wie schnell die Population zu einem Maximum konvergiert.
