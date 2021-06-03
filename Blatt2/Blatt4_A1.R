# Blatt 3
# Aufgabe 1
# a) b)

create_population <- function(n, borders) {
  return (runif(n, borders[1], borders[2]))
}

a <- c(0, 1, 2, 4, 10)
b <- c(0,1,2,4,10)

transform_fitnessfunc <- function(pop, a, b) {
  return (a * pop + b)
}

relative_fitness <- function(pop) {
  if (sum(pop) == 0) return (pop)
  return (pop / sum(pop))
}

create_pdf <- function(data_list) {
  pdf("Transformationen_Fitness.pdf")
  for (entry in data_list) {
    barplot(main = paste(entry[[1]], " * fit + ",entry[[2]]),
            entry[[4]],
            xlab = "Individuals",
            ylab = "relative fitness",
            ylim = c(0, 0.2))
  }
  dev.off()
}

population <- create_population(10, c(0, 1))

transformed_fitness_values <- list(a * b)
i <- 1

for (b_value in b) {
  for (a_value in a) {
    entry <- list(4)
    entry[[1]] = a_value
    entry[[2]] = b_value
    entry[[3]] = transform_fitnessfunc(population, a_value, b_value)
    entry[[4]] = relative_fitness(entry[[3]])
    transformed_fitness_values[[i]] = entry
    
    i = i + 1
  }
}

create_pdf(transformed_fitness_values)

# Aufgabe c)
# Die Multiplikation der Fitness (hier mit a) bewirkt, dass die absoluten Fitnesswerte skaliert werden.
# Die relativen Fitnesswerte bleiben jedoch gleich.
# Die Addition (hier mit b) bewirkt, dass eine insgesamt höhere Fitness in der Population ist
# Die absoluten Unterschiede zwischen den Individuen ändern sich allerdings nicht.
# Die Addition bewirkt dementsprechend, dass die relativen Fitnesswerte näher zusammenrücken.
# Bei der Kombination beider Operatoren lässt sich beobachten, dass durch die Skalierung der Multiplikation bei niedrigen Offsets


# Aufgabe 2
# Teilaufgabe a)

select_parents <- function(pop) {
  next_gen = c()
  max <- sum(pop)
  
  for (i in 1:length(pop)) {
    pick <- runif(1, 0 , max)
    cur <- 0
    for (indiv in pop) {
      cur = indiv + cur
      if (cur >= pick) {
        next_gen[i] = indiv
        break;
      }
    } 
  }
  
  return (next_gen)
}

generate_pdf <- function(pops, title) {
  pdf(title)
  for (i in pops) {
    pie(i)
  }
  dev.off()
}

run_EA <- function(n_gens, pop, a, b, pdf_title) {
  populations <- list(n_gens + 1)
  populations[[1]] = pop
  for (i in 1:n_gens) {
    pop = transform_fitnessfunc(pop, a, b)
    pop = select_parents(pop)
    populations[[i + 1]] = pop
  }
  generate_pdf(populations, pdf_title)
}

run_EA(100, population, 1, 0, "Aufgabe2_a.pdf")
run_EA(100, population, 3, 10, "Aufgabe2_b.pdf")

# Beobachtung:
# In b) sind die relativen Wahrscheinlichkeiten durch die Transformation deutlich gleicher Verteilt.
# In a) kommen die Populationen dadurch (meistens) schneller zur finalen Population, als die Populationen in b).
# Beide Varianten konvergieren aber recht früh (a: ca. Generation 11, b: ca. Generation 13 (je nach Startpopulation)) zu einer einheitlichen Population mit gleichverteilten relativen Wahrscheinlichkeiten.
