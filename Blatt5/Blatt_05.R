# Evolutionaere Algorithmen
# Blatt 5 - Uebung 4

library(ggplot2)
library(ggalt)
library(gganimate)
library(gifski)

init_population <- function(pop_size, genes, bounds) {
  return (data.frame(individual = c(1:pop_size),
                     matrix(runif(pop_size * genes, bounds[1], bounds[2]), nrow = pop_size),
                     generation= 0))
}

permutate_parents <- function(population, pop_size) {
  return (population[sample(nrow(population), pop_size),])
}

create_pdf <- function(df, title) {
  pdf(paste(title, ".pdf"))
  for (g in unique(df$generation)) {
    plt <- ggplot(data=df[df$generation == g,],
                mapping = aes(X1, X2, colour = individual)) +
      geom_encircle(s_shape = 1, expand=0, fill = "rosybrown1") +
      geom_point(size = 2) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      labs(title = paste("Generation: ", g))
    print(plt)
  }
  dev.off()
}

create_gif <- function(df, title) {
  anim <- ggplot(
    data = df,
    aes(x = X1, y = X2, colour = individual)
  ) +
    geom_encircle(s_shape = 1, expand=0, fill = "rosybrown1") +
    geom_point(size = 2) +
    transition_states(generation,
                      transition_length = 2,
                      state_length = 2) +
    labs(title = "Generation: {closest_state}")
  
  animate(anim, duration = 14, fps = 30, width = 500, height = 500, renderer = gifski_renderer())
  anim_save(paste(title, ".gif"))
}

pop_size = 20
population <- init_population(pop_size, 2, c(0, 1))

# mode = 1: single arithmetic Recombination
# mode = 2: Whole arithmetic recombination
# alpha = -1: random alpha for each parent-pair
run_EA <- function(pop, pop_size, genes, generations, alpha, mode , title) {
  a = alpha
  
  all_gens <- pop
  
  for (i in 1:generations) {
    pop <- permutate_parents(pop, pop_size)

      j = 1
      while (j < pop_size) {
        if (alpha == -1) a = runif(1)
        if (mode == 1) {
          pos <- sample(2:3, 1)
          child1_val = a * pop[j, pos] + (1 - a) * pop[j+1, pos]
          child2_val = a * pop[j+1, pos] + (1 - a) * pop[j, pos]
          pop[j, pos] = child1_val
          pop[j+1, pos] = child1_val
        } else if (mode == 2) {
          child1 = a * pop[j, 2:3] + (1 - a) * pop[j+1, 2:3]
          child2 = a * pop[j+1, 2:3] + (1 - a) * pop[j, 2:3]
          pop[j, 2:3] = child1
          pop[j+1, 2:3] = child2
        }
        pop$generation = i
        j = j + 2
      }
      pop <- pop[order(pop$individual),]
    all_gens <- rbind(all_gens, pop)
  }
  create_pdf(all_gens, title)
  create_gif(all_gens, title)
  
  return (all_gens)
}

pop1 <- run_EA(population, 20, 2, 10, 0.5, 1, "single_fixed")
pop2 <- run_EA(population, 20, 2, 10, -1, 1, "single_random")
pop3 <- run_EA(population, 20, 2, 10, 0.5, 2, "whole_fixed")
pop4 <- run_EA(population, 20, 2, 10, -1, 2, "whole_random")
