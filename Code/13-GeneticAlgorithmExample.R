#====================================================================
# purpose: simple example of a genetic algorithm in R
# author: tirthankar chakravarty
# created:
# revised:
# comments: this example shows:
# - how to solve an optimization problem using the R `genalg` package
# - how to produce an animation of the solution
#====================================================================
library(genalg)
library(ggplot2)
library(animation)
library(pryr)

# choices for the backpack
backpackChoices = data.frame(
  item = c("pocketknife", "beans", "potatoes", "unions", 
           "sleeping bag", "rope", "compass"), 
  survivalpoints = c(10, 20, 15, 2, 30, 
                     10, 30), 
  weight = c(1, 5, 10, 1, 7, 5, 1))

# weight limit of the backpack
weightlimit = 20

# evaluation function: evaluate chromosomes (gene configuration)
evalChromosome = function(chromosome) {
  survivalpoints = chromosome %*% backpackChoices$survivalpoints
  weight = chromosome %*% backpackChoices$weight
  if(weight > weightlimit) return(0) else return(-survivalpoints)
}

# optimize the backpack selection using a binary genetic algorithm
modelSurvival = rbga.bin(size = 7, # 7 genes in the chromosome
                         popSize = 200, # population size
                         iters = 100,  # number of population generations
                         mutationChance = 0.01,
                         elitism = TRUE,
                         evalFunc = evalChromosome)

# function to retrieve optimal solution
getBestSolution = function(rbga.object) {
  filter = rbga.object$evaluations == min(rbga.object$evaluations)
  bestObjectCount = sum(rep(1, rbga.object$popSize)[filter])
  if (bestObjectCount > 1) {
    bestSolution = rbga.object$population[filter, ][1, 
                                                    ]
  }
  else {
    bestSolution = rbga.object$population[filter, ]
  }
  return(bestSolution)
}

# compute the weight and survival points for the best solution
getBestSolution(modelSurvival) %*% backpackChoices$survivalpoints
getBestSolution(modelSurvival) %*% backpackChoices$weight

# get the evolution of the model
animatePlot = function(rbga.object, iter) {
  for(i in seq(1, iter)) {
    dfTemp = data.frame(generation = rep(seq(1, i), 2),
                        variable = c(rep("best", i), rep("mean", i)),
                        survivalpoints = c(rbga.object$best[1:i], rbga.object$mean[1:i]))
    ggTemp = ggplot(dfTemp, aes(x = generation, y = -survivalpoints, group = variable, color = variable)) +
      geom_line() +
      scale_x_continuous(limits = c(0, iter)) +
      scale_y_continuous(limits = c(0, 110)) +
      geom_hline(data = dfTemp, aes(yintercept = max(-survivalpoints)), lty = 2) +
      annotate("text", x = 1, y = max(-dfTemp$survivalpoints) + 2, hjust = 0, size = 3, color = "black",
               label = paste0("Best solution: ", max(-dfTemp$survivalpoints))) +
      theme_bw() +
      ggtitle("Knapsack optimization problem using genetic algorithms")
    print(ggTemp)
  }
}

# curry the function for use in animation
animatePlotSurvival = partial(animatePlot, rbga.object = modelSurvival, iter = 100)

# create and save the animation
ani.options(outdir = file.path(getwd(), "results"))
if(!exists(file.path(getwd(), "results")))dir.create(file.path(getwd(), "results"))
animation::saveGIF(animatePlotSurvival(), img.name = "survivalModelImage", 
                   movie.name = "13-survivalModelMovie.gif", clean = FALSE, interval = 0.05)
