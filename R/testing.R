wd <- getwd()
source(paste0(wd, "R/testing.R"))

test_parents <- generating_parent(chromosome_length = 1000)

test_mouse <- generating_mouse(parent_chromosomes = test_parents, num_recombinations = 5)

print(head(test_mouse))
