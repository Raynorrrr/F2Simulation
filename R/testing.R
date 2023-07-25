wd <- getwd()

test_parents <- generating_parent(chromosome_length = 1000)

test_mouse <- generating_mouse(parent_chromosomes = test_parents, num_recombinations = 5)

test_generation_f1 <- generating_generation(parent_chromosomes = test_parents, 
  num_recombinations = 3, num_mice = 100, generation = "F1")

test_generation_f2 <- generating_generation(parent_chromosomes = test_generation_f1,
  num_recombinations = 3, num_mice = 100, generation = "F2")

print(head(test_mouse))