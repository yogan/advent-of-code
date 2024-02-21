program main
   use day08
   implicit none

   integer :: sample_grid_size = 5, grid_size = 99
   integer :: sample_tree_count, tree_count, sample_score, score
   integer :: sample_grid(5,5), grid(99,99)
   logical :: sample_visibility(5,5), visibility(99,99)

   sample_grid = read_file("day08.sample", sample_grid_size)
   sample_visibility = calc_visibility(sample_grid, sample_grid_size)
   sample_tree_count = count_visible_trees(sample_visibility)
   print *, "Part 1 (sample): ", sample_tree_count

   grid = read_file("day08.in", grid_size)
   visibility = calc_visibility(grid, grid_size)
   tree_count = count_visible_trees(visibility)
   print *, "Part 1 (input): ", tree_count

   sample_score = find_max_scenic_score(sample_grid, sample_grid_size)
   score = find_max_scenic_score(grid, grid_size)
   print *, "Part 2 (sample): ", sample_score
   print *, "Part 2 (input): ", score

end program main
