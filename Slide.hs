--How to create graph on Excel:
--run "main"
--open csv file with Excel
--select all data points
--click insert
--choose scatter
--select scatter with straight lines
--change parameters in main to experiment



import System.Random
-- PART 1

-- definition of Point
type Point = (Float, Float)

-- function to calculate velocity with two given y values
velocity :: Float -> Float -> Float
velocity y0 yi = (2 * g * (y0 - yi))**0.5     --y0:starting point y1:y where the velocity is calculated for
    where g = 9.81

-- function to find the Euclidian distance of two points
distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = ((x1 - x2)**2 + (y1 - y2)**2)**0.5

-- function to calculate the time taken given two velocities and distance
time :: Float -> Float -> Float -> Float
time v1 v2 d = (d / ((v1 + v2)/2))    --v1:velocity at point1 v2:velocity at point2 d:distance between point1 and point2

-- function to combine start/end points with all supporting points to make other functions easier to loop through all points
all_points :: Point -> Point -> [Point] -> [Point]
all_points x y zs = x:zs ++ [y]

-- function which loops through all points and pairs up all points with the adjacent
make_adjacent_pairs :: [Point] -> [(Point, Point)]
make_adjacent_pairs [] = []
make_adjacent_pairs [_] = []
make_adjacent_pairs (x:xs) = [(x, head(xs))] ++ make_adjacent_pairs xs

-- function which combines make_adjacent_pairs and all_points to take a start/end point and supporting points and output all pairs of points
arrange_in_pairs :: Point -> Point -> [Point] -> [(Point, Point)]
arrange_in_pairs x y zs = make_adjacent_pairs all
    where all = all_points x y zs

-- function which calculates the time to travel between a pair of points
pair_time :: Float -> (Point, Point) -> Float
pair_time y0 ((x1, y1),(x2,y2)) = time v1 v2 d      -- y0:y value of starting point
    where
        d = distance (x1, y1) (x2,y2)
        v1 = velocity y0 y1
        v2 = velocity y0 y2

-- function which uses pair_time to find the total time to travel through all points through looping every pair of points
all_pair_time :: Float -> [(Point, Point)] -> Float
all_pair_time y0 [] = 0
all_pair_time y0 (x:xs) = (pair_time y0 x) + (all_pair_time y0 xs)

-- function which gets a start/end point and supporting points and uses all_pair_time/all_pairs to find total time
total_time :: Point -> Point -> [Point] -> Float
total_time (x0,y0) (x1,y1) zs = all_pair_time y0 all_pairs
    where all_pairs = arrange_in_pairs (x0,y0) (x1,y1) zs





-- PART 2

-- definition of Candidate type
type Candidate = (Point, Point, [Point], Float)

-- function which gets start/end and lists of supporting points getting total time for each case and creating a list of candidates
make_candidates :: Point -> Point -> [[Point]] -> [Candidate]
make_candidates x y [] = []
make_candidates x y (z:zs) = (x, y, z, (total_time x y z)) : (make_candidates x y zs)

-- function to quick sort candidates in order of time
sort_by_time :: [Candidate] -> [Candidate]
sort_by_time [] = []
sort_by_time ((np0, np1, nps, nt):xs) = sort_by_time lt ++ [(np0, np1, nps, nt)] ++ sort_by_time gteq
    where
        lt   = [(xp0, xp1, xps, xt) | (xp0, xp1, xps, xt) <- xs, xt < nt]
        gteq = [(xp0, xp1, xps, xt) | (xp0, xp1, xps, xt) <- xs, xt >= nt]

-- function used to convert candidate to string format for writing to csv file
candidate_to_string :: Candidate -> String
candidate_to_string (x, y, zs, t) = (point_to_string x) ++ "\n" ++ (concat_str zs) ++ (point_to_string y) ++ "\n" ++ "Time: " ++ (show t)

-- helper function of candidate_to_string to convert each point to a suitable string format
point_to_string :: Point -> String
point_to_string (x,y) = (show x) ++ "," ++ (show y)

-- function to loop all supporting points and adding them to a string
concat_str :: [Point] -> String
concat_str [] = ""
concat_str (x:xs) = point_to_string x ++ "\n" ++ concat_str xs

-- function which divides x and y values into supporting points
divide_list_aux :: [Float] -> [Float] -> [Float] -> [[Point]]
divide_list_aux xorg [] [] = []          -- xorg: original xs which it will return to when all x have been used and there are still y values which need to be paired
divide_list_aux xorg xs [] = []
divide_list_aux xorg [x] (y:ys) = [(x,y)]:(divide_list_aux xorg xorg ys)
divide_list_aux xorg (x:xs)[y] = [[(x,y)]]
divide_list_aux xorg (x:xs) (y:ys) = ((x,y):prev0):prevs
    where (prev0:prevs) = divide_list_aux xorg xs ys

-- function to create a list of sets of supporting points from a list of x values and a list of y values
divide_list :: [Float] -> [Float] -> [[Point]]
divide_list xs ys = divide_list_aux xs xs ys






-- PART 3

-- function to create a random list of Float numbers within a range
random_list :: Int -> (Float, Float) -> StdGen -> ([Float], StdGen)
random_list 0 _ gen = ([], gen)
random_list n minmax gen = ((r:rs), g2)
    where
        (r, g) = randomR minmax gen :: (Float,StdGen)   -- specify we expect r to be returned as float
        (rs, g2) = random_list (n-1) minmax g

-- function which creates random candidates
create_random_candidates :: Int -> Point -> Point -> [Float] -> (Float,Float) -> StdGen -> ([Candidate], StdGen)
create_random_candidates number_of_candidates starting ending x_values min_max_y random_generator = (random_candidates, new_generator)
    where (y_values, new_generator) = random_list (number_of_candidates * (length x_values)) min_max_y  random_generator
          random_points = divide_list x_values y_values
          random_candidates = make_candidates starting ending random_points





-- PART 4

-- function which gets a list of candidates, the number of candidates to be crossed over and the random generator and returns the new candidates + new generator
crossover :: [Candidate] -> Int -> StdGen -> ([Candidate], StdGen)
crossover cs n g = (cs ++ cs_new, g1)
    where pairs = [((cs !! c1), (cs !! c2)) | c1 <- [0..(n-1)], c2 <- [(c1+1)..(n-1)]]   -- c1:index of candidate1 and c2:is the index of candidate2, where pairs is a collection of all possible pairs of candidates below index n
          (cs_new, g1) = cross_pairs pairs g

-- function which gets a list of pairs of candidates and returns the list of new candidates made by mixing the given pair randomly
cross_pairs :: [(Candidate,Candidate)] -> StdGen -> ([Candidate], StdGen)
cross_pairs [] g = ([], g)
cross_pairs (cp:cps) g = (c:cs, g2)
    where (c, g1) = cross_pair cp g
          (cs,g2) = cross_pairs cps g1

-- function to make a new candidate by mixing the candidates in the given pair
cross_pair :: (Candidate, Candidate) -> StdGen -> (Candidate,StdGen)
cross_pair ((s, e, ps1, _), (_,_,ps2,_)) g = ((s, e, ps, t), g1)
    where (ps, g1) = cross_supp ps1 ps2 g
          t = total_time s e ps

-- function to make new supporting points by mixing the two sets of given supporting points
cross_supp :: [Point] -> [Point] -> StdGen -> ([Point], StdGen)
cross_supp [] [] g = ([], g)
cross_supp (c1:cs1) (c2:cs2) g = ((if r < 0.5 then c1 else c2) : xs, g2)    -- choose c1 or c2 with even chance where r is a random number between 0 and 1
    where (r,g1) = randomR(0 :: Float, 1 :: Float) g
          (xs, g2) = cross_supp cs1 cs2 g1

-- function which does single_mutation to the top x candidates
mutation :: Int -> Int -> (Float, Float) -> (Float,Float) -> [Candidate] -> StdGen -> ([Candidate], StdGen)
mutation 0 which_to_mutate shift_range min_max_y candidates random_generator = (candidates, random_generator)
mutation number_mutated which_to_mutate shift_range min_max_y (c:candidates) random_generator = ((c:new_candidate:new_candidates), new_new_generator)
    where (new_candidate, new_generator) = (single_mutation which_to_mutate shift_range min_max_y c random_generator)
          (new_candidates, new_new_generator) = mutation (number_mutated-1) which_to_mutate shift_range min_max_y candidates new_generator

-- function which gets a random number to mutate a y coordinate of a single supporting point and returns the new candidate with the modified points
single_mutation :: Int -> (Float, Float) -> (Float,Float) -> Candidate -> StdGen -> (Candidate, StdGen)
single_mutation which_to_mutate (shift_min, shift_max) (min_y, max_y) (start, end, supporting, time) random_generator = ((start, end, new_supporting, new_time), new_generator)
    where (shift, new_generator) = randomR (shift_min, shift_max) random_generator
          shifted_y = (snd (supporting!!which_to_mutate)) + shift
          valid_y = (min max_y (max min_y shifted_y))
          new_supporting = (take which_to_mutate supporting) ++ ((fst (supporting!!which_to_mutate)),valid_y):(drop (which_to_mutate + 1) supporting)
          new_time = total_time start end new_supporting





-- PART 6

-- general function to find the best candidate for given parameters
create_slide :: Point -> Point -> [Float] -> Int -> Int -> Int -> Int -> Int -> (Float, Float) -> Int -> Candidate
create_slide start end x_values num_of_iterations num_of_candidates num_crossbred num_mutated num_positions min_max_y seed = best_candidate
    where gen = (mkStdGen seed)
          (candidates, gen2) = create_random_candidates num_of_candidates start end x_values min_max_y gen
          sorted_candidates = sort_by_time candidates
          (final_candidates, gen3) = create_slide_aux num_of_iterations sorted_candidates num_crossbred num_mutated num_positions min_max_y gen2
          best_candidate = final_candidates!!0

-- this runs the genetic algorithm recursively
create_slide_aux :: Int -> [Candidate] -> Int -> Int -> Int -> (Float,Float) -> StdGen -> ([Candidate], StdGen)
create_slide_aux 0 candidates _ _ _ _ gen = (candidates, gen)
create_slide_aux num_of_iterations sorted_candidates num_crossbred num_mutated num_positions min_max_y gen = create_slide_aux (num_of_iterations-1) chopped_candidates num_crossbred num_mutated num_positions min_max_y new_gen
    where population = length sorted_candidates
          (new_candidates, gen2) = crossover sorted_candidates num_crossbred gen
          (index, gen3) = randomR (0,(num_positions-1)) gen2
          (new_new_candidates, new_gen) = mutation num_mutated num_positions (-40,40) min_max_y new_candidates gen3
          sorted_new_new_candidates = sort_by_time new_new_candidates
          chopped_candidates = take population sorted_new_new_candidates




-- example parameters executed for create_slide
main = do
    writeFile file_name best_csv
    where
          start = (0, 100)
          end = (100, 0)
          x_values = [10, 20, 30, 40, 50, 60, 70, 80, 90]
          num_of_iterations = 50000
          num_of_candidates = 250
          num_crossbred = 50
          num_mutated = 200
          num_positions = (length x_values)-1
          min_max_y = (2, 98)
          seed = 64
          best = create_slide start end x_values num_of_iterations num_of_candidates num_crossbred num_mutated num_positions min_max_y seed
          best_csv = candidate_to_string best
          file_name = "output_iteration" ++ (show num_of_iterations) ++ "_candidates" ++ (show num_of_candidates) ++ ".csv"
