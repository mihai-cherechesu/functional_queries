module Query where
import UserInfo
import Rating
import Movie
import Numeric
import Data.List
import Data.Function
import Data.Array

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

-- TODO 1 ---------------------------------------------------------------------
-- Intuition 1 (parse the data and store them in a triple nested list)
mock_parse :: Char -> Char -> String -> [[String]]
mock_parse col_sep ln_sep = foldr op [[[]]] 
    where
        op x ((y:ys):yss)
            | x == col_sep = (([]:(y:ys)):yss)
            | x == ln_sep  = [[]]:((y:ys):yss) 
            | otherwise    = (((x:y):ys):yss)

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table col_sep ln_sep raw_data = format_table (mock_parse col_sep ln_sep raw_data)

-- Now, the first line in the triple nested list is the TableSchema
format_table :: [[String]] -> Table
format_table processed_data = Table (head processed_data) (take (length (drop 1 processed_data) - 1) $ tail processed_data)

user_info_table = (read_table '|' '\n' UserInfo.user_info_str)
rating_table    = (read_table ' ' '\n' Rating.rating_str)
movie_table     = (read_table '|' '\n' Movie.movie_str)



-- TODO 2 ---------------------------------------------------------------------
-- Note: due to the current implementation, the maximum lengths of the columns
-- are not dinamically updated at each query. Thus, some lines will still keep
-- the padding from the maximum length, even when the query returned a table
-- without that line. The solution is to inspect the table the query returned
-- and compute the maximum lengths again and, where required, we trim the
-- additional padding. 

-- Gather the column lengths; a cell from the output matrix m[i][j] represents 
-- the length of column j, on line i (matrix m[i][j] is the return value of 
-- the function). As mentioned above, we cannot use the "length" function 
-- from Prelude, because the lines can have additional padding, thus we would
-- get a wrong length of the column. We calculate the real length of the column
-- by adding (+1) at each step, until we find a ' ' or the String becomes [].
-- On top of that, there is one more corner case - when we have a column like
-- "XXX YYY ZZZ", we don't need to stop at the first space. Hence, we will
-- stop only when we will find 2 consecutive spaces.
col_lens :: Table -> [[Int]]
col_lens (Table schema entries) = (map length_col schema) : (map (map (length_col)) entries)

length_col :: [Char] -> Int
length_col [] = 0
length_col (x:[])
    | x == ' ' = 0
length_col (x:xs)
    | x == ' ' && (head xs) == ' ' = 0
    | otherwise = 1 + (length_col xs)

-- Takes the maximum element on each index from two rows and returns the row
-- built with those elements
max_el :: [Int] -> [Int] -> [Int]
max_el [] _          = []
max_el _ []          = []
max_el (x:xs) (y:ys) = (max x y) : (max_el xs ys)

-- Builds a row that has on each position the maximum length from a column
max_col_lens :: [[Int]] -> [Int]
max_col_lens col_lens = foldr get_max [] col_lens 
    where
        get_max row acc
            | acc == [] = row
            | otherwise = (max_el row acc)

-- Repeats a character n times
-- Returns a string of n such characters
repeat_chr :: Int -> Char -> String
repeat_chr 0 _ = ""
repeat_chr n c = [c] ++ (repeat_chr (n - 1) c)

-- Pads a single unit of the table with the appropriate number of whitespaces
pad_cell :: Int -> String -> String
pad_cell n s = inspect_diff (n - (length s)) s

-- Takes action on the current column cell based on the difference between
-- the maximum (real) length of the cell (i.e. if we have "abcd___", the
-- length is 4, because the 3 trailing whitespaces are not considered) and
-- the current length ("def________" will have a current length of 11).
-- If the difference is 0, we can trivially say that the column is the longest
-- so we just return it.
-- If the difference is < 0, then, as we saw above, we need to trim the second
-- string to "def_", with only one space left (we cut curr_len - max_len spaces)
-- otherwise, we just add the required padding

-------------
-- Example:--
-------------
-- "def___" -> 6
-- "abcd"   -> 4
-- so we take (6 - diff) where diff = (6 - 4)

inspect_diff :: Int -> String -> String
inspect_diff diff s
    | diff == 0 = s
    | diff < 0  = take ((length s) + diff) s
    | otherwise = s ++ (repeat_chr diff ' ')

-- Adds the padding to each cell of the table
-- Includes both TableSchema and Entries
pad_table :: Table -> [Int] -> Table
pad_table (Table schema entries) max_lens = Table (zipWith (pad_cell) max_lens schema) (map (zipWith (pad_cell) max_lens) entries)

-- Adds the separators to each cell of the table
-- Includes both TableSchema and Entries
-- Convention 1: on input '~', the program should not
-- attach the '|' separator, because the function call
-- was made when we received a table from a query, hence
-- it has already padding and column separator, but lacks the line separator
-- Convention 2: on input '^', the function should not start
-- to accumulate the values with the "|\n" as a starting point, because
-- the lines in this case already have this line separator attached.
add_seps :: Char -> [String] -> [String]
add_seps '^' s = foldr (:) [] s
add_seps '~' s = foldr (:) ["|\n"] s
add_seps sep s = foldr ((:) . ((:) sep)) ["|\n"] s

add_seps_table :: Char -> Table -> Table
add_seps_table sep (Table schema entries) = Table (add_seps sep schema) (map (add_seps sep) entries)

-- Gets the length of a row - all lengths are the sam, hence
-- it does not matter which one is selected first
row_len :: [Entry] -> Int
row_len entries = length $ (foldr (++) [] (head entries))

-- Table with padding and separators, but not in a String format
final_table :: Char -> Table -> Table
final_table sep table = add_seps_table sep $ pad_table table $ max_col_lens $ col_lens $ table

user_info = final_table '|' user_info_table
rating    = final_table '|' rating_table
movie     = final_table '|' movie_table 

-- Enroll the Table data type in the Show type class
instance Show Table where
    show (Table header entries) = (repeat_chr ((row_len entries) - 1) '-') ++ "\n" ++ (foldr (++) [] header) ++ 
                                  (repeat_chr ((row_len entries) - 1) '-') ++ "\n" ++
                                  (foldr (++) [] (map (foldr (++) []) entries)) ++
                                  (repeat_chr ((row_len entries) - 1) '-') ++ "\n"



-- TODO 3 ---------------------------------------------------------------------
data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

-- Helper function that gets the Field value from the specified column
get_field :: Field -> Entry -> TableSchema -> Entry
get_field f e schema = filter (not_empty) $ zipWith (\x y -> if (equals x f) then y else []) schema e 
    where
        not_empty = \x -> x /= ""

-- Helper function: converts a string into an Integer
str_to_int :: String -> Integer
str_to_int s = read s :: Integer

-- Helper function: verifies the membership of an Integer to a defined interval
is_in :: Field -> [String] -> Bool
is_in _ []     = False
is_in f (x:xs) = (equals f x) || is_in f xs

getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Lt field x) schema = lt_func 
                            where
                                lt_func entry
                                    | (str_to_int $ tail $ head $ get_field field entry schema) < x = True
                                    | otherwise = False

getFilter (Eq field s) schema = eq_func 
                            where
                                eq_func entry
                                    | equals (head $ get_field field entry schema) s == True = True
                                    | otherwise = False

getFilter (In field i) schema = in_func 
                            where
                                in_func entry
                                    | is_in (head $ get_field field entry schema) i == True = True
                                    | otherwise = False

getFilter (Not cond) schema = not . getFilter cond schema
    

-- TODO 4 ---------------------------------------------------------------------
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Int Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

-- Returns the index of the desired column from the TableSchema
get_col_no :: Table -> Column -> Int
get_col_no (Table schema entries) col
    | (equals (drop 1 $ head schema) col) == True = 1
    | otherwise = 1 + (get_col_no (Table (tail schema) entries) col)

-- Returns True if the second argument is a prefix of the first arguement
-- e.g. equals("user_id   \n", "user_id") returns True
-- and  equals("user_", "user_id") return False
equals :: Column -> Column -> Bool
equals [] _ = True
equals (x:xs) []
    | (x == ' ') = True
    | otherwise  = False
equals (x:xs) (y:ys)
    | (x == '|' && y == '|') = (x == y) && (equals xs ys)
    | (x == '|') = (equals xs (y:ys))
    | otherwise  = (x == y) && (equals xs ys)

-- Returns the transposed version of a 2d-array
transp :: [[a]] -> [[a]]
transp [] = repeat []
transp (m:ms) = zipWith (:) m (transp ms)

-- Getters for Table sections
get_entries :: Table -> [Entry]
get_entries (Table schema entries) = entries

get_schema :: Table -> TableSchema
get_schema (Table schema entries) = schema

cosine_schema :: TableSchema
cosine_schema = ["user_id1", "user_id2", "sim"]

eval :: Query -> Table
eval (Atom table) = table
eval (Select cols query) = select_columns $ eval query 
    where
        select_columns (Table schema entries) = final_table '~'
                                (Table (map (\x -> head $ (drop (x - 1) (take x schema)))
                                       (map (get_col_no $ (Table schema entries)) cols))
                            (transp $ map (\x -> head $ (drop (x - 1) (take x (transp entries))))
                                     (map (get_col_no $ (Table schema entries)) cols)))

eval (SelectLimit cols x query) = limit_entries $ eval $ Select cols query 
    where
        limit_entries (Table schema entries) = final_table '^' (Table schema (take x entries))

eval (Filter cond query) = apply_filter $ eval query 
    where
        apply_filter (Table schema entries) = final_table '^' (Table schema (filter (getFilter cond schema) entries))

eval (query1 :|| query2) = join (final_table '^' (eval query1))
                                (final_table '^' (eval query2)) 
    where
        join (Table s1 e1) (Table s2 e2) = final_table '^' (Table s1 (e1 ++ e2))

eval (Cosine query) = sort_id_num $ eval query
    where
        sort_id_num (Table schema entries) = sort_id_lex $ final_table '|' 
            (Table cosine_schema (map_cosine (Table schema (sortNumeric entries)) 1 2))
            where
                sort_id_lex (Table s e) = (Table s (sortLex e))
        

-- TODO 5 ---------------------------------------------------------------------
-- Helper function: returns the mono-entry table with the user with id user_id
-- given as parameter
zone_table :: String -> Table
zone_table user_id = eval $ Filter (Eq "user_id" user_id) $ Atom user_info

same_zone :: String -> Query
same_zone user_id = Select ["user_id", "occupation"] $ Filter (Eq "zone" zone)
                                                     $ Filter (Not (Eq "user_id" user_id))
                                                     $ Atom user_info 
    where
        zone = head $ get_field "zone" (head $ get_entries $ zone_table user_id) 
                                               (get_schema $ zone_table user_id)


male_within_age :: Integer -> Integer -> Query
male_within_age lo hi = Select ["occupation", "zone"] $ Filter (Lt "age" hi) 
                                                      $ Filter (Not (Lt "age" (lo + 1)))
                                                      $ Filter (Eq "sex" "M")
                                                      $ Atom user_info

mixed :: [String] -> [String] -> Integer -> Query
mixed zns occs x = Select ["user_id"] $ Filter (In "zone" zns) 
                                      $ Filter (In "occupation" occs)
                                      $ Filter (Lt "age" x)
                                      $ Atom user_info
-------------------------------------------------------------------------------

-- Helper function: sorts the elements by the first field (user_id) in ascending order
-- and the second function by the second field (for testing)
sortNumeric :: [Entry] -> [Entry]
sortNumeric = sortBy (compare `on` (str_to_int . drop 1 . head))
sortNumeric2 = sortBy (compare `on` (str_to_int . drop 1 . head . tail))
sortLex = sortBy (compare `on` (drop 1 . head))

sorted_rating :: Table -> Table
sorted_rating t = sort_rating t
    where 
        sort_rating (Table schema entries) = (Table schema (sortNumeric entries))

sorted_rating2 :: Table -> Table
sorted_rating2 t = sort_rating2 t
    where 
        sort_rating2 (Table schema entries) = (Table schema (sortNumeric2 entries))

-- Total number of users, based on analysis (could be obtained by getting the first field
-- of the last line in the sorted table with sortNumeric) retrieval time ~ 2.5s
num_users :: Table -> Integer
num_users (Table s e) = str_to_int $ drop 1 $ head $ last $ e

-- Total number of movies, based on analysis (could be obtained by getting the second field
-- of the last line in the sorted table with sortNumeric2) retrieval time ~ 7s
num_movies :: Table -> Integer
num_movies t = str_to_int $ drop 1 $ head $ tail $ last $ get_entries (sorted_rating2 t)

str_to_double :: String -> Double
str_to_double s = read s :: Double

type NestedRArray = Array Integer Double
type RArray = Array Integer NestedRArray

-- Stores the ratings for each of the $num_movies$ movies, for each user of the $num_users$ users
-- The total amount of memory would be: 1682 * 943 * 4 bytes -> 6,3 MB -> O(num_users * num_movies) space
-- The vectors will be sparsely populated
u t = array (1, num_movies t) [(i, fromIntegral 0) | i <- [1..num_movies t]]
ratings :: Table -> RArray
ratings t = array (1, num_users t) [(i, u t) | i <- [1..num_users t]]

-- Testing -----------------------------------------------------------
-- Mock arrays for testing
mock_u = array (1, 5) [(i, fromIntegral 0) | i <- [1..5]]
mock_ratings :: RArray
mock_ratings = array (1, 5) [(i, mock_u) | i <- [1..5]]

-- Example from homework statement
mock_u1 = (mock_u // [(1, 3)]) // [(2, 3)]
mock_u2 = (((mock_u // [(1, 1)]) // [(2, 1)]) // [(3, 2)]) // [(5, 1)]
mock_u3 = (((mock_u // [(1, 1)]) // [(2, 0)]) // [(3, 0)]) // [(5, 3)]
mock_ratings1 = ((mock_ratings // [(1, mock_u1)]) // [(2, mock_u2)]) // [(3, mock_u3)] 
----------------------------------------------------------------------

-- Getters for fields
get_user_id :: Entry -> Integer
get_user_id entry = str_to_int $ drop 1 $ head entry

get_movie_id :: Entry -> Integer
get_movie_id entry = str_to_int $ drop 1 $ head $ tail entry

get_rating :: Entry -> Double
get_rating entry = str_to_double $ drop 1 $ head $ tail $ tail entry

-- We used the Data.Array API to store the data properly
-- (//) -> update cell
-- (!)  -> get value from cell
-- We get the array of ratings that corresponds to a user (ratings_arr ! uid)
-- and we update it at index movie_id with rating (.. // [(movie_id, rating)])
fill_ratings :: [Entry] -> RArray -> Table -> RArray
fill_ratings [] ratings_arr t = ratings_arr
fill_ratings (x:xs) ratings_arr t = fill_ratings xs (ratings_arr // 
            [((get_user_id x), (ratings_arr ! (get_user_id x)) // 
            [(get_movie_id x, get_rating x)])]) t

-- Get bounds of an Array
get_bounds :: Array Integer (a) -> [Integer]
get_bounds arr = [(fst $ bounds arr)..(snd $ bounds arr)]

-- Computes the norm of a sim vector
norm :: NestedRArray -> Double
norm = sqrt . sum . square

-- Returns the double as a string with 4 decimals point precision
double_to_str :: Double -> String
double_to_str d = showFFloat (Just 4) d ""  

-- Squares each element of the array
square :: NestedRArray -> NestedRArray
square pref_arr = accum (*) pref_arr [(i, pref_arr ! i) | i <- (get_bounds pref_arr)]

-- Computes the dot product of 2 arrays
-- dotp :: NestedRArray -> NestedRArray -> Double
dotp xs ys = foldr (+) 0 (zipWithA (*) xs ys)

-- <zipWith> version for Arrays
zipWithA f xs ys = listArray (bounds xs) [f (xs ! i) (ys ! i) | i <- range (bounds xs)]

-- Cache that stores each norm for all users
norms_arr :: RArray -> Table -> NestedRArray
norms_arr ratings_arr t = array (1, num_users t) [(i, norm $ ratings_arr ! i) | i <- (get_bounds ratings_arr)]

-- Input: receives 2 indexes for 2 distinct users
-- Computes the cosine similarity between 2 sim vectors
compute_cosine :: Integer -> Integer -> RArray -> Table -> String
compute_cosine ix_xs ix_ys r_arr t = double_to_str $ (dotp (r_arr ! ix_xs) (r_arr ! ix_ys)) /
                                                     ((norms ! ix_xs) * 
                                                      (norms ! ix_ys))
                                                      where
                                                          norms = norms_arr r_arr t

-- Compute the cosine similarity between any 2 users and store
-- them into the entries list
map_cosine :: Table -> Integer -> Integer -> [Entry]
map_cosine t i j
    | i == n    = []
    | j == n    = ((show i) : ((show j) : ((compute_cosine i j r t) : []))) : (map_cosine t (i + 1) (i + 2))
    | otherwise = ((show i) : ((show j) : ((compute_cosine i j r t) : []))) : (map_cosine t i (j + 1))
    where 
        n = num_users t
        r = fill_ratings entries ratings_arr t
            where
                ratings_arr = ratings t
                entries = get_entries t


test_table = eval $ Filter (Lt "user_id" 100) $ Atom rating
