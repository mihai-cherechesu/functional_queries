module Main where

import System.Environment
import System.IO
import Query

main = do
        args <- getArgs
        case args of
                [] -> error "No test specified!"
                [_] -> error "No subtest specified!"
                test:subtest:[] -> do
                        putStrLn $ show $ test_query (read test :: Integer) (read subtest :: Integer)
                _ -> error "Too many args"

-- Test cases
test_query :: Integer -> Integer -> Table

-- Test set 1, validating Show
test_query 1 1 = eval $ Atom movie
test_query 1 2 = eval $ Atom rating
test_query 1 3 = eval $ Atom user_info

-- Test set 2, validating Select
test_query 2 1 = eval $ Select ["movie_id"] $ Atom movie
test_query 2 2 = eval $ Select ["movie_id", "title"] $ Atom movie
test_query 2 3 = eval $ Select ["movie_id", "title", "genres"] $ Atom movie
test_query 2 4 = eval $ Select ["genres", "movie_id", "title"] $ Atom movie
test_query 2 5 = eval $ Select ["rating", "user_id"] $ Atom rating
test_query 2 6 = eval $ Select ["user_id", "rating"] $ Atom rating
test_query 2 7 = eval $ Select ["user_id", "sex", "occupation"] $ Atom user_info
test_query 2 8 = eval $ Select ["user_id", "zone", "age"] $ Atom user_info
test_query 2 9 = eval $ Select ["user_id", "occupation", "sex"] $ Atom user_info
test_query 2 10 = eval $ Select ["zone", "age", "sex", "user_id"] $ Atom user_info

-- Test set 3, validating SelectLimit
test_query 3 1 = eval $ SelectLimit ["movie_id"] 5 $ Atom movie
test_query 3 2 = eval $ SelectLimit ["title", "genres", "movie_id"] 25 $ Atom movie
test_query 3 3 = eval $ SelectLimit ["rating", "user_id"] 15 $ Atom rating
test_query 3 4 = eval $ SelectLimit ["user_id", "sex", "occupation"] 10 $ Atom user_info
test_query 3 5 = eval $ SelectLimit ["zone", "age", "sex", "user_id"] 30 $ Atom user_info 


-- Test set 4, validating Filter
test_query 4 1 = eval $ Filter (Lt "movie_id" 10) $ Atom movie
test_query 4 2 = eval $ Filter (Lt "age" 40) $ Atom user_info
test_query 4 3 = eval $ Filter (Eq "sex" "M") $ Atom user_info
test_query 4 4 = eval $ Filter (Eq "occupation" "writer") $ Atom user_info
test_query 4 5 = eval $ Filter (In "occupation" ["writer", "executive"]) $ Atom user_info
test_query 4 6 = eval $ Filter (In "occupation" ["administrator", "technician"]) $ Atom user_info
test_query 4 7 = eval $ Filter (Not (In "occupation" ["writer", "executive"])) $ Atom user_info
test_query 4 8 = eval $ Filter (Not (In "occupation" ["administrator", "technician"])) $ Atom user_info
test_query 4 9 = eval $ Select ["movie_id"] $ Filter (Not (Lt "rating" 3)) $ Atom rating
test_query 4 10 = eval $ Select ["user_id", "age"] $ Filter (Eq "occupation" "executive") $ Atom user_info

--Test set 5, validating Or/Reunion
test_query 5 1 = eval $ table1 :|| table2
    where
        table1 = Filter (Lt "movie_id" 10) $ Atom movie
        table2 = Filter (Not (Lt "movie_id" 100)) $ Atom movie

test_query 5 2 = eval $ table1 :|| table2
    where
        table1 = Filter (Lt "age" 18) $ Atom user_info
        table2 = Filter (Not (Lt "age" 65)) $ Atom user_info

test_query 5 3 = eval $ table1 :|| table2
    where
        table1 = Filter (Lt "user_id" 25) $ Select ["user_id", "occupation"] $ Atom user_info
        table2 = Filter (Not (Lt "user_id" 45)) $ Select ["user_id", "occupation"] $ Atom user_info

test_query 5 4 = eval $ table1 :|| table2
    where
        table1 = Filter (Lt "user_id" 20) $ Select ["user_id", "occupation"] $ Atom user_info
        table2 = Filter (Not (Lt "user_id" 50)) $ Select ["user_id", "occupation"] $ Atom user_info

test_query 5 5 = eval $ table1 :|| table2
    where
        table1 = Filter (In "occupation" ["writer", "executive"]) $ Atom user_info
        table2 = Filter (In "occupation" ["administrator", "technician"]) $ Atom user_info

-- Test set 6, validation Queries

-- test same_zone query
test_query 6 1 = eval $ same_zone "15"
test_query 6 2 = eval $ same_zone "422"
test_query 6 3 = eval $ same_zone "414"

-- test male_within_age query
test_query 6 4 = eval $ male_within_age 15 25
test_query 6 5 = eval $ male_within_age 20 40

-- test mixed query
test_query 6 6 = eval $ mixed ["15213", "05201", "55106", "30329"] ["other", "scientist", "educator"] 50
test_query 6 7 = eval $ mixed ["20854", "46538", "07102"] ["marketing"] 50
test_query 6 8 = eval $ mixed ["10003", "27510", "93117", "02138"] ["student", "librarian"] 30

-- Test set Bonus, cosine similarity
test_query 7 1 = eval $ Cosine $ Filter (Not (Lt "rating" 3)) $ Atom rating
test_query 7 2 = eval $ Cosine $ Filter (Not (Lt "rating" 4)) $ Atom rating 
test_query 7 3 = eval $ Cosine $ Filter (Lt "user_id" 100) $ Atom rating
test_query 7 4 = eval $ Cosine $ Filter (Lt "user_id" 500) $ Atom rating
