type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry] deriving Show
type ColSeparator = Char
type LnSeparator = Char

-- TODO 1 ---------------------------------------------------------------------
-- Intuition 1 (parse the data and store them in a triple nested list)
mock_parse :: Char -> Char -> String -> [[String]]
mock_parse col_sep ln_sep = foldr op [[[]]] where
    op x ((y:ys):yss)
        | x == col_sep = (([]:(y:ys)):yss)
        | x == ln_sep = [[]]:((y:ys):yss) 
        | otherwise = (((x:y):ys):yss)

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table col_sep ln_sep raw_data = format_table (mock_parse col_sep ln_sep raw_data)

-- Now, the first line in the triple nested list is the TableSchema
format_table :: [[String]] -> Table
format_table processed_data = Table (head processed_data) (tail processed_data)


-- TODO 2 ---------------------------------------------------------------------
instance Show Table where
    show (Table header entries) = undefined

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition


-- TODO 3 ---------------------------------------------------------------------
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter = undefined


-- TODO 4 ---------------------------------------------------------------------
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval = undefined


-- TODO 5 ---------------------------------------------------------------------
same_zone :: String -> Query
same_zone = undefined

male_within_age :: Integer -> Integer -> Query
male_within_age = undefined

mixed :: [String] -> [String] -> Integer -> Query
mixed = undefined
