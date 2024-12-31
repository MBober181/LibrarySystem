import Text.Read (readMaybe)
import Control.Monad (void)
import Data.List (find)

-- User data type
data User = User { userId :: Int
                 , username :: String
                 , borrowedBooks :: [Int]
                 } deriving Show

-- Book data type definition
data Book = Book { bookId :: Int
                 , title  :: String
                 , author :: String
                 , genre  :: String
                 , availableCopies :: Int
                 , borrowedBy :: Maybe Int
                 }

-- Custom Show instance for the Book type
instance Show Book where
    show book = 
        "ID: " ++ show (bookId book) ++ "\n" ++
        "Title: " ++ title book ++ "\n" ++
        "Author: " ++ author book ++ "\n" ++
        "Genre: " ++ genre book ++ "\n" ++
        "Available Copies: " ++ show (availableCopies book) ++ "\n" ++
        "Borrowed By: " ++ maybe "None" show (borrowedBy book) ++ "\n"
        ++ "-----------------------------------------"

-- User login function
login :: [User] -> IO (Maybe User)
login users = do
    putStrLn "Enter your user ID:"
    userIdStr <- getLine
    case readMaybe userIdStr :: Maybe Int of
        Just inputUserId -> do
            putStrLn "Enter your username:"
            inputUsername <- getLine
            -- Check if the provided userId and username match exactly one user
            let user = find (\a -> userId a == inputUserId && username a == inputUsername) users
            case user of
                Just a -> do
                    putStrLn $ "Welcome, " ++ username a ++ "!"
                    return (Just a)
                Nothing -> do
                    putStrLn "Incorrect user ID or username. Please try again."
                    login users
        Nothing -> do
            putStrLn "Invalid user ID. Please try again."
            login users

-- Main menu to navigate the library
showMenu :: [Book] -> User -> IO ()
showMenu library user = do
    putStrLn $ "Hi, " ++ username user ++ ", what would you like to do?"
    putStrLn "1. Add books"
    putStrLn "2. Remove books"
    putStrLn "3. Search for books"
    putStrLn "4. Borrow a book"
    putStrLn "5. Return a book"
    putStrLn "6. Available / Borrowed books"
    putStrLn "7. Exit"
    choose <- getLine

    case choose of
        "1" -> do
            updatedLibrary <- addBook library  -- Update the library after adding a book
            showMenu updatedLibrary user       -- Pass the updated library to the next iteration of the menu
        "2" -> do
            updatedLibrary <- removeBook library user  -- Update the library after removing a book
            showMenu updatedLibrary user               -- Pass the updated library to the next iteration of the menu
        "3" -> searchBooksMenu library user
        "4" -> borrowBook library user
        "5" -> returnBook library user
        "6" -> showBooks library >> showMenu library user
        "7" -> putStrLn "Thank you and goodbye!"
        _   -> putStrLn "Invalid input. Please try again." >> showMenu library user

-- Display books (available and borrowed)
showBooks :: [Book] -> IO ()
showBooks library = do
    if null library
        then putStrLn "No books available."
        else mapM_ print library

-- Function to borrow a book
borrowBook :: [Book] -> User -> IO ()
borrowBook library user = do
    putStrLn "Enter the book ID to borrow:"
    bookIdStr <- getLine
    case readMaybe bookIdStr :: Maybe Int of
        Just id -> do
            let book = find (\b -> bookId b == id && availableCopies b > 0) library
            case book of
                Just b -> do
                    putStrLn $ "You have borrowed " ++ title b
                    -- Update the book and user data
                    let updatedBook = b { availableCopies = availableCopies b - 1, borrowedBy = Just (userId user) }
                    let updatedLibrary = updatedBook : filter (\b' -> bookId b' /= id) library
                    let updatedUser = user { borrowedBooks = id : borrowedBooks user }
                    showMenu updatedLibrary updatedUser
                Nothing -> putStrLn "Sorry, this book is not available or does not exist."
        Nothing -> putStrLn "Invalid book ID. Please try again."
    showMenu library user  -- return to the main menu after borrowing

-- Function to return a book
returnBook :: [Book] -> User -> IO ()
returnBook library user = do
    putStrLn "Enter the book ID to return:"
    bookIdStr <- getLine
    case readMaybe bookIdStr :: Maybe Int of
        Just id -> do
            let book = find (\b -> bookId b == id && borrowedBy b == Just (userId user)) library
            case book of
                Just b -> do
                    putStrLn $ "You have returned " ++ title b
                    -- Update the book and user data
                    let updatedBook = b { availableCopies = availableCopies b + 1, borrowedBy = Nothing }
                    let updatedLibrary = updatedBook : filter (\b' -> bookId b' /= id) library
                    let updatedUser = user { borrowedBooks = filter (/= id) (borrowedBooks user) }
                    showMenu updatedLibrary updatedUser
                Nothing -> putStrLn "You have not borrowed this book."
        Nothing -> putStrLn "Invalid book ID. Please try again."
    showMenu library user  -- return to the main menu after returning

-- Function to handle the search menu
searchBooksMenu :: [Book] -> User -> IO ()
searchBooksMenu library user = do
    putStrLn "Choose search criteria:"
    putStrLn "1. Search by book ID"
    putStrLn "2. Search by title"
    putStrLn "3. Search by author"
    putStrLn "4. Search by genre"
    putStrLn "5. Search by copies"
    putStrLn "6. Return to main menu"
    criteria <- getLine

    case criteria of
        "1" -> do
            putStrLn "Enter the ID to search for:"
            searchCriteria <- getLine
            case readMaybe searchCriteria :: Maybe Int of
                Just id -> do
                    let searchResults = searchById id library
                    if null searchResults then
                        putStrLn "No books found for that ID"
                    else
                        mapM_ print searchResults
                Nothing -> putStrLn "Invalid Input"
            searchBooksMenu library user
        "2" -> do
            putStrLn "Enter the title to search for:"
            searchCriteria <- getLine
            let searchResults = searchByTitle searchCriteria library
            if null searchResults then
                putStrLn "No books found for that title."
            else
                mapM_ print searchResults
            searchBooksMenu library user
        "3" -> do
            putStrLn "Enter the author to search for:"
            searchCriteria <- getLine
            let searchResults = searchByAuthor searchCriteria library
            if null searchResults then
                putStrLn "No books found for that author."
            else
                mapM_ print searchResults
            searchBooksMenu library user
        "4" -> do
            putStrLn "Enter the genre to search for:"
            searchCriteria <- getLine
            let searchResults = searchByGenre searchCriteria library
            if null searchResults then
                putStrLn "No books found for that genre."
            else
                mapM_ print searchResults
            searchBooksMenu library user
        "5" -> do
            putStrLn "How many copies are you looking for?"
            searchCriteria <- getLine
            case readMaybe searchCriteria :: Maybe Int of
                Just copies -> do
                    let searchResults = searchByCopies copies library
                    if null searchResults then
                        putStrLn "No books found for that criteria."
                    else
                        mapM_ print searchResults
                Nothing -> putStrLn "Invalid Input"
            searchBooksMenu library user
        "6" -> showMenu library user
        _ -> putStrLn "Invalid input. Please try again." >> searchBooksMenu library user

-- Search function for books by ID
searchById :: Int -> [Book] -> [Book]
searchById searchCriteria = filter (\book -> bookId book == searchCriteria)                 

-- Search function for books by title
searchByTitle :: String -> [Book] -> [Book]
searchByTitle searchCriteria = filter (\book -> searchCriteria `elem` words (title book))

-- Search function for books by author
searchByAuthor :: String -> [Book] -> [Book]
searchByAuthor searchCriteria = filter (\book -> searchCriteria `elem` words (author book))

-- Search function for books by genre
searchByGenre :: String -> [Book] -> [Book]
searchByGenre searchCriteria = filter (\book -> searchCriteria `elem` words (genre book))

-- Search function for books by number of copies for bulk purchases
searchByCopies :: Int -> [Book] -> [Book]
searchByCopies searchCriteria = filter (\book -> availableCopies book >= searchCriteria)

-- Function to remove a book
removeBook :: [Book] -> User -> IO [Book]
removeBook library user = do
    putStrLn "Enter the book ID to remove:"
    bookIdStr <- getLine
    case readMaybe bookIdStr :: Maybe Int of
        Just id -> do
            -- Find the book in the library and filter it out if found
            let updatedLibrary = filter (\b -> bookId b /= id) library
            if length updatedLibrary == length library
                then putStrLn "Book not found." >> return library
                else putStrLn "Book removed successfully!" >> return updatedLibrary
        Nothing -> putStrLn "Invalid book ID. Please try again." >> return library
        
-- Function to add a book
addBook :: [Book] -> IO [Book]
addBook library = do
    putStrLn "Enter the book ID:"
    bookIdStr <- getLine
    let bookId = read bookIdStr :: Int
    putStrLn "Enter the title:"
    title <- getLine
    putStrLn "Enter the author:"
    author <- getLine
    putStrLn "Enter the genre:"
    genre <- getLine
    putStrLn "Enter the number of available copies:"
    copiesStr <- getLine
    let copies = read copiesStr :: Int

    let newBook = Book { bookId = bookId, title = title, author = author, genre = genre, availableCopies = copies, borrowedBy = Nothing }
    putStrLn $ "Added the book: " ++ title
    return (newBook : library)  -- Return updated library

-- Main function
main :: IO ()
main = do
    let book1 = Book { bookId = 15464, title = "Big Book", author = "Mr.N", genre = "True Crime", availableCopies = 39, borrowedBy = Nothing }
    let book2 = Book { bookId = 26847, title = "Small Book", author = "Mr.A", genre = "Fiction", availableCopies = 25, borrowedBy = Nothing }
    let book3 = Book { bookId = 36707, title = "This Book", author = "Mr.G", genre = "Non-Fiction", availableCopies = 11, borrowedBy = Nothing }
    let book4 = Book { bookId = 16734, title = "That Book", author = "Mr.H", genre = "True Crime", availableCopies = 34, borrowedBy = Nothing }
    let book5 = Book { bookId = 27457, title = "The Book", author = "Mr.Y", genre = "Fiction", availableCopies = 0, borrowedBy = Nothing }

    let library = [book1, book2, book3, book4, book5]
    let users = [User { userId = 12345, username = "Ross", borrowedBooks = [] },
                 User { userId = 12346, username = "Rachel", borrowedBooks = [] }]

    -- User login
    loggedInUser <- login users

    -- Start main menu
    case loggedInUser of
        Just user -> do
            putStrLn $ "Welcome, " ++ username user ++ " to the library system."
            showMenu library user  
        Nothing -> putStrLn "Exiting."
