data BookInfo = Book {
    bookID      :: Int,
    bookName    :: String,
    bookAuthors :: [String]
} deriving(Show)

data MagazineInfo = Magazine {
    magazineID      :: Int,
    magazineName    :: String,
    magazineAuthors :: [String]
} deriving(Show)

data BookReview = BookReview {
    bookInfo    :: BookInfo,
    customerID  :: Int,
    reviewBody  :: String
} deriving(Show)

myInfo = Book {
    bookID = 69,
    bookName = "Bookie",
    bookAuthors = ["Bookie Book", "John Doe"]
}

myReview = BookReview {
    bookInfo = myInfo,
    customerID = 420,
    reviewBody = "This was a cool book!"
}