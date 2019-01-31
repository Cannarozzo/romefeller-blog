import           Data.Time           (Day, utctDay, toGregorian, getCurrentTime, showGregorian)

getdataFormatada :: IO String
getdataFormatada = do
    now <- getCurrentTime
    let today = utctDay now
    return $ showGregorian today
   