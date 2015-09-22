module Logs where

-- Pattern Matching and Guards
type ErrorCode = Int

type RawLogEntry = (Int, String)

data LogEntry = Error String ErrorCode | Warning String | Info String
    deriving (Show)

-- Pattern matching on function definition
displayLogEntry :: LogEntry -> String
displayLogEntry (Error msg code) = msg ++ show code
displayLogEntry (Warning msg) = msg
displayLogEntry (Info msg) = msg

-- Guards Only
isHighErrorCode :: Int -> Bool
isHighErrorCode code
    | code >= 500 = True
    | code == 409 = True
    | otherwise = False

-- Guards & Pattern matching
isBadError :: LogEntry -> Bool
isBadError (Error _ code)
    | isHighErrorCode code = True
    | otherwise = False
isBadError _ = False

-- Pattern matching case
filterBadErrors :: [LogEntry] -> [LogEntry]
filterBadErrors (x:xs) = case isBadError x of
    True -> x : filterBadErrors xs
    False -> filterBadErrors xs
filterBadErrors [] = []

rawLogs :: [RawLogEntry]
rawLogs = [
        (500, "Server Unreachable"),
        (500, "Unable to connect to db."),
        (500, "Unable to connect to db."),
        (503, "External service unreachable."),
        (400, "Bad Input"),
        (200, "heartbeat"),
        (409, "Malicious Input"),
        (405, "Latency to east region")
    ]

parseLogEntry :: RawLogEntry -> LogEntry
parseLogEntry (code, msg)
    | isHighErrorCode code = Error msg code
    | code >= 400 = Warning msg
    | otherwise = Info msg

parseLogEntries :: [RawLogEntry] -> [LogEntry]
parseLogEntries (x:xs) = parseLogEntry x : parseLogEntries xs
parseLogEntries [] = []

-- parseLogEntries :: [RawLogEntry] -> [LogEntry]
-- parseLogEntries = map parseLogEntry

logs :: [LogEntry]
logs = parseLogEntries rawLogs
