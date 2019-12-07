module Parser where

import Control.Applicative
import Data.ByteString
import Data.ByteString.Internal (c2w)
import Data.Text.Encoding

import qualified Data.Attoparsec.ByteString as P
import Types

parseCommand :: ByteString -> Maybe Command
parseCommand s = case P.parseOnly pCommand s of
  Left _ -> Nothing
  Right c -> Just c
 where

  pCommand =
    (P.word8 colonW8
      *>  (pQuit
          <|> pTakeNick
          <|> pJoin
          <|> pPart
          <|> pNames
          <|> pList
          <|> pPrivMsg)) <* pWhitespace <* P.endOfInput


  pQuit =
    (P.string "QUIT") *> return Quit

  pTakeNick =
    (P.string "NICK") *> pWhitespace1 *> (TakeNick <$> pNick)

  pJoin =
    (P.string "JOIN") *> pWhitespace1 *> (Join <$> pRoom)

  pPart =
    (P.string "PART") *> pWhitespace1 *> (Part <$> pRoom)

  pNames =
    (P.string "NAMES") *> pWhitespace1 *> (Names <$> pRoom)

  pList =
    (P.string "LIST") *> return List

  pPrivMsg = do
    _ <- (P.string "PRIVMSG")
    _ <- pWhitespace1
    tgts <-  pTargets
    _ <- pWhitespace1
    msg <- decodeUtf8 <$>  P.takeWhile1 (not . endOfLine)
    return (PrivMsg tgts msg)

  pTargets = pTarget `P.sepBy` (P.word8 commaW8)

  pTarget = TgtRoom <$> pRoom <|> TgtNick <$> pNick

  pRoom = P.word8 hashW8 *> (Room . decodeUtf8 <$> P.takeWhile1 (/= spaceW8))

  pNick = do
    Nick . decodeUtf8 <$>  (P.takeWhile1 $ (\w -> w /= spaceW8 && w /= hashW8 && w /= tabW8))

  pWhitespace = P.takeWhile (\w -> w == spaceW8 || w == tabW8)
  pWhitespace1 = P.takeWhile1 (\w -> w == spaceW8 || w == tabW8)

  endOfLine w = w == 13 || w == 10

  hashW8 = c2w '#'
  commaW8 = c2w ','
  spaceW8 = c2w ' '
  colonW8 = c2w ':'
  tabW8 = c2w '\t'
