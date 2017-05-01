module Data.Ident where

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as BSC
import Data.ByteString.Builder
import Data.Monoid


data PortPair = PortPair Int Int

data Reply = ReplyError PortPair ErrorType
           | ReplyIdent PortPair OpsysField UserID

data ErrorType = InvalidPort
               | NoUser
               | HiddenUser
               | UnknownError
               | ErrorToken String

data OpsysField = OpsysField Opsys (Maybe Charset)

data Opsys = Other
           | Unix
           | Token String

data Charset = USAscii

data UserID = UserID String

-- | Parse a port pair request
parsePortPair :: BS.ByteString -> Maybe PortPair
parsePortPair req =
  case (fmap fst . BSC.readInt) <$> (BSC.split ',' req) of
    [p1,p2] -> PortPair <$> p1 <*> p2
    _ -> Nothing

-- | Render a reply for a given request
renderReply :: Reply -> ByteString
renderReply (ReplyError portPair errType) =
  BSL.toStrict . toLazyByteString $
       renderPortPair portPair
    <> string7 " : ERROR : "
    <> renderErrorType errType
renderReply (ReplyIdent portPair opsysField userID) =
  BSL.toStrict . toLazyByteString $
       renderPortPair portPair 
    <> string7 " : USERID : "
    <> renderOpsysField opsysField
    <> string7 " : "
    <> renderUserId userID

renderPortPair :: PortPair -> Builder
renderPortPair (PortPair i j) =
     intDec i
  <> string7 ", "
  <> intDec j

renderErrorType :: ErrorType -> Builder
renderErrorType InvalidPort = string7 "INVALID-PORT"
renderErrorType NoUser = string7 "NO-USER"
renderErrorType HiddenUser = string7 "HIDDEN-USER"
renderErrorType UnknownError = string7 "UNKNOWN-ERROR"
renderErrorType (ErrorToken err) =
  string7 err

renderOpsysField :: OpsysField -> Builder
renderOpsysField (OpsysField opsys charset) =
     renderOpsys opsys
  <> renderCharset charset

renderUserId :: UserID -> Builder
renderUserId (UserID userID) = string7 userID

renderOpsys :: Opsys -> Builder
renderOpsys Other = string7 "OTHER"
renderOpsys Unix = string7 "UNIX"
renderOpsys (Token tok) = string7 tok

renderCharset :: Maybe Charset -> Builder
renderCharset Nothing = mempty
renderCharset _ = error "Charset not supported"
