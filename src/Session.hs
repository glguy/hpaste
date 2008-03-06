module Session (SessionId, get_session_id) where

import Network.CGI
import System.Random
import Utils.Misc(maybeRead)

type SessionId = Int

session_cookie_name :: String
session_cookie_name = "sid"

get_session_id :: CGI SessionId
get_session_id =
 do mb_sessionCookie <- getCookie session_cookie_name
    case maybeRead =<< mb_sessionCookie of
      Nothing -> do sid <- liftIO generate_sid
                    setCookie (make_session_cookie sid)
                    return sid
      Just sid -> return sid

generate_sid :: IO Int
generate_sid = randomIO

make_session_cookie :: Int -> Cookie
make_session_cookie sid = Cookie
  { cookieName = session_cookie_name
  , cookieValue = show sid
  , cookieExpires = Nothing
  , cookieDomain = Nothing
  , cookiePath = Nothing
  , cookieSecure = False
  }
