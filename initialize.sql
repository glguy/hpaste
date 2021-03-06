CREATE TABLE IF NOT EXISTS paste
  ( pasteid INTEGER PRIMARY KEY
  , createstamp DEFAULT CURRENT_TIMESTAMP NOT NULL
  , content TEXT NOT NULL
  , title   TEXT NOT NULL
  , author  TEXT NOT NULL
  , hostname  TEXT
  , ipaddress TEXT NOT NULL
  , expireon INTEGER
  , language TEXT NOT NULL
  , channel TEXT  NOT NULL
  , parentid  INTEGER
  ) ;
CREATE TABLE IF NOT EXISTS channel
  ( channelid INTEGER PRIMARY KEY
  , channelname TEXT NOT NULL UNIQUE
  );
CREATE TABLE IF NOT EXISTS annotation
  ( pasteid INTEGER NOT NULL
  , line INTEGER NOT NULL
  , PRIMARY KEY (pasteid, line)
  );
CREATE TABLE IF NOT EXISTS user
  ( userid INTEGER NOT NULL PRIMARY KEY
  , username TEXT NOT NULL UNIQUE
  , userpassword TEXT NOT NULL
  , ircmask TEXT
  , admin INTEGER NOT NULL DEFAULT 0
  );
