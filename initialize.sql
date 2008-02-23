CREATE TABLE IF NOT EXISTS paste
  ( pasteid INTEGER PRIMARY KEY
  , createstamp DEFAULT CURRENT_TIMESTAMP NOT NULL
  , content TEXT NOT NULL
  , title   TEXT NOT NULL
  , author  TEXT NOT NULL
  , hostname  TEXT
  , ipaddress TEXT
  , expireon INTEGER
  , language TEXT NOT NULL
  , channel TEXT  NOT NULL
  , parentid  INTEGER
  ) ;
CREATE TABLE IF NOT EXISTS channel
  ( channelid INTEGER PRIMARY KEY
  , channelname TEXT NOT NULL UNIQUE
  );
