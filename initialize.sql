CREATE TABLE IF NOT EXISTS paste
  ( pasteid INTEGER PRIMARY KEY
  , createstamp DEFAULT CURRENT_TIMESTAMP NOT NULL
  , content TEXT NOT NULL
  , title   TEXT
  , author  TEXT
  , hostname  TEXT
  , ipaddress TEXT
  , expireon INTEGER
  , language TEXT
  , parentid  INTEGER
  ) ;
