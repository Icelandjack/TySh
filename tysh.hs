-- TDA452 Functional programming, HT 2012
-- Lab 4

-- Baldur Blöndal
-- 890929-5613 (890929-T356)
-- baldurpet@gmail.com

-- John J. Camilleri
-- 860819-7318
-- johncam@student.chalmers.se

-- TySh: a typed shell

module TySh where

-- type FilePath = String

type Permissions = (Int,Int,Int)
  
data MimeType = TXT | HTML | PDF | Other

data FileDetails = FileDetails {
  name :: String ,
  path :: FilePath ,
  permissions :: Permissions ,
  size :: Integer ,
  mimetype :: MimeType
  }

u = undefined

ls :: FilePath -> [FileDetails]
ls = u

pwd :: FilePath
pwd = u
