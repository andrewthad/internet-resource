{-# language OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Data.Word (Word8)
import Data.Bytes.Types (Bytes(..))

import qualified Data.List as L
import qualified Data.Char
import qualified Internet.Host as H
import qualified GHC.Exts as Exts

-- TODO: Currently, there are not any tests to confirm
-- that things near or above the length limit of 255 work
-- correctly.

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ testGroup "Hostname"
    [ testGroup "lenientDecodeString"
      [ testCase "A" $
          H.lenientDecodeString "a.bar" @?=
          H.lenientDecodeString "a..bar"
      , testCase "B" $
          H.lenientDecodeString "b.bar" @?=
          H.lenientDecodeString "b.-bar"
      , testCase "C" $
          H.lenientDecodeString "c.bar" @?=
          H.lenientDecodeString "c-.bar"
      , testCase "D" $
          H.lenientDecodeString "d.bar" @?=
          H.lenientDecodeString "d-.-.-bar"
      , testCase "E" $
          H.lenientDecodeString "e.bar." @?=
          H.lenientDecodeString "e.bar.."
      , testCase "F" $
          H.lenientDecodeString "f.bar" @?=
          H.lenientDecodeString "..f.bar"
      , testCase "G" $
          H.lenientDecodeString ("g.." ++ take 65 infChars ++ "..org") @?=
          H.lenientDecodeString ("g." ++ take 63 infChars ++ ".org")
      , testCase "H" $
          H.lenientDecodeString "h.bar--" @?=
          H.lenientDecodeString "h.bar"
      , testCase "I" $
          H.lenientDecodeString ("i.." ++ take 62 infChars ++ "----.org") @?=
          H.lenientDecodeString ("i." ++ take 62 infChars ++ ".org")
      , testCase "J" $
          H.lenientDecodeString ".." @?= H.root
      , testCase "K" $
          H.lenientDecodeString "-." @?= H.root
      , testCase "L" $
          H.lenientDecodeString "" @?= H.root
      , testCase "M" $
          H.lenientDecodeString ("m..----" ++ take 62 infChars ++ "-.org") @?=
          H.lenientDecodeString ("m." ++ take 62 infChars ++ ".org")
      , testCase "N" $
          H.lenientDecodeString ("n..----" ++ take 62 infChars ++ "---") @?=
          H.lenientDecodeString ("n." ++ take 62 infChars)
      , testCase "O" $
          H.lenientDecodeString "o.EXAMPLE.COM" @?=
          H.lenientDecodeString "o.example.com"
      ]
    , testGroup "decodeUtf8"
      [ testCase "A" $ H.decodeUtf8 (str "a.bar") @?= Just "a.bar"
      , testCase "B" $ H.decodeUtf8 (str "b.-bar") @?= Nothing
      , testCase "C" $ H.decodeUtf8 (str "c-.bar") @?= Nothing
      , testCase "D" $ H.decodeUtf8 (str "d-.-.-bar") @?= Nothing
      , testCase "E" $ H.decodeUtf8 (str "e.bar..") @?= Nothing
      , testCase "F" $ H.decodeUtf8 (str "..f.bar") @?= Nothing
      , testCase "G" $
          H.decodeUtf8 (str ("g.." ++ take 65 infChars ++ "..org"))
          @?=
          Nothing
      , testCase "H" $
          H.decodeUtf8 (str ("h." ++ take 63 infChars ++ ".org"))
          @?=
          Just (H.lenientDecodeString ("h." ++ take 63 infChars ++ ".org"))
      , testCase "I" $ H.decodeUtf8 (str "i.bar--") @?= Nothing
      , testCase "J" $
          H.decodeUtf8 (str ("j." ++ take 62 infChars ++ "----.org"))
          @?=
          Nothing
      , testCase "K" $
          H.decodeUtf8 (str ".") @?= Just H.root
      , testCase "L" $
          H.decodeUtf8 (str "") @?= Just H.root
      , testCase "M" $
          H.decodeUtf8 (str "o.EXAMPLE.COM") @?= Just "o.example.com"
      , testCase "N" $
          H.decodeUtf8 (str "192.0.2.29") @?= Just "192.0.2.29"
      ]
    ]
  ]

infChars :: [Char]
infChars = L.cycle "abcdefg"

str :: String -> Bytes
str x =
  let Bytes arr 0 len = Exts.fromList (map c2w ("abc" ++ x ++ "xyz"))
   in Bytes arr 3 (len - 6)

c2w :: Char -> Word8
c2w = fromIntegral . Data.Char.ord

