{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Onion.Address
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testValid, testInvalid]

testValid :: TestTree
testValid =
  testCase "Valid Addresses" $
    assertBool "Valid Addresses" $
      all (valid . validateAddress) validAddresses

testInvalid :: TestTree
testInvalid =
  testCase "Invalid Addresses" $
    assertBool "Invalid Addresses" $
      all (invalid . validateAddress) invalidAddresses

validAddresses :: [ByteString]
validAddresses =
  [ "pg6mmjiyjmcrsslvykfwnntlaru7p5svn6y2ymmju6nubxndf4pscryd.onion"
  , "sp3k262uwy4r2k3ycr5awluarykdpag6a7y33jxop4cs2lu5uz5sseqd"
  , "http://xa4r2iadxm55fbnqgwwi5mymqdcofiu3w6rpbtqn7b2dyn7mgwj64jyd.onion/"
  ]

invalidAddresses :: [ByteString]
invalidAddresses =
  [ "pg6mmjiyjmcrsslvykfwnatlaru7p5svn6y2ymmju6nubxndf4pscryd.onion"
  , "sp3k262uwy4r2k3ycr5awfuarykdpag6a7y33jxop4cs2lu5uz5sseqd"
  , "http://xa4r2iadxm55fbnqgwwi5dymqdcofiu3w6rpbtqn7b2dyn7mgwj64jyd.onion/"
  ]