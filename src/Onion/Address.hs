{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Onion.Address (
  -- Validating Addresses
  validateAddress,
  Validity (..),
  valid,
  invalid,
  ByteString,

  -- * Parsing and Validating Manually
  OnionAddress,
  parseUriAsOnionAddress,
  validateOnionAddress,
) where

import Control.Monad
import Crypto.Hash
import Data.Bifunctor
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString.Base32
import Data.ByteString.Char8 qualified as BS
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import URI.ByteString

-- | Represents the validity of an onion address.
data Validity err = Invalid err | Valid
  deriving (Eq, Show)

-- | Are we 'Valid'?
valid :: Validity err -> Bool
valid Valid = True
valid (Invalid _) = False

-- | Are we 'Invalid'?
invalid :: Validity err -> Bool
invalid = not . valid

data OnionAddressComponents = OnionAddressComponents
  { version :: ByteString
  , publicKey :: ByteString
  , checksum :: ByteString
  }
  deriving (Show, Eq)

-- | Represents an unvalidated onion address.
newtype OnionAddress = OnionAddress
  { onionAddressBytes :: ByteString
  }
  deriving (Eq, Show)
  deriving newtype (IsString, BA.ByteArrayAccess)

-- | Validate an onion address as a 'ByteString'.
validateAddress :: ByteString -> Validity Text
validateAddress bytes = case parseUriAsOnionAddress bytes of
  Left msg -> Invalid $ "error: " <> msg
  Right addr -> validateOnionAddress addr

-- | Validate a parsed onion address.
validateOnionAddress :: OnionAddress -> Validity Text
validateOnionAddress addr = either Invalid (const Valid) $ do
  let normalized = normalizeAddress addr
  components <- parseOnionAddressComponents normalized
  if calculateCheckSum components == checksum components
    then pure ()
    else Left "error: checksum mismatch"

normalizeAddress :: OnionAddress -> OnionAddress
normalizeAddress (OnionAddress addr) = OnionAddress normalized
 where
  normalized
    | BS.takeEnd 6 addr == ".onion" = BS.dropEnd 6 addr
    | otherwise = addr

sha3 :: ByteString -> ByteString
sha3 = BA.convert . hash @_ @SHA3_256

calculateCheckSum :: OnionAddressComponents -> ByteString
calculateCheckSum OnionAddressComponents{..} = BS.take 2 . sha3 $ ".onion checksum" <> publicKey <> version

-- | Parse a URI as an onion address.
parseUriAsOnionAddress :: ByteString -> Either Text OnionAddress
parseUriAsOnionAddress bytes =
  if ':' `BS.elem` bytes
    then do
      parsed <- first (T.pack . show) $ parseURI laxURIParserOptions bytes
      auth <- maybe (Left "Error: No authority in URI") Right $ uriAuthority parsed
      pure . OnionAddress . hostBS . authorityHost $ auth
    else Right $ OnionAddress bytes

parseOnionAddressComponents :: OnionAddress -> Either Text OnionAddressComponents
parseOnionAddressComponents (OnionAddress addr) = do
  decoded <- decodeBase32 addr
  let (pubkey, rest) = BS.splitAt 32 decoded
      (checkSum, version) = BS.splitAt 2 rest

  when (BS.length version /= 1) $
    Left "Invalid checksum length"

  when (BS.length pubkey /= 32) $
    Left "Invalid public key length"

  when (BS.length checkSum /= 2) $
    Left "Invalid checksum length"

  pure $ OnionAddressComponents version pubkey checkSum
