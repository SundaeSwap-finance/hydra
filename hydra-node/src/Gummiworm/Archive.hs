{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeApplications #-}
module Gummiworm.Archive where

{- TODO: not yet clear where this code should live -}

import Hydra.Prelude
import qualified Data.ByteString as BS
import Codec.Serialise.Class (Serialise(encode, decode))
import Codec.Serialise.Encoding (encodeListLen, encodeListLenIndef, encodeBreak)
import Codec.Serialise.Decoding (decodeListLen, peekTokenType, TokenType (..), Decoder, decodeListLenOrIndef, decodeBreakOr)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time (secondsToNominalDiffTime, nominalDiffTimeToSeconds)
import Hydra.Cardano.Api (Tx, SerialiseAsCBOR (deserialiseFromCBOR), HasTypeProxy (proxyToAsType))
import Codec.Serialise (deserialiseOrFail)
import Codec.Compression.GZip (decompress, compress)
import Hydra.Persistence (PersistenceException(PersistenceException))

-- since there are only a few of these its easier to check the size in the Serialise instance
-- but if we get a lot of these we can define a type alias to BS w KnownNat len or something
-- not effective for our usage tho !
newtype GummiwormVersion = GummiwormVersion BS.ByteString -- Size 2 (bytes)
  deriving stock (Show, Eq)

newtype GummiwormId = GummiwormId BS.ByteString -- Size 28 (bytes)
  deriving stock (Show, Eq)

newtype PubKey = PubKey BS.ByteString -- Size 32 (bytes)
  deriving stock (Show, Eq)
newtype PubKeyHash = PubKeyHash BS.ByteString -- Size 28 (bytes)
  deriving stock (Show, Eq)
newtype Signature = Signature BS.ByteString  -- Size 64 (bytes)
  deriving stock (Show, Eq)

newtype FileHash = FileHash BS.ByteString  -- Size 32 (bytes)
  deriving stock (Show, Eq)

newtype CardanoTx = CardanoTx BS.ByteString -- OG rust code doesnt specify len 
  deriving stock (Show, Eq)

--TODO: unpack any types thats not indefinite list


-- should equivalent to the rust types
data ArchiveMetadata = ArchiveMetadata
  { gummiwormArchiveVersion :: GummiwormVersion
  , timeStamp :: POSIXTime -- underlying type is Word64 / uint64, representing time since unix epoch in seconds https://github.com/SundaeSwap-finance/gumdrop/blob/8f4aeca7ff74f0e87e6bae7704e0819af9447c7d/gummiworm-archive/src/lib.rs#L48
  , gummiwormId :: GummiwormId
  , gummiwormOperatorList :: [PubKeyHash]
  , txIndexRangeStart :: Word64
  , txIndexRangeEnd :: Word64
  } deriving (Show, Eq, Generic)

data ArchiveTransactionIdx = ArchiveTransactionIdx
  { gummiwormArchiveVersion :: GummiwormVersion
  , transactionIndices :: [UserIdxEntry]
  } deriving (Show, Eq, Generic)

data ArchiveTransactions = ArchiveTransactions
  { gummiwormArchiveVersion :: GummiwormVersion
  , transactions :: [CardanoTx]
  } deriving (Show, Eq, Generic)

data ArchiveWitnesses = ArchiveWitnesses
  { gummiwormArchiveVersion :: GummiwormVersion
  , metadataFileHash :: FileHash
  , txFileHash :: FileHash
  , txGziFileHash :: FileHash
  , txIdxFileHash :: FileHash
  , witnesses :: [Witness]
  } deriving (Show, Eq, Generic)

data Witness = Witness
  { pubKey :: PubKey
  , signature :: Signature
  } deriving (Show, Eq, Generic)

data UserIdxEntry = UserIdxEntry
  { key :: Text
  , compPos :: Word64
  , uncompPos :: Word64
  , numBytes :: Word64
  } deriving (Show, Eq, Generic)
-- manually writing these instances to make sure they correspond to the Rust code

decodeOrFail :: TokenType -> String -> Decoder s a -> Decoder s a
decodeOrFail expectedType errMsg decoder = do
  peekTokenType >>= \actualType ->
    if actualType == expectedType
    then decoder
    else fail errMsg

decodeOrFail' :: Serialise a => TokenType -> String -> Decoder s a
decodeOrFail' expectedType errMsg = decodeOrFail expectedType errMsg decode

-- wonky type signature, but basically just let us decode any bytestring newtype, with a fixed byte len
-- note that this version checks for CBOR TypeBytes, which is always definite and NOT indefinite, to mirror the rust code
decodeByteStringNewtypeWithLen :: forall a s. (Coercible a BS.ByteString) => Int -> String -> Decoder s a
decodeByteStringNewtypeWithLen expectedLen errMsg = do
  bs <- decodeOrFail' TypeBytes errMsg
  when (BS.length bs /= expectedLen) $ fail errMsg
  pure (coerce @_ @a bs)

decodePubKeyHashOrFail :: String -> Decoder s PubKeyHash
decodePubKeyHashOrFail = decodeByteStringNewtypeWithLen 28

decodeCardanoTxOrFailWithAdditionalText :: String -> Decoder s CardanoTx
decodeCardanoTxOrFailWithAdditionalText additionalErrMsg = do
  bs <- decodeOrFail' TypeBytes $ "Bad Gummiworm cardano transaction bytes" <> additionalErrMsg
  pure $ coerce @BS.ByteString @CardanoTx bs

-- NOTE: we append the argument after the error message
-- this obviously might not necessarily localize but it works fine in english, for the primary purpose of tracking index
decodeUserIdxEntryOrFailWithAdditionalText :: String -> Decoder s UserIdxEntry
decodeUserIdxEntryOrFailWithAdditionalText additionalErrMsg = do
    arrayLen <- decodeListLen
    when (arrayLen /= 4) $ fail $ "Bad Gummiworm archive transaction index entry array size" <> additionalErrMsg

    key <- decodeOrFail' @Text TypeString $ "Bad Gummiworm archive transaction index entry key" <> additionalErrMsg

    compPos <- decodeOrFail' @Word64 TypeUInt $ "Bad Gummiworm archive transaction index entry compressed position" <> additionalErrMsg

    uncompPos <- decodeOrFail' @Word64 TypeUInt $ "Bad Gummiworm archive transaction index entry uncompressed position" <> additionalErrMsg

    numBytes <- decodeOrFail' @Word64 TypeUInt $ "Bad Gummiworm archive transaction index entry length" <> additionalErrMsg

    pure $ UserIdxEntry{key, compPos, uncompPos, numBytes}

-- we're just going to use the typeclass here instead of defining decodeWithError for every type
instance Serialise GummiwormVersion where
  encode (GummiwormVersion bs) = encode bs
  decode = decodeByteStringNewtypeWithLen 2 "Bad Gummiworm version bytes"

instance Serialise GummiwormId where
  encode (GummiwormId bs) = encode bs
  decode = decodeByteStringNewtypeWithLen 28 "Bad Gummiworm ID bytes"

instance Serialise PubKey where
  encode (PubKey bs) = encode bs
  decode = decodeByteStringNewtypeWithLen 32 "Bad Gummiworm public key hash bytes"

instance Serialise PubKeyHash where
  encode (PubKeyHash bs) = encode bs
  decode = decodePubKeyHashOrFail "Bad Gummiworm public key hash bytes"

instance Serialise Signature where
  encode (Signature bs) = encode bs
  decode = decodeByteStringNewtypeWithLen 64 "Bad Gummiworm signature bytes"


instance Serialise FileHash where
  encode (FileHash bs) = encode bs
  decode = decodeByteStringNewtypeWithLen 32 "Bad Gummiworm file hash bytes"


instance Serialise CardanoTx where
  encode (CardanoTx bs) = encode bs
  decode = decodeCardanoTxOrFailWithAdditionalText ""

instance Serialise UserIdxEntry where
  encode x = encodeListLen 4 <> encode (key x) <> encode (compPos x) <> encode (uncompPos x) <> encode (numBytes x)
  {-# HLINT ignore "Use <$>" #-}
  decode = decodeUserIdxEntryOrFailWithAdditionalText ""
  --TODO: better error handling, Codec.Serialise only has monadfail which isnt enough to try a parse and fail with explicit error
  -- CBOR library itself probably has like an Alternative instance itself or something
  -- the peek token type thing is a little ugly but i dont wanna spend too much time on this

instance Serialise ArchiveMetadata where
  encode ArchiveMetadata{gummiwormArchiveVersion, timeStamp, gummiwormId, gummiwormOperatorList, txIndexRangeStart, txIndexRangeEnd} =
    encodeListLen 6 
    <> encode gummiwormArchiveVersion 
    <> encode (fromInteger @Word64 . truncate . nominalDiffTimeToSeconds $ timeStamp) --TODO: this might need to have a fromIntegral or something to pack into an int/word
    <> encode gummiwormId 
    -- haskell language server says `slist` has a version of `length` that works with infinite lists (presumably peano encoded?)
    -- gummiworm operator list is currently always (appreciably) finite, but that would be of use for very very large streaming archives
    -- NOTE: default encode for [] is indefinite, which is why we do this
    <> encodeListLen (fromIntegral @Int @Word . length $ gummiwormOperatorList)
        <> foldMap encode gummiwormOperatorList
    <> encode txIndexRangeStart 
    <> encode txIndexRangeEnd
  decode = do
    arrayLen <- decodeListLenOrIndef
    when (arrayLen /= Just 6) $ fail "Bad Gummiworm archive metadata array size"

    gummiwormArchiveVersion <- decode

    timeStamp <- do
      -- apparently u64 in minicbor corresponds to TypeUInt, not TypeUInt64
      secondsSinceUnixEpoch <- decodeOrFail' @Word64 TypeUInt "Bad Gummiworm archive metadata timestamp"
      pure $ secondsToNominalDiffTime $ fromIntegral secondsSinceUnixEpoch


    gummiwormId <- decode

    numOperators <- decodeListLenOrIndef
    gummiwormOperatorList <- case numOperators of
        -- Just num -> forM [0..num] $ \i -> decodePubKeyHashOrFail ("Bad Gummiworm public key hash bytes at index " <> show i)
        -- this might look like the rust code but apparently rust .. is not inclusive, but in haskell enumFromThenTo ([..]) is -^
        Just num -> forM [1..num] $ \i -> decodePubKeyHashOrFail ("Bad Gummiworm public key hash bytes at index " <> show (i - 1))
        -- i think also fromEnumThenTo should give [] if [0..num-1] goes out of bounds, so that could also work
        Nothing -> fail "Bad Gummiworm archive metadata witness array"

    txIndexRangeStart <- decodeOrFail' TypeUInt "Bad Gummiworm archive metadata transaction index range start"

    txIndexRangeEnd <- decodeOrFail' TypeUInt "Bad Gummiworm archive metadata transaction index range end"

    pure $ ArchiveMetadata{gummiwormArchiveVersion, timeStamp, gummiwormId, gummiwormOperatorList, txIndexRangeStart,txIndexRangeEnd}

instance Serialise ArchiveTransactionIdx where
  encode ArchiveTransactionIdx{gummiwormArchiveVersion, transactionIndices} =
    encodeListLen 2 <> encode gummiwormArchiveVersion <> encode transactionIndices
  decode = do
    arrayLen <- decodeListLenOrIndef
    when (arrayLen /= Just 2) $ fail "Bad Gummiworm archive transaction indices array size"

    gummiwormArchiveVersion <- decode

    numEntries <- decodeListLenOrIndef
    transactionIndices <- case numEntries of
        Just num -> forM [0..num] $ \i -> decodeUserIdxEntryOrFailWithAdditionalText (" for index " <> show i)
        Nothing -> fail "Bad Gummiworm archive transaction indices entry array"

    pure $ ArchiveTransactionIdx{gummiwormArchiveVersion, transactionIndices}


instance Serialise ArchiveTransactions where
  encode ArchiveTransactions{gummiwormArchiveVersion, transactions} =
    encodeListLen 2 <> encode gummiwormArchiveVersion <> encodeListLenIndef <> foldMap encode transactions <> encodeBreak
    -- it seems the default as of time of writing is that encode default instance for lists is indef, but we hardcode this just to be sure
  decode = do
    arrayLen <- decodeListLenOrIndef
    when (arrayLen /= Just 2) $ fail "Bad Gummiworm archive transactions array size"

    gummiwormArchiveVersion <- decode @GummiwormVersion

    -- if we ever import monad-loops we could probably rewrite this to not have manual recursion
    -- it probably doesn't matter though
    let tillBreakGo :: Int -> Decoder s [CardanoTx]
        tillBreakGo i = do
          peekTokenType >>= \case
            TypeBreak -> pure []
            _ -> do
              x <- decodeCardanoTxOrFailWithAdditionalText (" for index " <> show i)
              (x:) <$> tillBreakGo (i+1)
    numTransactions <- decodeListLenOrIndef
    transactions <- case numTransactions of
        Just num -> forM [0..num - 1] $ \i -> decodeCardanoTxOrFailWithAdditionalText (" for index " <> show i)
        Nothing -> tillBreakGo 0

    {-# HLINT ignore "Redundant fmap" #-}
    fmap (numTransactions,) decodeBreakOr >>= \case
      (Just _, True) -> fail "Bad Gummiworm archive transactions, definite transaction list with unnecessary break/terminator"
      (Just _, False) -> pure ()
      (Nothing, False) -> fail "Bad Gummiworm archive transactions, indefinite transaction list lacking break/terminator"
      (Nothing, True) -> pure ()

    pure $ ArchiveTransactions{gummiwormArchiveVersion, transactions}

instance Serialise Witness where
  encode Witness{pubKey, signature} =
    fold [ encodeListLen 2
         , encode pubKey
         , encode signature ]
  decode = do
    arrayLen <- decodeListLenOrIndef
    when (arrayLen /= Just 2) $ fail "Bad Gummiworm archive witness array size"

    pubKey <- decode
    signature <- decode

    pure $ Witness{pubKey, signature}

instance Serialise ArchiveWitnesses where 
  encode
    ArchiveWitnesses{gummiwormArchiveVersion, metadataFileHash, txFileHash, txGziFileHash, txIdxFileHash, witnesses} =
      fold [ encodeListLen 6
           , encode gummiwormArchiveVersion
           , encode metadataFileHash
           , encode txFileHash
           , encode txGziFileHash
           , encode txIdxFileHash
           , encode witnesses ]
  decode = do
    gummiwormArchiveVersion <- decode @GummiwormVersion
    metadataFileHash <- decode @FileHash
    txFileHash <- decode @FileHash
    txGziFileHash <- decode @FileHash
    txIdxFileHash <- decode @FileHash
    witnesses <- decode @[Witness]
    pure $ ArchiveWitnesses {gummiwormArchiveVersion, metadataFileHash, txFileHash, txGziFileHash, txIdxFileHash, witnesses}

readTransactionsFrom :: LByteString 
    -> IO (Either
          _ [Tx])
readTransactionsFrom bs =
  do
    -- FIXME(ELAINE): unify error handling
    txes <- fmap (deserialiseOrFail @ArchiveTransactions . decompress) (pure bs)  >>= \case
      Left e -> throwIO $ PersistenceException (show e)
      Right decoded -> pure decoded
    let deserializeTxesFromArchiveTransactions =  fmap (deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) . coerce @_ @BS.ByteString) . transactions
      -- in traverse_ applyTxIO (deserializeTxesFromArchiveTransactions txes)
    pure $ sequenceA (deserializeTxesFromArchiveTransactions txes)