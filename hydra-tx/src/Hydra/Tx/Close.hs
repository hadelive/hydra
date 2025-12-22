{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Close where

import Hydra.Cardano.Api
import Hydra.Prelude
import Codec.CBOR.Write (toStrictByteString)
import Codec.CBOR.Encoding (encodePreEncoded)
import Cardano.Ledger.Binary (serialize')
import qualified Data.ByteString.Lazy as LBS

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Builder (unsafeBuildTransaction)
import Hydra.Plutus.Extras.Time (posixFromUTCTime, posixToUTCTime)
import Hydra.Tx (
  ConfirmedSnapshot (..),
  HeadId,
  ScriptRegistry (headReference),
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
  fromChainSnapshotNumber,
  getSnapshot,
  hashUTxO,
  headIdToCurrencySymbol,
  headReference,
 )
import Hydra.Tx.Crypto (toPlutusSignatures)
import Hydra.Tx.Utils (IncrementalAction (..), findStateToken, mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (BuiltinByteString, toBuiltin)
import Cardano.Crypto.Hash (Blake2b_256, hashToBytes, hashWith)
import PlutusLedgerApi.V3 qualified as Plutus

-- * Construction

type PointInTime = (SlotNo, UTCTime)

-- | Representation of the Head output after a CollectCom transaction.
data OpenThreadOutput = OpenThreadOutput
  { openThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , openContestationPeriod :: OnChain.ContestationPeriod
  , openParties :: [OnChain.Party]
  }
  deriving stock (Eq, Show, Generic)

-- | Create a transaction closing a head with either the initial snapshot or
-- with a multi-signed confirmed snapshot.
closeTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Head identifier
  HeadId ->
  -- | Last known version of the open head.
  SnapshotVersion ->
  -- | Snapshot with instructions how to close the head.
  ConfirmedSnapshot Tx ->
  -- | Lower validity slot number, usually a current or quite recent slot number.
  SlotNo ->
  -- | Upper validity slot and UTC time to compute the contestation deadline time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  OpenThreadOutput ->
  IncrementalAction ->
  -- | Pondora NFT reference input
  Maybe TxIn ->
  Tx
closeTx scriptRegistry vk headId openVersion confirmedSnapshot startSlotNo (endSlotNo, utcTime) openThreadOutput incrementalAction pondoraRefInput =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference (headScriptRef : maybeToList pondoraRefInput) mempty
      & addTxOuts [headOutputAfter]
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxValidityLowerBound (TxValidityLowerBound startSlotNo)
      & setTxValidityUpperBound (TxValidityUpperBound endSlotNo)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "CloseTx")
 where
  OpenThreadOutput
    { openThreadUTxO = (headInput, headOutputBefore)
    , openContestationPeriod
    , openParties
    } = openThreadOutput

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  headRedeemer = toScriptData $ Head.Close closeRedeemer

  -- Extract OpenDatum from the head output and compute its hash
  -- For non-initial closes, this MUST succeed (hash is required on-chain)
  openDatumHash :: Maybe BuiltinByteString
  openDatumHash =
    case txOutScriptData (fromCtxUTxOTxOut headOutputBefore) >>= fromScriptData of
      Just (Head.Open openDatum) ->
        -- Hash the serialized ScriptData to match on-chain: blake2b_256(serialiseData(toBuiltinData openDatum))
        Just $ toBuiltin $ hashToBytes $ hashWith @Blake2b_256 id $ toStrictByteString $ toCBOR $ getScriptData $ toScriptData openDatum
      _ ->
        -- For initial snapshot, no OpenDatum is expected (Initial state)
        -- For confirmed snapshots, this should never happen - the head should be in Open state
        case confirmedSnapshot of
          InitialSnapshot{} -> Nothing
          ConfirmedSnapshot{} -> error "Expected Open datum in head output for confirmed snapshot close"

  closeRedeemer =
    case confirmedSnapshot of
      InitialSnapshot{} -> Head.CloseInitial{openDatumHash = Nothing}
      ConfirmedSnapshot{signatures, snapshot = Snapshot{version}} ->
        case incrementalAction of
          ToCommit utxo' ->
            if version == openVersion
              then
                Head.CloseUnusedInc
                  { signature = toPlutusSignatures signatures
                  , alreadyCommittedUTxOHash = toBuiltin $ hashUTxO utxo'
                  , openDatumHash  -- MUST be Just hash, enforced above
                  }
              else
                Head.CloseUsedInc
                  { signature = toPlutusSignatures signatures
                  , alreadyCommittedUTxOHash = toBuiltin $ hashUTxO utxo'
                  , openDatumHash  -- MUST be Just hash, enforced above
                  }
          ToDecommit utxo' ->
            if version == openVersion
              then
                Head.CloseUnusedDec
                  { signature = toPlutusSignatures signatures
                  , openDatumHash  -- MUST be Just hash, enforced above
                  }
              else
                Head.CloseUsedDec
                  { signature = toPlutusSignatures signatures
                  , alreadyDecommittedUTxOHash = toBuiltin $ hashUTxO utxo'
                  , openDatumHash  -- MUST be Just hash, enforced above
                  }
          NoThing ->
            Head.CloseAny
              { signature = toPlutusSignatures signatures
              , openDatumHash  -- MUST be Just hash, enforced above
              }

  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  headDatumAfter =
    mkTxOutDatumInline $
      Head.Closed
        Head.ClosedDatum
          { snapshotNumber =
              fromIntegral . number $ getSnapshot confirmedSnapshot
          , utxoHash =
              toBuiltin . hashUTxO $ Hydra.Tx.utxo (getSnapshot confirmedSnapshot)
          , alphaUTxOHash =
              case closeRedeemer of
                Head.CloseUsedInc{} ->
                  toBuiltin . hashUTxO @Tx . fromMaybe mempty . utxoToCommit $ getSnapshot confirmedSnapshot
                _ -> toBuiltin $ hashUTxO @Tx mempty
          , omegaUTxOHash =
              case closeRedeemer of
                Head.CloseUnusedDec{} ->
                  toBuiltin . hashUTxO @Tx . fromMaybe mempty . utxoToDecommit $ getSnapshot confirmedSnapshot
                _ -> toBuiltin $ hashUTxO @Tx mempty
          , parties = openParties
          , contestationDeadline
          , contestationPeriod = openContestationPeriod
          , headId = headIdToCurrencySymbol headId
          , contesters = []
          , version = fromIntegral openVersion
          }

  contestationDeadline =
    addContestationPeriod (posixFromUTCTime utcTime) openContestationPeriod

-- * Observation

data CloseObservation = CloseObservation
  { headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  , contestationDeadline :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeCloseTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CloseObservation
observeCloseTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open Head.OpenDatum{}, Head.Close{}) -> do
      (_, newHeadOutput) <- findTxOutByScript (utxoFromTx tx) Head.validatorScript
      newHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut newHeadOutput
      (closeContestationDeadline, onChainSnapshotNumber) <- case fromScriptData newHeadDatum of
        Just (Head.Closed Head.ClosedDatum{contestationDeadline, snapshotNumber}) ->
          pure (contestationDeadline, snapshotNumber)
        _ -> Nothing
      pure
        CloseObservation
          { headId
          , snapshotNumber = fromChainSnapshotNumber onChainSnapshotNumber
          , contestationDeadline = posixToUTCTime closeContestationDeadline
          }
    _ -> Nothing
