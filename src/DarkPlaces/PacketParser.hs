module DarkPlaces.PacketParser (
    DPServerPacket(..),
    ProtocolVersion(..),
    ServerInfoData(..),
    SoundInfoData(..),
    PacketOrError(),
    defaultDemoState,
    parsePacket,
    parsePackets,
    iterPacketsWithState,
    iterPackets,
    listPackets
) where

import Prelude hiding (sequence)
import Control.Monad hiding (sequence, mapM)
import Control.Applicative
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Word
import Data.Int
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Traversable (sequence)
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Data.Bits
import DarkPlaces.ProtocolConstants
import DarkPlaces.Types
import DarkPlaces.Binary


data DPServerPacket = DPNop
                    | DPDisconnect
                    | DPUpdateStat (Either Word8 ClientStatsEnum) Int
                    | DPVersion (Maybe ProtocolVersion)
                    | DPSetView Word16
                    | DPSound SoundInfoData QVector -- <data> <position>
                    | DPTime Float
                    | DPPrint BL.ByteString
                    | DPStuffText BL.ByteString 
                    | DPSetAngle SetAngleData
                    | DPServerInfo (Either Word32 ServerInfoData)
                    | DPLightStyle Word8 BL.ByteString
                    | DPUpdateName Word8 BL.ByteString  -- <user number> <user name>
                    | DPUpdateFrags Word8 Int16
                    | DPClientData Word32 ClientDataPacket -- <bits> <client data>
                    | DPStopSound Word16
                    | DPUpdateColors Word8 Word8 -- <user number> <user color>
                    | DPParticle
                    | DPDamage Int Int QVector -- <armor> <blood> <from locatiom>
                    | DPSpawnStatic
                    | DPSpawnBaseline
                    | DPTempEntity
                    | DPSetPause
                    | DPSignonNum Word8
                    | DPCenterPrint
                    | DPKilledMonster
                    | DPFoundSecret
                    | DPSpawnStaticSound Bool QVector Int Word8 Word8 -- <type> <Vector origin> <Number> <vol> <atten>
                    | DPIntermission
                    | DPFinale BL.ByteString
                    | DPCDTrack Word8 Word8 -- <cd track> <loop track>
                    | DPSellScreen
                    | DPCutScene
                    | DPShowlmp
                    | DPHidelmp
                    | DPSkybox -- 37
                    | DPDownloadData Word32 Word16 BL.ByteString -- <start> <size> <data> 50
                    | DPUpdateStatUbyte (Either Word8 ClientStatsEnum) Int
                    | DPPointParticles Word16 QVector QVector Word16 -- <effect index> <origin> <velocity> <count> 61
                    | DPPointParticles1 Word16 QVector -- <efect num> <start>
    deriving(Show, Eq)


data ServerInfoData = QWServerInfoData
                    | DPServerInfoData {
    dpserverProtocol :: ProtocolVersion,
    dpmaxClients :: Word8,
    dpgameType :: Word8,
    dpsignonMessage :: BL.ByteString,
    dpmodelsPrecached :: [BL.ByteString],
    dpsoundsPrecached :: [BL.ByteString]
    } deriving(Show, Eq)


data SoundInfoData = QWSoundData Word8 Float Int Int Int
                -- <volume> <atten> <ent> <channel> <sound num>, speed always 1.0
                   | DPSoundData Word8 Float Float Int Int Int
                -- <volume> <atten> <speed> <ent> <channel> <sound num>
    deriving(Show, Eq)


data SetAngleData = SetAngleOld Float Float Float
                  | SetAngleNew Float Float Float
    deriving(Show, Eq)


data ClientDataPacket = ClientDataPacket {
    mpunchAngle :: QVector,
    mpunchVector :: QVector,
    mvelocity :: QVector,
    onGround :: Bool,
    inWater :: Bool,
    idealPitch :: Maybe Float,
    statsInfo :: [(ClientStatsEnum, Int)]
} deriving(Show, Eq)


data ServerProtocolState = ServerProtocolState {
    protocol :: ProtocolVersion,
    gamemode :: GameMode
} deriving(Show, Eq)



type ServerPacketParser = Get DPServerPacket
type ServerProtocolStateM a = StateT ServerProtocolState Get a
type PacketOrError = Either Word8 DPServerPacket


getProtocol :: ServerProtocolStateM ProtocolVersion
getProtocol = protocol <$> get

setProtocol :: ProtocolVersion -> ServerProtocolStateM ()
setProtocol proto = modify $ \s -> s {protocol=proto}

getGameMode :: ServerProtocolStateM GameMode
getGameMode = gamemode <$> get


setGameMode :: GameMode -> ServerProtocolStateM ()
setGameMode mode = modify $ \s -> s {gamemode=mode}


updateProtoState :: DPServerPacket -> ServerProtocolStateM ()
updateProtoState (DPVersion (Just p)) = setProtocol p
updateProtoState (DPServerInfo (Right p@(DPServerInfoData {}))) = setProtocol $ dpserverProtocol p
updateProtoState _ = return ()


updatesState :: DPServerPacket -> ServerProtocolStateM DPServerPacket
updatesState x = updateProtoState x >> return x

defaultDemoState :: ServerProtocolState
defaultDemoState = ServerProtocolState {protocol=ProtocolDarkplaces7, gamemode=GameXonotic}


parsePacket :: ServerProtocolStateM PacketOrError
parsePacket = sequence =<< getServerPacketParser <$> lift getWord8


parsePackets :: ServerProtocolStateM [PacketOrError]
parsePackets = do
    empty <- lift isEmpty
    if empty
        then return []
        else do
            either_packet <- parsePacket
            case either_packet of
                Right packet -> (Right packet:) <$> parsePackets
                Left t -> return [Left t]


iterPacketsWithState :: BL.ByteString -> ServerProtocolState -> [Either ErrorInfo (PacketOrError, ServerProtocolState)]
iterPacketsWithState packets_data state = go (decoder state) $ BL.toChunks packets_data
  where
    decoder s = runGetIncremental (runStateT parsePacket s)
    go (Fail _ offset msg) _ = [Left (offset, msg)]
    go (Partial k) [] = go (k Nothing) []
    go (Partial k) (x:xs) = go (k $ Just x) xs
    go (Done left _ (res, s')) xs = Right (res, s') : if end then [] else go (decoder s') xs'
      where
        empty = null xs && B.null left
        end = empty || isLeft res
        xs' = left:xs


iterPackets :: BL.ByteString -> ServerProtocolState -> ([Either ErrorInfo PacketOrError], ServerProtocolState)
iterPackets packets_data state = convert (iterPacketsWithState packets_data state) state
  where
    convert (x:xs) s = case x of
        Right (p, s') -> let (res, s'') = convert xs s'
                         in (Right p : res, s'')

        Left (offset, msg) -> ([Left (offset, msg)], s)

    convert [] s = ([], s)


listPackets :: BL.ByteString -> ServerProtocolState -> Either ErrorInfo ([PacketOrError], ServerProtocolState)
listPackets packets_data state = convert $ runGetOrFail (runStateT parsePackets state) packets_data
  where
    convert (Left (_, offset, msg)) = Left (offset, msg)
    convert (Right (_, _, r)) = Right r


getServerPacketParser :: Word8 -> Either Word8 (ServerProtocolStateM DPServerPacket)
getServerPacketParser t = case t of
    1 -> Right $ lift getNop
    2 -> Right $ lift getDisconnect
    3 -> Right $ lift getUpdateStats
    4 -> Right $ lift getVersion >>= updatesState
    5 -> Right $ lift getSetView
    6 -> Right $ lift . getSound False =<< getProtocol
    7 -> Right $ lift getTime
    8 -> Right $ lift getPrint
    9 -> Right $ lift getStuffText
    10 -> Right $ lift . getSetAngle =<< getProtocol
    11 -> Right $ lift getServerInfo >>= updatesState
    12 -> Right $ lift getLightStyle
    13 -> Right $ lift getUpdateName
    14 -> Right $ lift getUpdateFrags
    15 -> Right $ getProtocol >>= \p -> getGameMode >>= lift . getClientData p
    16 -> Right $ lift getStopSound
    17 -> Right $ lift getUpdateColors
    19 -> Right $ lift . getDamage =<< getProtocol
    25 -> Right $ lift getSignonNum
    29 -> Right $ lift . getSpawnStaticSound False =<< getProtocol
    30 -> Right $ lift getIntermission
    31 -> Right $ lift getFinale
    32 -> Right $ lift getCDTrack
    50 -> Right $ lift getDownloadData
    51 -> Right $ lift getUpdateStatUbyte
    59 -> Right $ lift . getSpawnStaticSound True =<< getProtocol
    61 -> Right $ lift . getPointParticles =<< getProtocol
    62 -> Right $ lift . getPointParticles1 =<< getProtocol
    _ ->  Left t

getNop :: ServerPacketParser
getNop = return DPNop

getDisconnect :: ServerPacketParser
getDisconnect = return DPDisconnect

getUpdateStats :: ServerPacketParser
getUpdateStats = do
    i <- getWord8
    let stats = maybe (Left i) Right $ statsFromNum i
    DPUpdateStat stats . fromIntegral <$> getInt32le

getVersion :: ServerPacketParser
getVersion = DPVersion . protocolVersionFromNum <$> getWord32le

getSetView :: ServerPacketParser
getSetView = DPSetView <$> getWord16le

getSound :: Bool -> ProtocolVersion -> ServerPacketParser
getSound large proto = DPSound
    <$> (if proto == ProtocolQuakeWorld
            then getQWSound
            else getDPSound)
    <*> getQVector proto
  where
    defaultVolume = 255
    defaultAttenuation = 1.0
    getQWSound = do
        val <- getWord16le
        volume <- if testBit val 15
            then getWord8
            else return defaultVolume

        atten <- if testBit val 14
            then (/ 64.0) . fromIntegral <$> getWord8
            else return defaultAttenuation

        sound_num <- fromIntegral <$> getWord8
        let v = fromIntegral val
        let (ent, channel) = (v `shiftR` 3 .&. 1023, v .&. 7)
        return $ QWSoundData volume atten ent channel sound_num

    getDPSound = do
        field_mask <- getWord8
        volume <- if testBit field_mask snd_volume_bit
            then getWord8
            else return defaultVolume

        atten <- if testBit field_mask snd_attenuation_bit
            then (/ 64.0) . fromIntegral <$> getWord8
            else return defaultAttenuation

        speed <- if testBit field_mask snd_speedushort4000_bit
            then (/ 4000.0) . fromIntegral <$> getWord16le
            else return 1.0

        (ent, channel) <- if testBit field_mask snd_largeentity_bit
            then (\e c -> (fromIntegral e, fromIntegral c)) <$> getWord16le <*> getInt8
            else (\v -> (v `shiftR` 3, v .&. 7)) . fromIntegral <$> getWord16le

        let large_sound_num = large ||
                              testBit field_mask snd_largesound_bit ||
                              proto == ProtocolNehahraBJP2 ||
                              proto == ProtocolNehahraBJP3

        sound_num <- if large_sound_num
            then fromIntegral <$> getWord16le
            else fromIntegral <$> getWord8

        return $ DPSoundData volume atten speed ent channel sound_num


getTime :: ServerPacketParser
getTime = DPTime <$> getFloat32le

getPrint ::ServerPacketParser
getPrint = DPPrint <$> getLazyByteStringNul

getStuffText :: ServerPacketParser
getStuffText = DPStuffText <$> getLazyByteStringNul

getSetAngle :: ProtocolVersion -> ServerPacketParser
getSetAngle proto = DPSetAngle <$> if proto `elem` [(ProtocolDarkplaces5)..]
    then SetAngleNew <$> getAngle16i <*> getAngle16i <*> getAngle16i
    else SetAngleOld <$> getAngle8i <*> getAngle8i <*> getAngle8i

 
-- TODO: not full
getServerInfo :: ServerPacketParser
getServerInfo = do
    proto_num <- getWord32le
    let maybe_proto = protocolVersionFromNum proto_num
    case maybe_proto of
        Nothing -> return $ DPServerInfo (Left proto_num)
        Just proto@(ProtocolQuakeWorld) -> toDPServerPacket <$> parseQuakeWorldInfo proto
        Just proto -> toDPServerPacket <$> parseOtherInfo proto
  where
    parseQuakeWorldInfo proto = undefined
    parseOtherInfo proto = do
        maxclients <- getWord8
        gametype <- getWord8
        signon_msg <- getLazyByteStringNul
        models_precached <- getStringList
        sounds_precached <- getStringList
        return DPServerInfoData {
            dpserverProtocol=proto,
            dpmaxClients=maxclients,
            dpgameType=gametype,
            dpsignonMessage=signon_msg,
            dpmodelsPrecached=models_precached,
            dpsoundsPrecached=sounds_precached}
    toDPServerPacket = DPServerInfo . Right


getLightStyle :: ServerPacketParser
getLightStyle = DPLightStyle <$> getWord8 <*> getLazyByteStringNul


getUpdateName :: ServerPacketParser
getUpdateName = DPUpdateName <$> getWord8 <*> getLazyByteStringNul

getUpdateFrags :: ServerPacketParser
getUpdateFrags = DPUpdateFrags <$> getWord8 <*> (fromIntegral <$> getWord16le)

getClientData :: ProtocolVersion -> GameMode -> ServerPacketParser
getClientData proto mode = do
    bits <- getBits
    ms_view_height <- maybeDo (testBit bits su_viewheight_bit) getInt8
    m_ideal_pitch <- maybeDo (testBit bits su_idealpitch_bit) getInt8
    (p_angl, p_vec, vel) <- getMpVectors bits
    ms_items <- maybeDo (testBit bits su_items_bit || proto `elem` hipnotic_demos) getInt32le
    stats' <- case proto of
        ProtocolDarkplaces5 -> parseDP5Stats bits
        _  | proto `elem` (quakes ++ neharaFamily ++ darkplacesUpto4) -> execWriterT $ getOldStats bits
        _       -> return []

    m_view_zoom <- case testBit bits su_viewzoom_bit of
        True | proto `elem` [(ProtocolDarkplaces2)..(ProtocolDarkplaces4)] -> Just <$> getWord8asInt
        True  -> Just <$> getWord16asInt
        False -> return Nothing

    let view_zoom = maybeToList $ (\n -> (ViewZoomStat, n)) <$> m_view_zoom

    let stats = toStats ms_view_height ViewHeightStat ++
                toStats ms_items ItemsStat ++ stats' ++ view_zoom

    return $ DPClientData bits ClientDataPacket {
        mpunchAngle=p_angl,
        mpunchVector=p_vec,
        mvelocity=vel,
        onGround=testBit bits su_onground_bit,
        inWater=testBit bits su_inwater_bit,
        idealPitch= fromIntegral <$> m_ideal_pitch,
        statsInfo=stats
    }
  where
    quakes =[ProtocolQuake, ProtocolQuakeDP]
    neharaFamily = [(ProtocolNehahraMovie)..(ProtocolNehahraBJP3)]
    darkplacesUpto4 = [(ProtocolDarkplaces1)..(ProtocolDarkplaces4)]
    hipnotic_demos = quakes ++ neharaFamily ++ [(ProtocolDarkplaces1)..(ProtocolDarkplaces5)]
    getWord16as32 = (fromIntegral :: Word16 -> Word32) <$> getWord16le
    getWord8as32 = (fromIntegral :: Word8 -> Word32) <$> getWord8
    toStats num key = maybeToList $ (\n -> (key, fromIntegral n)) <$> num
    statsVal key n = [(key, n)]
    getBits = do
        bits <- getWord16as32

        bits <- if testBit bits su_extend1_bit
            then (\b -> bits .|. shift b 16) <$> getWord8as32
            else return bits

        bits <- if testBit bits su_extend2_bit
            then (\b -> bits .|. shift b 32) <$> getWord8as32
            else return bits
        
        return bits

    maybeDo cond res = if cond then Just <$> res else return Nothing 
    getMpVectors bits = do
        r_vecs <- forM [0..2] $ \i -> do
            p_angl <- if testBit bits (su_punch1_bit + i)
                then getPunchAngle
                else return 0

            p_vec <- if testBit bits (su_punchvec1_bit + i)
                then getPunchvec
                else return 0

            vel <- if testBit bits (su_velocity1_bit + i)
                then  getVelocity
                else return 0

            return (p_angl, p_vec, vel)

        let (angls, vecs, vels) = unzip3 r_vecs
        return (buildQVector angls, buildQVector vecs, buildQVector vels)
      where
        getPunchAngle :: Get Float
        getPunchAngle = if proto `elem` (neharaFamily ++ quakes)
            then fromIntegral <$> getInt8
            else getAngle16i

        getPunchvec = if proto `elem` darkplacesUpto4
            then getCord16i
            else getFloat32le

        getVelocity = if proto `elem` (quakes ++ neharaFamily ++ darkplacesUpto4)
            then (16 *) . fromIntegral <$> getInt8
            else getFloat32le

        buildQVector = fromJust . qvectorFromList

    maybeGetStat bits bit key = if testBit bits bit then statsVal key <$> getInt16asInt else return []
    getStat key = (\v -> (key, v)) <$> getInt16asInt
    parseDP5Stats bits = do
        stats <- sequence [maybeGetStat bits su_weaponframe_bit WeaponFrameStat,
            maybeGetStat bits su_armor_bit ArmorStat,
            maybeGetStat bits su_weapon_bit WeaponStat]

        stats' <- sequence $ getStat <$> [HealthStat, AmmoStat, ShellsStat, NailsStat,
            RocketsStat, CellsStat]

        stats'' <- statsVal ActiveWeaponStat <$> getWord16asInt
        
        return $ concat stats ++ stats' ++ stats''

    getOldStats :: Word32 -> WriterT ClientStatsList Get ()
    getOldStats bits = do
        when (testBit bits su_weaponframe_bit) $ do
            tell =<< statsVal WeaponFrameStat <$> lift getWord8asInt

        when (testBit bits su_armor_bit) $ do
            tell =<< statsVal ArmorStat <$> lift getWord8asInt

        when (testBit bits su_weapon_bit) $ do
            let r = lift $ if proto `elem` [(ProtocolNehahraBJP)..(ProtocolNehahraBJP3)]
                then getWord16asInt
                else getWord8asInt

            tell =<< statsVal WeaponStat <$> r

        tell =<< statsVal HealthStat <$> lift getInt16asInt
        tell =<< statsVal AmmoStat <$> lift getWord8asInt
        tell =<< statsVal ShellsStat <$> lift getWord8asInt
        tell =<< statsVal NailsStat <$> lift getWord8asInt
        tell =<< statsVal RocketsStat <$> lift getWord8asInt
        tell =<< statsVal CellsStat <$> lift getWord8asInt
        let awep = if mode `elem` [GameNexuiz, GameVoreTournament, GameHipnotic, GameRogue, GameQuoth]
            then shift 1 <$> getWord8asInt
            else getWord8asInt

        tell =<< statsVal ActiveWeaponStat <$> lift awep


getStopSound :: ServerPacketParser
getStopSound = DPStopSound <$> getWord16le -- (n `shiftR` 3) (n .&. 7)

getUpdateColors :: ServerPacketParser
getUpdateColors = DPUpdateColors <$> getWord8 <*> getWord8

-- 19
getDamage :: ProtocolVersion -> ServerPacketParser
getDamage p = DPDamage <$> getWord8asInt <*> getWord8asInt <*> getQVector p


-- 25
getSignonNum :: ServerPacketParser
getSignonNum = DPSignonNum <$> getWord8


-- 30
getIntermission :: ServerPacketParser
getIntermission = return DPIntermission

getFinale :: ServerPacketParser
getFinale = DPFinale <$> getLazyByteStringNul

getCDTrack :: ServerPacketParser
getCDTrack = DPCDTrack <$> getWord8 <*> getWord8

getDownloadData :: ServerPacketParser
getDownloadData = do
    start <- getWord32le
    size <- getWord16le
    download_data <- getLazyByteString $ fromIntegral size
    return $ DPDownloadData start size download_data


getUpdateStatUbyte :: ServerPacketParser
getUpdateStatUbyte = do
    i <- getWord8
    let stats = maybe (Left i) Right $ statsFromNum i
    v <- fromIntegral <$> getWord8
    return $ DPUpdateStatUbyte stats v


getSpawnStaticSound :: Bool -> ProtocolVersion -> ServerPacketParser
getSpawnStaticSound l p = DPSpawnStaticSound is_large <$> getQVector p <*> getSoundNum <*> getWord8 <*> getWord8
  where
    is_large = l || p == ProtocolNehahraBJP2
    getSoundNum = if is_large
        then fromIntegral <$> getWord16le
        else fromIntegral <$> getWord8


getPointParticles :: ProtocolVersion -> ServerPacketParser
getPointParticles p = DPPointParticles <$> getWord16le <*> getQVector p <*> getQVector p <*> getWord16le


getPointParticles1 :: ProtocolVersion -> ServerPacketParser
getPointParticles1 p = DPPointParticles1 <$> getWord16le <*> getQVector p
