module PacketParser (
    DPServerPacket(..),
    ProtocolVersion(..),
    defaultDemoState,
    parsePacket,
    parsePackets
) where

import Prelude hiding (sequence)
import Control.Monad hiding (sequence, mapM)
import Control.Applicative
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Word
import Data.Int
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as SM
import Data.Traversable (sequence)
import Data.List (elem, foldl')
{-import Control.Monad.Writer.Lazy-}
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Data.Bits
import ProtocolConstants
import DPTypes



data ProtocolVersion = ProtocolQuake
                     | ProtocolQuakeWorld
                     | ProtocolQuakeDP
                     | ProtocolNehahraMovie
                     | ProtocolNehahraBJP
                     | ProtocolNehahraBJP2
                     | ProtocolNehahraBJP3
                     | ProtocolDarkplaces1
                     | ProtocolDarkplaces2
                     | ProtocolDarkplaces3
                     | ProtocolDarkplaces4
                     | ProtocolDarkplaces5
                     | ProtocolDarkplaces6
                     | ProtocolDarkplaces7
    deriving(Show, Eq, Ord, Bounded, Enum)


data DPServerPacket = DPBad
                    | DPNop
                    | DPDisconnect
                    | DPUpdateStat Word8 Word32 -- Should be signed ?
                    | DPVersion (Maybe ProtocolVersion)
                    | DPSetView Word16
                    | DPSound
                    | DPTime Float
                    | DPPrint L.ByteString
                    | DPStuffText L.ByteString 
                    | DPSetAngle SetAngleData
                    | DPServerInfo (Either Word32 ServerInfoData)
                    | DPLightStyle Word8 L.ByteString
                    | DPUpdateName Word8 L.ByteString  -- <user number> <user name>
                    | DPUpdateFrags Word8 Int16
                    | DPClientData Word32 ClientDataPacket -- <bits> <client data>
                    | DPStopSound Word16
                    | DPUpdateColors Word8 Word8 -- <user number> <user color>
                    | DPParticle
                    | DPDamage
                    | DPSpawnStatic
                    | DPSpawnBaseline
                    | DPTempEntity
                    | DPSetPause
                    | DPSignonNum Word8
                    | DPCenterPrint
                    | DPKilledMonster
                    | DPFoundSecret
                    | DPSpawnStaticSound
                    | DPIntermission
                    | DPFinale
                    | DPCDTrack Word8 Word8 -- <cd track> <loop track>
                    | DPSellScreen
                    | DPCutScene
                    | DPShowlmp
                    | DPHidelmp
                    | DPSkybox -- 37
                    | DPDownloadData Word32 Word16 L.ByteString -- <start> <size> <data> 50
                    | DPSpawnStaticSound2 QVector Word16 Word8 Word8 --  <Vector origin> <Number> <vol> <atten> 59
    deriving(Show, Eq)


data ServerInfoData = QWServerInfoData
                    | DPServerInfoData ProtocolVersion Word8 Word8 L.ByteString [L.ByteString] [L.ByteString]
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
    protocol :: ProtocolVersion
} deriving(Show, Eq)



type ServerPacketParser = Get DPServerPacket
type ServerProtocolStateM a = StateT ServerProtocolState Get a


getProtocol :: ServerProtocolStateM ProtocolVersion
getProtocol = protocol <$> get


defaultDemoState = ServerProtocolState {protocol=ProtocolDarkplaces7}


protocolVersionMaps :: [(Word32, ProtocolVersion, String)]
protocolVersionMaps = [
    (3504, ProtocolDarkplaces7, "DP7"),
	(3503, ProtocolDarkplaces6, "DP6"),
    (3502, ProtocolDarkplaces5, "DP5"),
	(3501, ProtocolDarkplaces4, "DP4"),
	(3500, ProtocolDarkplaces3, "DP3"),
	(97, ProtocolDarkplaces2, "DP2"),
	(96, ProtocolDarkplaces1, "DP1"),
	(15, ProtocolQuakeDP, "QUAKEDP"),
	(15, ProtocolQuake, "QUAKE"),
	(28, ProtocolQuakeWorld, "QW"),
	(250, ProtocolNehahraMovie, "NEHAHRAMOVIE"),
	(10000, ProtocolNehahraBJP, "NEHAHRABJP"),
	(10001, ProtocolNehahraBJP2, "NEHAHRABJP2"),
	(10002, ProtocolNehahraBJP3, "NEHAHRABJP3")]
 

-- get ProtocolVersion by Long Int
protocolVersionFromNum :: Word32 -> Maybe ProtocolVersion
protocolVersionFromNum key = SM.lookup key map_table
  where
    map_table = SM.fromList $ map (\(x, y, z) -> (x, y)) protocolVersionMaps


parsePacket :: ServerProtocolStateM (Either Word8 DPServerPacket)
parsePacket = sequence =<< getServerPacketParser <$> lift getWord8


parsePackets :: ServerProtocolStateM [Either Word8 DPServerPacket]
parsePackets = do
    empty <- lift isEmpty
    if empty
        then return []
        else do
            either_packet <- parsePacket
            case either_packet of
                Right packet -> (Right packet:) <$> parsePackets
                Left t -> return [Left t]


getServerPacketParser :: Word8 -> Either Word8 (ServerProtocolStateM DPServerPacket)
getServerPacketParser t = case t of
    0 -> Right $ lift parseBad
    1 -> Right $ lift parseNop
    2 -> Right $ lift parseDisconnect
    3 -> Right $ lift parseUpdateStats
    4 -> Right $ lift parseVersion
    5 -> Right $ lift parseSetView
    -- 6 sound
    7 -> Right $ lift parseTime
    8 -> Right $ lift parsePrint
    9 -> Right $ lift parseStuffText
    10 -> Right $ lift . parseSetAngle =<< getProtocol
    11 -> Right $ lift parseServerInfo
    12 -> Right $ lift parseLightStyle
    13 -> Right $ lift parseUpdateName
    14 -> Right $ lift parseUpdateFrags
    15 -> Right $ lift . parseClientData =<< getProtocol
    16 -> Right $ lift parseStopSound
    17 -> Right $ lift parseUpdateColors
    25 -> Right $ lift parseSignonNum
    32 -> Right $ lift parseCDTrack
    50 -> Right $ lift parseDownloadData
    59 -> Right $ lift parseSpawnStaticSound2
    _ ->  Left t

parseBad :: ServerPacketParser
parseBad = return DPBad

parseNop :: ServerPacketParser
parseNop = return DPNop

parseDisconnect :: ServerPacketParser
parseDisconnect = return DPDisconnect

parseUpdateStats :: ServerPacketParser
parseUpdateStats = DPUpdateStat <$> getWord8 <*> getWord32le

parseVersion :: ServerPacketParser
parseVersion = DPVersion . protocolVersionFromNum <$> getWord32le

parseSetView :: ServerPacketParser
parseSetView = DPSetView <$> getWord16le

-- parseSound for DPSound

parseTime :: ServerPacketParser
parseTime = DPTime <$> getFloat32le

parsePrint ::ServerPacketParser
parsePrint = DPPrint <$> getLazyByteStringNul

parseStuffText :: ServerPacketParser
parseStuffText = DPStuffText <$> getLazyByteStringNul

parseSetAngle :: ProtocolVersion -> ServerPacketParser
parseSetAngle proto = DPSetAngle <$> if proto `elem` [(ProtocolDarkplaces5)..]
    then SetAngleNew <$> getAngle16i <*> getAngle16i <*> getAngle16i
    else SetAngleOld <$> getAngle8i <*> getAngle8i <*> getAngle8i

 
-- TODO: not full
parseServerInfo :: ServerPacketParser
parseServerInfo = do
    proto_num <- getWord32le
    let maybe_proto = protocolVersionFromNum proto_num
    case maybe_proto of
        Nothing -> return $ DPServerInfo (Left proto_num)
        Just proto -> if proto == ProtocolQuakeWorld
            then toDPServerPacket <$> parseQuakeWorldInfo proto
            else toDPServerPacket <$> parseOtherInfo proto
  where
    parseQuakeWorldInfo proto = undefined
    parseOtherInfo proto = do
        maxclients <- getWord8
        gametype <- getWord8
        signon_msg <- getLazyByteStringNul
        models_precached <- getStringList
        sounds_precached <- getStringList
        return $ DPServerInfoData proto maxclients gametype signon_msg models_precached sounds_precached
    toDPServerPacket = DPServerInfo . Right


parseLightStyle :: ServerPacketParser
parseLightStyle = DPLightStyle <$> getWord8 <*> getLazyByteStringNul


parseUpdateName :: ServerPacketParser
parseUpdateName = DPUpdateName <$> getWord8 <*> getLazyByteStringNul

parseUpdateFrags :: ServerPacketParser
parseUpdateFrags = DPUpdateFrags <$> getWord8 <*> (fromIntegral <$> getWord16le)

parseClientData :: ProtocolVersion -> ServerPacketParser
parseClientData proto = do
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
    getWord8asInt = (fromIntegral :: Word8 -> Int) <$> getWord8
    getInt8asInt = (fromIntegral :: Int8 -> Int) <$> getInt8
    getWord16as32 = (fromIntegral :: Word16 -> Word32) <$> getWord16le
    getInt16asInt = (fromIntegral :: Int16 -> Int) <$> getInt16le
    getWord16asInt = (fromIntegral :: Word16 -> Int) <$> getWord16le
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
        -- TODO: check gamemode
        -- https://github.com/xonotic/darkplaces/blob/45f4690471d588436f2033dc5af008d40d57b36b/cl_parse.c#L2235


parseStopSound :: ServerPacketParser
parseStopSound = DPStopSound <$> getWord16le -- (n `shiftR` 3) (n .&. 7)

parseUpdateColors :: ServerPacketParser
parseUpdateColors = DPUpdateColors <$> getWord8 <*> getWord8


-- 25
parseSignonNum :: ServerPacketParser
parseSignonNum = DPSignonNum <$> getWord8

-- 32
parseCDTrack :: ServerPacketParser
parseCDTrack = DPCDTrack <$> getWord8 <*> getWord8

parseDownloadData :: ServerPacketParser
parseDownloadData = do
    start <- getWord32le
    size <- getWord16le
    download_data <- getLazyByteString $ fromIntegral size
    return $ DPDownloadData start size download_data


-- need check protocol for QVector
parseSpawnStaticSound2 :: ServerPacketParser
parseSpawnStaticSound2 = DPSpawnStaticSound2 <$> getQVector <*> getWord16le <*> getWord8 <*> getWord8


getStringList :: Get [L.ByteString]
getStringList = do
    str <- getLazyByteStringNul
    if L.null str
        then return []
        else (str :) <$> getStringList


getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8


getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le

getInt32le :: Get Int32
getInt32le = fromIntegral <$> getWord32le

getAngle8i :: Get Float
getAngle8i = (360.0 / 256.0 *) . fromIntegral <$> getInt8


getAngle16i :: Get Float
getAngle16i = (360.0 / 65536.0 *) . fromIntegral <$> getInt16le


getCord16i :: Get Float
getCord16i = fromIntegral <$> getInt16le
