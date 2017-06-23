module Sound.Hommage.Midi
 ( 
 -- | The action 'writeMidiFile' stores the datatype 'MidiFile' in a MIDI-File. 
 --   It consists of a field for the 'Ticks' per quarter and a 'MidiTrack' (or some). 
 --   The function 'runMidiMusic' produces such a track from a 'MidiMusic', which can
 --   be created using the functions 'noteMidiMusic', 'restMidiMusic', 'appendMidiMusic' and
 --   'mergeMidiMusic'. 
 --   'MidiNote' is used to create note or controller events; it consists of the Midi-Channel
 --   ('Chan') and two fields with a 'MidiValue'.

 -- * MidiFile
   MidiFile (..)
 , writeMidiFile 
 , Ticks 
 -- * MidiNote
 , MidiNote (..)
 , Chan 
 , MidiValue 
 -- * MidiMusic 
 , MidiMusic
 , noteMidiMusic 
 , restMidiMusic 
 , appendMidiMusic 
 , mergeMidiMusic 
 , runMidiMusic 
 -- * MidiTrack
 , MidiTrack 
 , Delta 
 -- * MidiEvent
 , MidiEvent (..)
 -- * Encoding
 , encodeMidiFile 
 , midiHeaderSync 
 , midiHeaderSingle
 , midiHeaderAsync  
 , encodeMidiTrack
 , encodeDelta 
 , encodeMidiDeltaEvent 
 , encodeEvent 
 , lastByte 
 , nextByte 
 , toBytes 
 )
 where

import Sound.Hommage.WavFile

import Data.Word
import Data.Bits
import Data.Ratio
---------------------------------------------------------------------------------------------------
type Delta = Int
-- | A kind of extended MidiTrack type, that allows delta-times without events 
--   (will be summed up by converting to 'MidiTrack').
type MidiMusic = [(Delta, Maybe MidiEvent)]
-- | A sequence of MidiEvents with a delta-time which means the number of ticks
--   before the event happens (relative to the last event). 
type MidiTrack = [(Delta, MidiEvent)]
-- | Ticks per Quarter. For some unknown reasons it does not work with any value.
--   96 seems to be ok. 
type Ticks = Int
---------------------------------------------------------------------------------------------------
-- | Converts a 'MidiMusic' to a 'MidiTrack'. 
runMidiMusic :: MidiMusic -> MidiTrack 
runMidiMusic mt = loop 0 mt
 where
  loop delta ((d, Just e)  : r) = (delta + d, e) : loop 0 r
  loop delta ((d, Nothing) : r) = loop (d+delta) r
  loop delta []                 = [(delta, MidiEndOfTrack)]

-- | Writes a 'MidiFile' to a File.
writeMidiFile :: FilePath -> MidiFile -> IO ()
writeMidiFile fp mf = writeDataFile fp $ encodeMidiFile mf
---------------------------------------------------------------------------------------------------
-- | A Midi-Channel (0-15)
type Chan = Word8
-- | A Midi-Value (0-127)
type MidiValue = Word8
---------------------------------------------------------------------------------------------------
data MidiNote = MidiNote Chan MidiValue MidiValue -- ^ A note with pitch and volume
              | MidiCtrl Chan MidiValue MidiValue -- ^ Controllernumber and value
---------------------------------------------------------------------------------------------------
-- | Creates a note or controller event with given length (relative to 'Ticks'). 
--   The length means the delta-time between note-on and note-off resp. the time after
--   the controller event.
noteMidiMusic :: Int -> MidiNote -> MidiMusic
noteMidiMusic d (MidiNote c n v) = 
 [(0, Just $ MidiNoteOn c n v), (d, Just $ MidiNoteOff c n v)]
noteMidiMusic d (MidiCtrl c n v) = 
 [(0, Just $ MidiControl c n v), (d, Nothing)]

-- | A pause with given length.
restMidiMusic :: Int -> MidiMusic
restMidiMusic d = [(d, Nothing)]
---------------------------------------------------------------------------------------------------
-- | Parallel composition of 'MidiMusic'.
mergeMidiMusic :: MidiMusic -> MidiMusic -> MidiMusic
mergeMidiMusic ((d1,m1):r1) ((d2,m2):r2) | d1 < d2   = (d1,m1) : mergeMidiMusic r1 ((d2-d1,m2):r2)
                                         | d1 > d2   = (d2,m2) : mergeMidiMusic ((d1-d2,m1):r1) r2
                                         | otherwise = (d1,m1) : (0, m2) : mergeMidiMusic r1 r2
mergeMidiMusic [] r2 = r2
mergeMidiMusic r1 [] = r1

-- | Sequencial composition of 'MidiMusic'.
appendMidiMusic :: MidiMusic -> MidiMusic -> MidiMusic
appendMidiMusic = (++)
---------------------------------------------------------------------------------------------------
data MidiFile = MidiSync   Ticks [MidiTrack] -- ^ Some parallel (synchronous) MidiTracks. 
              | MidiSingle Ticks MidiTrack   -- ^ A single MidiTrack. (Seems not to work yet)
              | MidiAsync  Ticks [MidiTrack] -- ^ Some asynchronous MidiTracks. (Seems not to work yet)

data MidiEvent = MidiNoteOff  !Chan !MidiValue !MidiValue     -- note-nr and velocity
               | MidiNoteOn   !Chan !MidiValue !MidiValue     -- note-nr and velocity
               | MidiKeyAfter !Chan !MidiValue !MidiValue     -- note-nr and velocity
               | MidiControl  !Chan !MidiValue !MidiValue     -- controller-nr and value
               | MidiEndOfTrack
--               | MidiProgram  !Chan !MidiValue                -- program-nr
--               | MidiChannelAfter !Chan !MidiValue            -- channel-nr
--               | MidiPitchWheel   !Chan !MidiValue !MidiValue -- bottom, top
---------------------------------------------------------------------------------------------------
encodeMidiFile :: MidiFile -> [Word8]
encodeMidiFile mf =
 case mf of        -- HEADER              TRACKS                    TICKS
  MidiSync tc  mts -> midiHeaderSync   ++ toBytes 2 (length mts) ++ toBytes 2 tc ++ (mts >>= encodeMidiTrack)
  MidiSingle tc mt -> midiHeaderSingle ++ toBytes 2 1            ++ toBytes 2 tc  ++ encodeMidiTrack mt
  MidiAsync tc mts -> midiHeaderAsync  ++ toBytes 2 (length mts) ++ toBytes 2 tc ++ (mts >>= encodeMidiTrack)
---------------------------------------------------------------------------------------------------
midiHeaderSingle :: [Word8]
midiHeaderSingle = [ 77, 84, 104, 100, 0, 0, 0, 6, 0, 0]

midiHeaderSync :: [Word8]
midiHeaderSync = [ 77, 84, 104, 100, 0, 0, 0, 6, 0, 1]

midiHeaderAsync :: [Word8]
midiHeaderAsync = [ 77, 84, 104, 100, 0, 0, 0, 6, 0, 2]
---------------------------------------------------------------------------------------------------
encodeMidiTrack :: MidiTrack -> [Word8]
encodeMidiTrack ms = [ 77, 84, 114, 107 ] ++ toBytes 4 (length t) ++ t
 where
  t = (ms >>= encodeMidiDeltaEvent) 
---------------------------------------------------------------------------------------------------
encodeDelta :: Int -> [Word8]
encodeDelta n = loop n
 where 
  loop n | n < 128   = [fromIntegral n]
         | otherwise = fromIntegral (128 + mod n 128) : loop (div n 128)
---------------------------------------------------------------------------------------------------
encodeMidiDeltaEvent :: (Delta, MidiEvent) -> [Word8]
encodeMidiDeltaEvent (delta, me) = encodeDelta delta ++ encodeEvent me

encodeEvent :: MidiEvent -> [Word8]
encodeEvent me = 
 case me of
  MidiNoteOff ch no va  -> [ch + 128, no, va]
  MidiNoteOn  ch no va  -> [ch + 144, no, va]
  MidiKeyAfter ch no va -> [ch + 160, no, va]
  MidiControl ch no va  -> [ch + 176, no, va]
  MidiEndOfTrack        -> [255,47,0]
--------------------------------------------------------------------------------------------------
lastByte :: Int -> Word8
lastByte n = fromIntegral (mod n 256)

nextByte :: Int -> Int
nextByte n = div n 256

toBytes :: Int -> Int -> [Word8]
toBytes k n = reverse $ take k $ map lastByte $ iterate nextByte n  -- reverse?
---------------------------------------------------------------------------------------------------

