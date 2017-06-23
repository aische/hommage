-- | HOMMAGE - Haskell Offline Music Manipulation And Generation EDSL
--
--   Daniel van den Eijkel  <mailto:dvde@gmx.net>
module Sound.Hommage
 (

 -- * Basic sound stuff

 -- | Mono and stereo signals:
   module Sound.Hommage.Signal
 -- | Oscillators
 , module Sound.Hommage.Osc
 -- | Envelopes
 , module Sound.Hommage.Envelope
 -- | Filter
 , module Sound.Hommage.Filter

 -- * Musical Notation and Sythesizers

  -- | A datatype for representing musical structures (idea stolen from Haskore)
 , module Sound.Hommage.Notation
 -- | The Play Monad
 , module Sound.Hommage.Play
 -- | The Sound and Effect classes
 , module Sound.Hommage.Sound

 -- * More usable stuff

 -- | A datatype for buffered audio data
 , module Sound.Hommage.Sample
 -- | Creating MIDI files
 , module Sound.Hommage.Midi
 -- | Pure Haskell FFT Filter
 , module Sound.Hommage.FFT
 -- | Random Notation
 , module Sound.Hommage.Rand
 -- Tools
 , module Sound.Hommage.Tools

 -- * Internals

 -- | File Input\/Output for binary files and WAV files:
 , module Sound.Hommage.WavFile
 -- | Basic module for DFT transforming (depends on a C-code file):
 , module Sound.Hommage.DFTFilter
 -- | Audio rendering
 , module Sound.Hommage.Seq
 -- | Miscelaneous convenient functions, collected in an independent module:
 , module Sound.Hommage.Misc
 )
 where

import Sound.Hommage.Misc
import Sound.Hommage.WavFile
import Sound.Hommage.DFTFilter
import Sound.Hommage.Signal
import Sound.Hommage.Sample

import Sound.Hommage.Midi
import Sound.Hommage.Notation
import Sound.Hommage.Seq

import Sound.Hommage.Osc
import Sound.Hommage.Envelope
import Sound.Hommage.Filter

import Sound.Hommage.Play
import Sound.Hommage.Sound
import Sound.Hommage.FFT
import Sound.Hommage.Rand
import Sound.Hommage.Tools

{-

module Sound.Hommage.WavFile
 (
 -- * Binary Files
   writeDataFile
 , readDataFile
 , openDataFile

 -- * WAV-Files
 , writeWavFile
 , writeWavFileMono
 , writeWavFileStereo
 , readWavFile
 , openWavFile

 -- * Cast between Int16 and Double representation of WAV-data
 , wavInt16ToDouble
 , wavDoubleToInt16

 -- * Low-Level implemetation issues
 -- ** Arrays and Files
 , readArrayFromFile
 , writeArrayToFileWithHeader
 , writeArrayToFile

 -- ** Single Stream
 , openSingleInputFile
 , openSingleOutputFile

 , openSingleInputWavFile
 , openSingleOutputWavFileMono
 , openSingleOutputWavFileStereo

 -- ** Buffered Stream
 , openInputFile
 , openOutputFile
 , openOutputFileWithHeader
 , openInputWavFile
 , openOutputWavFileMono
 , openOutputWavFileStereo

 -- ** Header Stuff and others
 , HeaderFun
 , HeaderSize
 , noHeader

 , wavHeaderFunMono
 , wavHeaderFunStereo
 , wavHeaderSize

 , initWriteWavHeaderMono
 , initWriteWavHeaderStereo
 , initReadWavHeader
 , closeWriteWavHeader
 , encode
 , decode
 , encodeWavLengt
 , initWavHeaderMono
 , initWavHeaderStereo
 , sizeOfArrayElements
 , inferSizeOfArrayElements
 , inferSizeOfArrayElements'
 )


module Sound.Hommage.Signal
 ( Mono
 , Stereo (..)
 , leftStereo
 , rightStereo
 , stereoToMono
 , monoToStereo
 , Signal (..)
 , signalToMono
 , signalToStereo
 , eitherSignal
 , readWavSignal
 , openWavSignal
 , writeWavMono
 , writeWavStereo
 , writeWavSignal
 )


module Sound.Hommage.Seq
 ( mixdownNumNotation
 , runReaderWithBPM
 , Seq (..)
 , noteSeq
 , noteSeq'
 , mixdownNumSeq
 , applySeq
 , applySeqS
 , applySeqE
 , filterSeq
 )

module Sound.Hommage.Sample
 (
 -- * Sample Datastructure
   Sample
 , arraySample
 , sizeSample
 , newSample
 , newSampleFromList

 -- * File Operations
 , openWavSample
 , saveWavSampleMono
 , saveWavSampleStereo

 -- * Pitching, Playing and Scratching
 , pitchWavSignal
 , scratchWavSignal
 , playSample
 , pitchSample
 , scratchSample
 , playCoeffs

 -- * Transforming Samples
 , mapSample
 , foldlSample
 , updateSample
 , normaliseSample
 , fadeinoutSample
 , stretchSample

 -- * Filter and Fourier Coeffs
 , Coeffs
 , arrayCoeffs
 , sizeCoeffs
 , wavesizeCoeffs
 , syntheseSample
 , analyseSample
 , filterCoeffs
 , mkPlayCoeffs
 , readCoeffs
 , writeCoeffs

 -- * Playing a Sample
 , mkTriggerSample
 , mkPlaySample
 , mkScratchSample
 , mkPitchSample

 -- * Array functions
 , interpolArray
 , mkLoopArray
 , mkPlayArray

 )

module Sound.Hommage.Osc
 (
 -- * Sound Generators
   osc
 , sinus
 , cosinus
 , rect
 , saw
 , tri

 -- * Functions for Lists
-- , amplify
 , sampleAndHold
 , average
 , terminateAt
 , follow
-- , variableDelay

 -- * Functions for single values
 -- | These Functions can be used with 'map':
 , compress
 , noteToFrequency
 , adjustFrequency

 -- * Other Functions
 -- | These Functions are not simple (i. e. linear) list transformers:
 , splitWaves
 , crossfade
 )


module Sound.Hommage.Notation
 (
 -- * Duration
   Dur
 , absDur
 -- * Music Notation
 , Notation (..)
 , unmaybeNotation
 , runNotation
 , runNotationWith
 -- * Musical class
 , Stretchable (..)
 , Arrangeable (..)
 , Musical (..)

 -- * Notation and Midi
 , writeMidiSyncNotation
 , midi
 , midi'
 , midiSyncFile

 -- * More Notation functions
 , note
 , mapNotation

 )

module Sound.Hommage.Misc
 (
 -- * Imperative control structure
   for
 , for'

 -- * Usefull list functions
 , map_
 , foldr_
 , merge
 , uneitherlist

 , walk
 , appendmaps
 , appendmaps'

 , qsort
 , qsortM

 -- * Embedding IO in a list (-map)
 , inList
 , inList'
 , inList''
 , toList
 , toList'
 , inListE
 , inListE'
 , inListE''
 , toListE
 , toListE'

 )

module Sound.Hommage.Midi
 ( Ticks
 , MidiFile (..)
 , writeMidiFile
 , Delta
 , MidiTrack

 , Chan
 , MidiValue
 , MidiMusic
 , MidiNote (..)

 , noteMidiMusic
 , restMidiMusic
 , appendMidiMusic
 , mergeMidiMusic
 , runMidiMusic

 , MidiEvent (..)
 , encodeMidiFile
 , midiHeaderSync
 , encodeMidiTrack
 , encodeDelta
 , encodeMidiDeltaEvent
 , encodeEvent
 , lastByte
 , nextByte
 , toBytes
 )

module Sound.Hommage.Filter
 (
 -- * Filters
   lowpassfilter
 , highpassfilter
 , bandpassfilter
 , stretchfilter
 , morphfilter
 , vocoder

 -- * FilterSpec
 , specfilter
 , FilterSpec
 , shiftFilterSpec

 -- * FilterSpec and Lists
 , filterSpecToList
 , filterSpecsToLists
 , interpolFilterLists

 -- * Other
 , ffreq
 , ffreqi
 , average512

 -- * Filterfunctions
 , lowpassFS
 , highpassFS
 , bandpassFS
 , lowp0FS
 , lowp1FS
 , bandp0FS
 , bandp1FS
 , highp0FS
 , highp1FS

 )

module Sound.Hommage.Envelope
 (

 -- * ADSR
   ADSR
 -- * ADSR Functions
 , listADSR
 , listADSR'
 , stretchADSR
 , stretchToADSR
 , envpart
 , envpart_cos
 -- * Envelope
 , EnvShape (..)
 , EnvMode (..)
 , playADSR
 , Env (..)
 , playEnv
 )

module Sound.Hommage.DFTFilter
 (
 -- * User-Level Filter Functions
   dftfilter
 , dftsynthese
 , dftanalyse
 , dftfilterWith

 -- * IO and Array
 -- ** CoeffArr
 , CoeffArr
 , readCoeffArr
 , writeCoeffArr
 , storeCoeff
 , unstoreCoeff
 -- ** CoeffMap
 , CoeffMap
 , mkCoeffMap
 , coeffmap
 -- ** Analyse
 , mkAnalyse
 -- ** Synthese
 , mkSynthese
 -- ** Transform
 , mkFilterBuffered
 -- ** Interface to C-code
 , analyseDFT
 , syntheseDFT
 , kurveDFT
 )

-}

