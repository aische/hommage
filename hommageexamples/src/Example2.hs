module Example2 where

import Prelude hiding ((<*>))
import Sound.Hommage
import Data.List
import Data.Ratio
-------------------------------------------------------------------------------
bassdrum :: Double -> Notation (Play Signal)
bassdrum vol = note $ PlayWav "samples/bassdrum.wav" ==> Amplifier vol

hihat :: Double -> Notation (Play Signal)
hihat vol = note $ PlayWav "samples/hihat.wav" ==> Amplifier (0.5*vol)

snare :: Double -> Notation (Play Signal)
snare vol = note $ PlayWav "samples/snare.wav" ==> Amplifier (0.25*vol)

--crash :: Double -> Notation (Play Signal)
--crash vol = note $ PlayWav "samples/crash.wav" ==> Amplifier (0.5*vol)
-------------------------------------------------------------------------------
beatloop :: Int -> Notation (Play Signal)
beatloop 1  = line' $ replicate 4 $ bassdrum 1
beatloop 2  = line' $ replicate 4 $ line' [bassdrum 1, hihat 1]
beatloop 3  = line' $ replicate 2 $ line' [bassdrum 1, hihat 1, bassdrum 1 :=: snare 1, hihat 1]
beatloop 4  = line' $ replicate 2 $ line' [bassdrum 1, bassdrum 1 :=: snare 1]
beatloop _  = rest
-------------------------------------------------------------------------------
-- Bass Synth
-------------------------------------------------------------------------------
bassloop :: Int -> Notation (Double, Double, Double, Double, Double)
--bassloop 1 = line' $ replicate 16 $ note (0,0.8,0.28,0.3,0.1)
bassloop 1 = line' $ replicate 16 $ note (0,1,0.3,0.4,0.2)
bassloop 2 = line' $ replicate 8 $ line' [ note (0, 1, 0.3, 0.4, 0.2)
                                         , note (12, 1, 0.2, 0.4, 0.4)
                                         ]
bassloop 3 = line' $ replicate 4 $ line' [ note (0, 1, 0.3, 0.4, 0.2)
                                         , note (0, 1, 0.3, 0.4, 0.2)
                                         , note (12, 1, 0.2, 0.4, 0.4)
                                         , note (0, 1, 0.3, 0.4, 0.2)
                                         ]
bassloop _ = rest
------------------------------------------------------------------------------
-- cutoff: 0.2 .. 0.4
-- slope:  1 .. 2
-- filterenv: 0 .. 0.5
-- duration: 1%8
synth1  :: (Sound pitch, Sound volume, Sound cutoff, Sound slope, Sound filterenv)
        => pitch -> volume -> cutoff -> slope -> filterenv -> Play Signal
synth1 pitch volume cutoff slope filterenv =
       pitch
   ==> ToFreq 12
   ==> Oscillator Saw 55.0
   ==> Filter
        (Lowpass (play slope)
                 ( (Envelope FitADR CosLike (1000,2000,0.5,1000) ==> Amplifier filterenv)
                   <+> cutoff))
   ==> Amplifier
            [ Interpolate Linear (0,1) <?> Abs 500
            , Interpolate Linear (1,0.9) <?> Abs 500
            , Interpolate CosLike (0.9,0.4) <?> Rel (1%4)
            , Interpolate Linear (0.4,0.2) <?> Flex 1
            , Interpolate CosLike (0.2,0) <?> Rel (1%4)
            ]
   ==> Amplifier volume
-------------------------------------------------------------------------------
samplebassline :: (Sound pitch, Sound volume, Sound cutoff) => pitch -> volume -> cutoff -> [(Double, Double) -> Notation (Play Signal)]
samplebassline pitch volume cf =
    map (\(x, len) -> \(p,v) ->
      note ( (pitch <+> p)
         ==> ToFreq 12
         ==> (\xs -> osc (cycle x)
                         (map (adjustFrequency (fromIntegral len) 55) xs))
         ==> Filter (Bandpass (0.3::Double) (0.3::Double) cf)
         ==> Amplifier (Envelope FitADR CosLike (1,3,0.2,2))
         ==> Amplifier (volume <*> v)
           )
        )
  $ filter (\(_, len) -> len > 0)
  $ map (\x -> (normalize x, length x))
  $ splitWaves
  $ signalToMono
  $ openWavSignal "samples/in.wav"

normalize :: [Double] -> [Double]
normalize xs = map (/v) xs
 where
  v = maximum $ map abs xs
-------------------------------------------------------------------------------
sortWaves :: Ord a => ([Mono] -> a) -> [[Mono]] -> [[Mono]]
sortWaves f = map fst
            . sortBy (\a b -> compare (snd a) (snd b))
        . map (\x -> (x, f x))

transSample1 :: ([[Mono]] -> [[Mono]]) -> [Mono]
transSample1 f  = concat
                $ f
                $ splitWaves
                $ signalToMono
                $ openWavSignal "samples/in.wav"

ts :: Int -> [Mono]
ts x  = transSample1 $ case x of
   1  -> sortWaves length
   2  -> sortWaves (\x -> sum (map abs x) / fromIntegral (length x + 1))
   3  -> sortWaves (\x -> negate $ sum (map abs x) / fromIntegral (length x + 1))
   4  -> map (\x -> x >>= \x -> [x,x])

   5  -> map (\x -> x ++ x)
   6  -> map sort
   7  -> map reverse
   8  -> let { loop [] = []
             ; loop xs = (take 10 xs) ++ loop (tail xs)
             } in loop
   9  -> let { loop [] = []
             ; loop xs = reverse (take 10 xs) ++ loop (tail xs)
             } in loop
   10 -> let { loop [] = []
             ; loop xs = sort (take 10 xs) ++ loop (tail xs)
             } in loop
   _  -> id
-------------------------------------------------------------------------------
hall :: Fractional a => Int -> [a] -> [a]
hall len inp = let buf  = replicate len 0.0 ++ outp
                   outp = zipWith3 (\x x' i -> x*0.5 + x'*0.4 + i*0.3) buf (tail buf)
                          (inp ++ replicate 10000 0.0)
               in outp

hallStereo :: Int -> [Stereo] -> [Stereo]
hallStereo = hall
-------------------------------------------------------------------------------
hallSynth :: (Sound input, Sound pitch, Sound volume) => pitch -> volume -> input -> Play Signal
hallSynth pitch volume input = do
 xs <- playMono (pitch ==> ToFreq 12)
 let pt = if null xs then 1 else max 1 (head xs)
     pt' = 44100 / (55.0 * pt)
 input ==> effect (hallStereo (round pt')) ==> Amplifier (Envelope FitS CosLike (200,1,1,1000)) ==> Amplifier volume

-------------------------------------------------------------------------------
--scratchSynth :: (Sound pitch, Sound volume) => pitch -> volume -> Play Signal
--scratchSynth pitch volume = pitch ==> ToFreq 12 ==> ScratchWav "samples/in.wav" ==> Amplifier volume
-------------------------------------------------------------------------------
beatpart :: String -> Notation (Play Signal)
beatpart "intro" = line $ map beatloop [0,0,0,0,0,0,0,0]
beatpart "h1" = line $ map beatloop [1,1,1,1,2,2,2,2]
beatpart "h2" = line $ map beatloop [4,4,4,4,3,3,3,3]
beatpart "h3" = line $ map beatloop [2,2,2,2,2,2,2,2]
beatpart "h4" = line $ map beatloop [4,4,4,4,4,4,4,4]
beatpart "outro" = line $ map beatloop [0,0,0,0,0,0,0,0]
beatpart _       = Rest 8

basspart "intro" = line $ map bassloop [1,1,2,2,3,3,2,3]
basspart "h1"    = line $ map bassloop [1,1,1,1,1,1,1,1]
basspart "h2"    = line $ map bassloop [1,1,1,1,1,1,1,1]
basspart "h3"    = line $ map bassloop [2,2,2,2,2,2,2,2]
basspart "h4"    = line $ map bassloop [3,3,3,3,3,3,3,3]
basspart "outro" = line $ map bassloop [1,1,2,2,3,3,2,3]
basspart _       = Rest 8

hallpart :: String -> Notation (Double, Double)
hallpart "intro" = line $ map hallloop [0,0,0,0,0,0,0,0]
hallpart "h1"    = line $ map hallloop [1,2,3,4,1,2,3,4]
hallpart "h2"    = line $ map hallloop [1,2,3,4,1,2,3,4]
hallpart "h3"    = line $ map hallloop [1,2,1,2,1,2,1,2]
hallpart "h4"    = line $ map hallloop [5,5,5,5,5,5,5,5]
hallpart "o1"    = line $ map hallloop [6,6,7,8,9,9,9,9]
hallpart "o2"    = line $ map hallloop [9,9,9,9,9,9,9,9]
hallpart "outro" = line $ map hallloop [10,0,0,0,0,0,0,0]
hallpart _       = Rest 8

samploop :: Int -> [(Double, Double)]
samploop 1 = replicate 16 (0,1)
samploop 2 = concat $ replicate 8 [(0,1), (12,1)]
samploop 3 = zip [0..15] (repeat 1)
samploop 4 = zip [0, (-1)..(-15)] (repeat 1)
samploop _ = replicate 16 (0,0)

samppart :: String -> [(Double, Double)]
samppart "intro" = concat $ map samploop [1,1,1,3,2,1,2,4]
samppart "h1"    = concat $ map samploop [1,1,1,1,1,1,1,1]
samppart "h2"    = concat $ map samploop [1,1,1,1,1,1,1,1]
samppart "h3"    = concat $ map samploop [1,1,1,1,1,1,1,3]
samppart "h4"    = concat $ map samploop [1,2,1,1,1,2,1,3]
samppart "outro" = concat $ map samploop [1,1,1,1,2,1,2,4]


hallloop :: Int -> Notation (Double, Double)
hallloop 1 = line $ map (\p -> Note (1%4) (p,1)) [0,12,0,12]
hallloop 2 = line $ map (\p -> Note (1%4) (p,1)) [4,16,4,16]
hallloop 3 = line $ map (\p -> Note (1%4) (p,1)) [7,19,7,19]
hallloop 4 = line $ map (\p -> Note (1%4) (p,1)) [0,12,0,12]
hallloop 5 = chord [ line $ map (\p -> Note (1%4) (p,1)) [0,12,0,12]
                   , line [ Note (1%2) (24,0.8), Note (1%2) (22,0.7) ]
                   ]
hallloop 6 = line' $ replicate 4 $ line' [ rest, chord [note (0,0.3), note (12,0.2), note (24,0.1)] ]
hallloop 7 = line' $ replicate 6 $ line' [ rest, chord [note (0,0.3), note (12,0.2), note (24,0.1)] ]
hallloop 8 = line' $ replicate 8 $ line' [ rest, chord [note (0,0.3), note (12,0.2), note (24,0.1)] ]
hallloop 9 = line' $ replicate 8 $ chord [note (0,0.3), note (12,0.2), note (24,0.1)]
hallloop 10 = line' [note (0,1), note (12,1), rest, rest]
hallloop _ = Rest 1

scratchpart 1 = line
          $ replicate 2
          $ line [ note (0::Double,1::Double)
                 , note (0::Double,1::Double)
                 , Note (1%2) (0::Double,1::Double)
                 , Note (3%2) (0::Double,1::Double)
                 ]
scratchpart _ = Rest 8
-------------------------------------------------------------------------------
song2 :: [Stereo]
song2 = map (*0.3)
      $ runSong 110 $ do
         input1 <- track $ notationMono $ line [ Rest 8, note $ play $ Mono $ cycle $ ts 8]
         -- input2 <- track $ notationMono $ line [ note $ play $ Mono $ cycle $ ts 0]
         cutoff1 <- track $ fmap cycle $ notationMono $ Note 2 $ play [ Interpolate Linear (0.2,0.6) <?> Rel 1
                                                                      , Interpolate Linear (0.6,0.2) <?> Rel 1
                                                                      ]
         volume1 <- track $ notationMono $ line [ Note 8 $ play [Interpolate Linear (-1,0) <?> Rel 1]
                                                , Rest 16
                                                , Note 8 $ play [Interpolate Linear (0,-1) <?> Rel 1]
                                                , Rest 16
                                                ]
         pitch <- track $ fmap cycle $ notationMono $ line [ note $ play [Interpolate Linear (0,0) <?> Rel 1]
                                                           , note $ play [Interpolate Linear (7,7) <?> Rel 1]
                                                           , note $ play [Interpolate Linear (0,0) <?> Rel 1]
                                                           , note $ play [Interpolate Linear (5,5) <?> Rel 1]
                                                           ]
         --pitch2 <- track $ fmap cycle $ notationMono $ line [ Note (1%32) $ play [Interpolate Linear (2,1) <?> Rel 1]
         --                                                   , Note (1%32) $ play [Interpolate Linear (1,0.8) <?> Rel 1]
         --                                                   , Note (1%32) $ play [Interpolate Linear (1,-1) <?> Rel 1]
         --                                                   , Note (1%32) $ play [Interpolate Linear (-1,-1.5) <?> Rel 1]
         --                                                   ]
         return
          $ notationStereo
          $ chord
          [
            -- Beats
            line $ map beatpart ["intro", "h1", "h2", "h3", "h4", "outro"]

            -- Bass
          , fmap (\(p,v,c,s,e) -> synth1 pitch v c s e)
          $ line $ map basspart ["intro", "h1", "h2", "h3", "h4", "outro"]

            -- SampleOsc
          , stretch (1%16)
          $ line
          $ zipWith ($)
            (cycle $ samplebassline ((24::Double) <+> pitch) ((1::Double) <+> (volume1 ==> Infinite 0)) cutoff1) --(0.25::Double)
          $ concat
          $ map samppart ["intro", "h1", "h2", "h3", "h4", "outro"]


            -- Hall
          , fmap (\(p,v) -> hallSynth p v input1)
          $ line
          $ map hallpart ["intro", "h1", "h2", "h3", "h3", "outro"]
{-
          , fmap (\(p,v) -> (do
                         i <- playTrack input2
                         --p ==> ToFreq 12 ==> AddTo
                         pitch2 ==> osc i ==> Amplifier (Envelope FitS Linear (200,1,1,200) ==> Amplifier (v <*> (0.8::Double)))))
          --(\(p,v) -> hallSynth p v input2)
          $ line
          $ replicate 100
          $ map scratchpart [1,0,0,1,0]
-}
          ]
-------------------------------------------------------------------------------
main :: IO ()
main = writeWavStereo "song2.wav" song2
