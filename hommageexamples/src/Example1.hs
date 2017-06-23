module Example1 where

import Prelude hiding ((<*>))
import Sound.Hommage
import Data.List
import Data.Ratio

main1 = writeWavMono "out1.wav"
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main2 = writeWavMono "out2.wav"
      $ concat
      $ sortWaves length
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main3 = writeWavMono "out3.wav"
      $ concat
      $ sortWaves (\x -> sum (map abs x) / fromIntegral (length x + 1))
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main4 = writeWavMono "out4.wav"
      $ concat
      $ sortWaves (\x -> negate $ sum (map abs x) / fromIntegral (length x + 1))
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"

sortWaves :: Ord a => ([Mono] -> a) -> [[Mono]] -> [[Mono]]
sortWaves f = map fst
            . sortBy (\a b -> compare (snd a) (snd b))
            . map (\x -> (x, f x))


main5 = writeWavMono "out5.wav"
      $ concat
      $ map (\x -> x >>= \x -> [x,x])
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main6 = writeWavMono "out6.wav"
      $ concat
      $ map (\x -> x ++ x)
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main7 = writeWavMono "out7.wav"
      $ concat
      $ map sort
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main8 = writeWavMono "out8.wav"
      $ concat
      $ map reverse
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main9 = writeWavMono "out9.wav"
      $ concat
      $ let { loop [] = []
            ; loop xs = (take 10 xs) ++ loop (tail xs)
            } in loop
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main10 = writeWavMono "out10.wav"
      $ concat
      $ let { loop [] = []
            ; loop xs = reverse (take 10 xs) ++ loop (tail xs)
            } in loop
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main11 = writeWavMono "out11.wav"
      $ concat
      $ let { loop [] = []
            ; loop xs = sort (take 10 xs) ++ loop (tail xs)
            } in loop
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main12 = writeWavMono "out12.wav"
      $ runSong 110
      $ return
      $ notationMono
      $ stretch (1%16)
      $ line
      $ map (\x -> note ( osc (cycle x) (repeat 1)
                          ==> Amplifier (Envelope FitADR CosLike (1,3,0.5,2)) ))
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main13 = writeWavMono "out13.wav"
      $ runSong 140
      $ return
      $ notationMono
      $ stretch (1%16)
      $ line
      $ map (\x -> note ( osc (cycle x) (repeat 0.25)
                          ==> Amplifier (Envelope FitADR CosLike (1,3,0.5,2)) ))
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main14 = writeWavMono "out14.wav"
      $ runSong 120
      $ return
      $ notationMono
      $ stretch (1%16)
      $ line
      $ map (\(x, len) -> note ( osc (cycle x) (repeat (adjustFrequency (fromIntegral len) 120 1))
                                 ==> Amplifier (Envelope FitADR CosLike (1,3,0.5,2)) ))
      $ filter (\(x, len) -> len > 0)
      $ map (\x -> (x, length x))
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main15 = writeWavMono "out15.wav"
      $ runSong 120
      $ return
      $ notationMono
      $ stretch (1%16)
      $ line
      $ map (\(x, len) -> note ( osc (cycle x) (repeat (adjustFrequency (fromIntegral len) 120 1))
                                 ==> Amplifier (Envelope FitADR CosLike (1,3,0.5,2)) ))
      $ filter (\(x, len) -> len > 0)
      $ map (\x -> (normalize x, length x))
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"

normalize xs = map (/v) xs
 where
  v = maximum $ map abs xs


main16 = writeWavMono "out16.wav"
      $ runSong 120
      $ return
      $ notationMono
      $ stretch (1%16)
      $ line
      $ map (\(x, len) -> note ( osc (cycle x) (repeat (adjustFrequency (fromIntegral len) 120 1))
                                 ==> Filter (Lowpass (0.6::Double) (0.35::Double) )
                                 ==> Amplifier (Envelope FitADR CosLike (1,3,0.5,2)) ))
      $ filter (\(x, len) -> len > 0)
      $ map (\x -> (normalize x, length x))
      $ splitWaves
      $ signalToMono
      $ openWavSignal "samples/in.wav"


main17 = writeWavMono "out17.wav"
      $ runSong 120 $ do
         cutoff <- track $ play $ map ((+0.2).(*0.28).(+1)) $ sinus $ repeat 0.003
         return
          $ notationMono
          $ stretch (1%16)
          $ line
          $ map (\(x, len) -> note ( osc (cycle x) (repeat (adjustFrequency (fromIntegral len) 120 1))
                                     ==> Filter (Lowpass (0.8::Double) cutoff)
                                     ==> Amplifier (Envelope FitADR CosLike (1,3,0.5,2)) ))
          $ filter (\(x, len) -> len > 0)
          $ map (\x -> (normalize x, length x))
          $ splitWaves
          $ signalToMono
          $ openWavSignal "samples/in.wav"


main18 = writeWavMono "out18.wav"
      $ map (*0.8)
      $ runSong 120 $ do
         cutoff <- track $ play $ map ((+0.2).(*0.28).(+1)) $ sinus $ repeat 0.003
         return
          $ notationMono
          $ chord
          [ line $ replicate 64 $ Note (1%4) (PlayWav "samples/bassdrum.wav" ==> Amplifier (0.7::Double))
          , stretch (1%16)
          $ line
          $ take 256
          $ map (\(x, len) -> note ( osc (cycle x) (repeat (adjustFrequency (fromIntegral len) 120 1))
                                     ==> Filter (Lowpass (0.8::Double) cutoff)
                                     ==> Amplifier (Envelope FitADR CosLike (1,3,0.5,2)) ))
          $ filter (\(x, len) -> len > 0)
          $ map (\x -> (normalize x, length x))
          $ splitWaves
          $ signalToMono
          $ openWavSignal "samples/in.wav"
          ]


main19 = writeWavMono "out19.wav"
      $ map (*0.65)
      $ runSong 120 $ do
         cutoff  <- track $ play $ map ((+0.2).(*0.28).(+1)) $ sinus $ repeat 0.003
         sample1 <- track $ play $ cycle $ signalToMono $ openWavSignal "out6.wav"
         return
          $ notationMono
          $ chord
          [ line $ replicate 128 $ Note (1%4) (PlayWav "samples/bassdrum.wav" ==> Amplifier (0.7::Double))
          , line $ replicate 256 (Note (1%16) (sample1 ==> Amplifier (Envelope FitADR Linear (1,2,0.5,1) ==> Amplifier (0.8::Double)))
                                 :+: Rest (1%16))
          , stretch (1%16)
          $ line
          $ take 512
          $ map (\(x, len) -> note ( osc (cycle x) (repeat (adjustFrequency (fromIntegral len) 120 1))
                                     ==> Filter (Lowpass (0.8::Double) cutoff)
                                     ==> Amplifier (Envelope FitADR CosLike (1,3,0.5,2)) ))
          $ filter (\(x, len) -> len > 0)
          $ map (\x -> (normalize x, length x))
          $ splitWaves
          $ signalToMono
          $ openWavSignal "samples/in.wav"
          ]


main20a = writeWavMono "out20a.wav"
      $ map (*0.65)
      $ runSong 120 $ do
         cutoff  <- track $ play $ map ((+0.2).(*0.28).(+1)) $ sinus $ repeat 0.003
         sample1 <- track $ play $ cycle $ signalToMono $ openWavSignal "out6.wav"
         volume  <- track $ notationMono $ line [Rest 4, Note 4 $ play $ Interpolate Linear (0,1::Double), Note 24 $ play (1::Double)]
         return
          $ notationMono
          $ chord
          [ line $ map beats ([0,1,2,3] >>= replicate 8)
          , line $ replicate 256 (Note (1%16) (sample1 ==> Amplifier (Envelope FitADR Linear (1,2,0.5,1) ==> Amplifier (volume <*> (0.8::Double))))
                                 :+: Rest (1%16))
          , line $ map samples [0,3,0,3,1,0,2,3]
          , stretch (1%16)
          $ line
          $ take 512
          $ map (\(x, len) -> note ( osc (cycle x) (repeat (adjustFrequency (fromIntegral len) 120 1))
                                     ==> Filter (Lowpass (0.6::Double) cutoff)
                                     ==> Amplifier (Envelope FitADR CosLike (1,3,0.5,2)) ))
          $ filter (\(x, len) -> len > 0)
          $ map (\x -> (normalize x, length x))
          $ splitWaves
          $ signalToMono
          $ openWavSignal "samples/in.wav"
          ]


main20 = writeWavMono "out20.wav"
      $ map (*0.6)
      $ runSong 120 $ do
         cutoff  <- track $ play $ map ((+0.2).(*0.28).(+1)) $ sinus $ repeat 0.001
         sample1 <- track $ play $ cycle $ signalToMono $ openWavSignal "out6.wav"
         sample2 <- track $ play $ cycle $ signalToMono $ openWavSignal "out11.wav"
         volume  <- track $ notationMono $ line [Rest 4, Note 12 $ play $ Interpolate CosLike (0,1::Double), Note 48 $ play (1::Double)]
         return
          $ notationMono
          $ chord
          [ line $ map beats ([1,0,1,2,3,3,2,1] >>= replicate 8)
          , line $ replicate 512 (Note (1%16) (sample1 ==> Amplifier (Envelope FitADR Linear (1,2,0.5,1) ==> Amplifier (volume <*> (0.8::Double))))
                                 :+: Rest (1%16))
          , line $ map samples [0,0,0,3,1,0,2,3,4,3,1,0,2,0,1,3,1]
          , fmap (sample2player sample2) $ line (Rest 32 : (replicate 128 $ line [ Rest (1%8), Note (1%8) (0.5::Double) ]))
          , stretch (1%16)
          $ line
          $ take 1024
          $ map (\(x, len) -> note ( osc (cycle x) (repeat (adjustFrequency (fromIntegral len) 120 1))
                                     ==> Filter (Lowpass (0.6::Double) cutoff)
                                     ==> Amplifier (Envelope FitADR CosLike (1,3,0.5,2)) ))
          $ filter (\(x, len) -> len > 0)
          $ map (\x -> (normalize x, length x))
          $ splitWaves
          $ signalToMono
          $ openWavSignal "samples/in.wav"
          ]

beats 0 = rest
beats 1 = line' $ replicate 4 $ note (PlayWav "samples/bassdrum.wav" ==> Amplifier (0.7::Double))
beats 2 = chord [ beats 1
                , line' $ replicate 4 $ line' [rest, note (PlayWav "samples/hihat.wav" ==> Amplifier (0.4::Double))]
                ]
beats 3 = chord [ beats 2
                , line' $ replicate 2 $ line' [rest, note (PlayWav "samples/snare.wav" ==> Amplifier (0.36::Double))]
                ]

samples 0 = stretch 4 rest
samples 1 = stretch 4 $ note $ play $ PlayWav "out4.wav"
samples 2 = stretch 4 $ note (PlayWav "out2.wav" ==> Amplifier (0.8::Double))
samples 3 = line [Rest 3, Rest (1%3), Note (2%3) $ play $ PlayWav "out3.wav"]
samples 4 = Note 4 $ play $ PlayWav "out5.wav"

sample2player sample vol = sample ==> Amplifier (Envelope FitADR Linear (1,2,0.5,1)) ==> Amplifier vol

main = do
 main1
 main2
 main3
 main4
 main5
 main6
 main7
 main8
 main9
 main10
 main11
 main12
 main13
 main14
 main15
 main16
 main17
 main18
 main19
 main20a
 main20


