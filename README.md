# Hommage

#### Haskell Offline Music Manipulation And Generation EDSL

Hommage is a textual "digital audio workstation". It was written in 2006 and combines ideas from the musical notation of the Haskore library and the possibility to use lazy lists as representation of audio data.

    # go to the hommageexamples directory
    cd hommageexamples
    
    # build the examples and the hommage library
    stack build
    
    # go to the run directory, because there's the samples folder used by the examples
    cd run
    
    # run the examples program from there like this (some parts of the path might be different):
    ../.stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/hommageexamples/hommageexamples
    
#### One of the examples:

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

