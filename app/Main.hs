module Main where

import System.Process

import Data.Foldable
import Text.Printf

import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB

type Semitones = Float 
type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Beats = Float

outputFilePath :: FilePath
outputFilePath = "output.bin"

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beats
bpm = 120.0

-- Length of a beat is based on target beats per minute
beatDuration :: Seconds
beatDuration = 60.0 / bpm

-- step :: Beats --  ?
-- step =  (hz * 2 * pi) / sampleRate

main = do
  save $ (wave <> (rest 1) <> reverse wave) <> (rest 1) <> wave
  runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath -- trick

-- multiply
volume = 0.5

-- generate sampleRate samples per second
-- wave :: [Float]
-- wave = concat [freq 440.0 1.0, freq 540.0 1.0] -- concat together different notes

-- wave :: [Pulse]
-- wave = concat [ note (2*i) duration | i <- [0..10]]
--   where duration = 0.5
waveWithRests :: [Pulse]
waveWithRests =
  intercalate (rest 0.2)
    [ note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.5
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.5
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.5
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.5
    , note (-2) 0.5
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.5
    ]



wave :: [Pulse]
wave =
  concat
    [ note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.5
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.5
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.5
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.5
    , note (-2) 0.5
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.5
    ]

wave2 = (note 0 3.5) <> wave <> (concat [rest 3]) <> wave

waveScale :: [Pulse]
waveScale = concat [ note 0 duration
              , note 2 duration
              , note 4 duration
              , note 5 duration
              , note 7 duration
              , note 9 duration
              , note 11 duration
              , note 12 duration 
              ]
  where
    duration = 0.5


-------------------------------------------------------------  
-- will sound like one long note 
nonBrokenNoteWave :: [Pulse]
nonBrokenNoteWave = concat [ note 0 5.5
                           , note 0 0.25
                           , note 0 0.25
                           , note 0 0.25
                           , note 0 0.25
                           , note 0 0.5
                           
                           , note 5 0.25
                           , note 5 0.25
                           , note 5 0.25
                           , note 5 0.25
                           , note 5 0.25
                           , note 5 0.25
                           , note 5 0.25
                           , note 5 0.5                          
                           ]
  where
    duration = 0.5


-- attack and decay:

--  /\___
-- /     \

-- note should look like this otherwise its choppy 
-------------------------------------------------------------

{-
   we should change duration to Beats Per Minute 
-}



  
freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
  where
    step =  (hz * 2 * pi) / sampleRate
    
    attack :: [Pulse]
    attack = map (min 1.0) [0.0, 0.001 ..]

    release :: [Pulse]
    release = reverse $ take (length output) attack 
    
    output :: [Pulse]
    output = map sin $ map (*step) [0.0 .. sampleRate * duration] 

        -- this is the difference between notes I believe 

-- Semitones represent number of half tones away from pitch standard
note :: Semitones -> Beats -> [Pulse]
note numSemitones beats = freq (f numSemitones) (beats * beatDuration)

rest :: Beats -> [Pulse]
rest beats = map (*0) [0.0 .. sampleRate * (beats * beatDuration)]
  -- where
  --   step = ((f 0) * 2 * pi) / sampleRate 

restWave :: [Pulse]
restWave = concat [ note 0 3.5
                  , rest 2
                  , note 0 2
                  ]

f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0/12.0)) ** n
-- formula here is just based on how to relate a target note to pitch standard (basically)
-- https://pages.mtu.edu/~suits/NoteFreqCalcs.html
{-
   for pitch standard (440 hz) we must do 440 cycles of sin(x) which ranges from 0 -> 2*pi
-}
        
save :: [Pulse] -> IO ()
save wave = LB.writeFile outputFilePath $  B.toLazyByteString $ fold $  Prelude.map B.floatLE wave

