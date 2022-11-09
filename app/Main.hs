module Main where

import System.Process

import Data.Foldable
import Text.Printf

import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB

import Graphics.Matplotlib

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
bpm = 97.0 ---120.0

volume :: Float
volume = 0.25

testPlot = onscreen $ contourF (\a b -> sin (a*pi/180.0) + cos (b*pi/180.0)) (-100) 100 (-200) 200 10
testPlot2 = onscreen $ line1  $ ([(0,0),(1,1),(2,1)] :: [(Int,Int)]) -- $ zip ([1..] :: [Int]) $ note 6 eigth
testPlot3 = onscreen $ line ([1.0, 1.1 .. 10.0] :: [Float]) (map sin [1.0, 1.1 ..10] :: [Float]) 

plotNote pitch noteType = onscreen $ line ([0.0 .. sampleRate * noteType * beatDuration]) (note pitch noteType)

-- Length of a beat is based on target beats per minute
beatDuration :: Seconds
beatDuration = 60.0 / bpm

--------------------------------------------------------------------
--- CONSTANTS

-- fix this weirdness 
whole = 1.5

half = whole / 2 
qrt = whole / 4 
eigth = whole / 8 
sixt = whole / 16 

dot :: Fractional a => a -> a 
dot x = 1.5 * x

--double dot
ddot :: Fractional a => a -> a 
ddot x = x + (x/2) + (x/4)

--------------------------------------------------------------------

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)
-- step :: Beats --  ?
-- step =  (hz * 2 * pi) / sampleRate

main = do
  --save $ concat [note 0 qrt, note 0 qrt, note 0 qrt, note 0 qrt]
  --save $ interRest [ note 6 half, rest 1, note 5 qrt, rest qrt, note 8 qrt, note 4 half] --welcomeToTheBlackParade
  -- save welcomeToTheBlackParade
  save wttbp2
  --save $ (wave <> (rest 1) <> reverse wave) <> (rest 1) <> wave
  runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath -- trick

-- multiply


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

-- sampleRate * noteType * beatDuration -> number of points 

  
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

welcomeToTheBlackParade = interRest [ wttbp1, wttbp2, wttbp3, wttbp4, wttbp5_6, wttbp7, wttbp8_9_10, wttbp11, wttbp12_13_14, wttbp15, wttbp16, wttbp17, wttbp18,
                                      wttbp19, wttbp20, wttbp21, wttbp22 
                                    ]


-- Rests and timing will also need to line up for all instruments / channels that receive PCM signals
interRest :: [[Pulse]] -> [Pulse]
interRest xs = intercalate (rest qrt) xs


-- each grouping is a bar? or group walls thingy in sheet music

-- TODO(galen): Make this actually 4/4 time not just weird duration choices
wttbp1 :: [Pulse] 
wttbp1 = interRest [ interRest [ note 6 half, rest 1, note 5 qrt, rest qrt, note 8 qrt],
                     interRest [ note 4 half, rest half, note 3 qrt, rest qrt, note 6 qrt ],
                     interRest [ pat636qrt', pat535qrt' ],
                     interRest [ pat636qrt', pat6543qrt ]                   
                   ]

-- wttbp1 :: [Pulse]
-- wttbp1 = 
--   concat $ concat [[  note 6 1 
--           , rest 1 
--           , note 5 0.5
--           , rest 0.5
--           , note 8 0.5
--           ],
--           [ rest 0.5
--           , note 4 1     
--           , rest 1
--           , note 3 0.5
--           , rest 0.5
--           , note 6 0.5
--           ],
--           [ rest 0.5
--           , note 2 1
--           , rest 0.25
--           , note 5 (0.5 * 1.5) -- dot 
--           , rest 0.25
--           , note 5 0.25 
--           , rest 0.25 
--           ],
--           [ note 6 (0.5 * 1.5)
--           , note 3 (0.5 * 1.5)
--           , note 6 0.5
--           , rest 0.25 
--           , note 5 (0.5 * 1.5)
--           , note 3 (0.5 * 1.5)
--           , note 5 0.5
--           ], 
--           [ rest 0.25 
--           , note 6 (0.5 * 1.5)
--           , note 3 (0.5 * 1.5)
--           , note 5 (0.5 * 1.5)
--           , rest 0.25 
--           , note 6 0.5
--           , note 5 0.5
--           , note 4 0.5
--           , note 3 0.5
--           ]
--           ]

wttbp2 :: [Pulse]
wttbp2 = interRest [ interRest [ note 4 sixt, note 2 qrt, pat56543qrt' ],
                     interRest [ note 2 half, note 3 (dot qrt), pat45qrt ],
                     interRest [ pat636qrt', pat535qrt' ],
                     interRest [ pat636qrt', pat6543qrt ],
                     interRest [ note 4 sixt, note 2 qrt, pat56543qrt']
                   ]

-- wttbp2 :: [Pulse]
-- wttbp2 = concat $ concat [[ rest 0.25 
--                          , note 4 (0.25 / 2) 
--                          , rest 0.25 
--                          , note 2 0.5
--                          , rest 0.25
--                          , note 5 0.5
--                          , note 6 (0.5 * 1.5)
--                          , note 5 0.5
--                          , note 4 0.5 
--                          , note 3 0.5 
--                          ], 
--                          [ rest 0.25 
--                          , note 2 1
--                          , rest 0.25
--                          , note 3 (0.5 * 1.5)
--                          , rest 0.25
--                          , note 4 0.5 
--                          , note 5 0.5 
--                          , rest 0.25 
--                          ],
--                          [ note 6 (0.5 * 1.5), note 3 (0.5 * 1.5), note 6 0.5
--                          , rest 0.25
--                          , note 5 (0.5 * 1.5), note 3 (0.5 * 1.5), note 5 0.5 
--                          ],
--                          [ rest 0.25
--                          , note 6 (0.5 * 1.5), note 3 (0.5 * 1.5), note 5 0.5
--                          , rest 0.25 
--                          , note 6 0.5, note 5 0.5, note 4 0.5, note 3 0.5
--                          ], 
--                          [ rest 0.25
--                          , note 4 (0.25 / 4)
--                          , rest 0.25
--                          , note 2 0.5
--                          , rest 0.25
--                          , note 5 0.5, note 6 (0.5 * 1.5), note 5 0.5, note 4 0.5, note 3 0.5 
--                          ]
--                          ]

wttbp3 :: [Pulse]
wttbp3 = interRest [ interRest [ note 2 half, note 3 (dot qrt), pat45qrt],
                     interRest [ pat636qrt', pat535qrt' ],
                     interRest [ pat636qrt', pat6543qrt ],
                     interRest [ note 4 sixt, note 2 qrt, pat56543qrt' ],
                     interRest [ note 2 half, note 3 (dot qrt), pat45qrt ]
                   ]

-- wttbp3 :: [Pulse]
-- wttbp3 = concat $ concat [[ rest 0.25 
--                           , note 2 1 
--                           , rest 0.25
--                           , note 3 (0.5 * 1.5)
--                           , rest 0.25
--                           , note 4 0.5, note 5 0.5 
--                           , rest 0.25
--                           ],
--                           concat [ pat636qrt' --note 6 (0.5 * 1.5), note 3 (0.5 * 1.5), note 6 0.5
--                                  , [rest 0.25] 
--                                  , pat535qrt' 
--                                  ],
--                           concat [ pat636qrt' 
--                                  , [rest 0.25]
--                                  , pat6543qrt 
--                                  ],
--                           concat [ concat [ note 4 (0.5 / 4)
--                                    , rest 0.25 
--                                    , note 2 0.5
--                                    ],
--                                    pat56543qrt'
--                                    ],
--                           concat [ concat [ note 2 1 
--                                    , rest 0.25 
--                                    , note 3 (0.5 * 1.5)
--                                    ]
--                                    , pat45qrt 
--                                    ] 
--                           ]

-- I also wonder if I could do some trick for the curves from one note to the next ( ( rotated cclockwise )
  -- ** Called a tie and just extends a note (i think past the bar for time signature)

-- Perhaps I could manipulate per bar for changing time signatures?

-- If we are using interRest for in the bars, we must use them (i think) for bars concatted 
wttbp4 :: [Pulse]
wttbp4 = interRest [ interRest [ pat636qrt', pat535qrt' ] -- all single bars
                   , interRest [ pat636qrt', pat6543qrt ] 
                   , interRest [ note 4 sixt, note 2 qrt, pat56543qrt' ]
                   , interRest [ note 2 half, note 3 half ]
                   , interRest [ note (-1) qrt, patN_21qrt, note 0 qrt, patN1014 ]
                   ]


tie :: [Pulse] -> [Pulse] -> [Pulse]
tie p p' = p <> p 

-- basically just opposite of connect with rest 
ties :: [[Pulse]] -> [Pulse]
ties xs = intercalate [] xs 

-- a `tie` b 

wttbp5_6 = wttbp5 `tie` wttbp6 

-- fmap (\(x,y) -> note x y)

wttbp5 :: [Pulse]
wttbp5 = interRest [ interRest [ pat31qrt, note 2 (dot qrt), note 4 (eigth), note 3 qrt]
                   , interRest [ note 1 eigth, note 2 (dot qrt), note 1 eigth, note 0 qrt, note (-1) eigth ]
                   , ties [interRest [ note 3 (dot qrt), pat00qrt', note (-1) qrt, pat00qrt ], 
                           interRest [ note 0 eigth, note (-1) qrt, pat00qrt', note (-1) 0.5, note 0 eigth]
                          ] 
                   , interRest [ pat10N1N2N1qrt, note (-3) qrt, pat01qrt ] 
                   ]

-- wttbp5 and 6 must be put together specially for the loop 
wttbp6 :: [Pulse]
wttbp6 = ties [ interRest [ pat10N1N2qrt, note (-4) (dot qrt), rest sixt ],
                interRest [ note 0 eigth, note (-1) qrt, pat00qrt', note (-1) qrt, pat00qrt],
                interRest [ note 0 eigth, note (-1) qrt, note 0 eigth, pat10N1N2N1qrt ],
                interRest [ note (-1) sixt, note (-3) qrt, pat010N1N2N4qrt ],
                interRest [ note (-4) (dot half), note (-4) qrt]
              ]
                  
wttbp7 :: [Pulse]
wttbp7 = interRest [ interRest [ patN_545454_qrt', rest eigth, note (-4) eigth ],
                     interRest [ patN_3334_qrt, note (-7) (dot qrt), rest sixt, note (-4) eigth],
                     ties [interRest [ patN_544345567_qrt ],
                           interRest [ note (-7) (ddot qrt), rest sixt, rest qrt, rest eigth, note (-4) eigth ]
                         ]
                   ]                        

wttbp8 :: [Pulse]
wttbp8 = interRest [ interRest [ patN_11141_0N10qrt' ],
                     interRest [ patN10N10N1qrt', rest eigth, rest eigth, note 0 eigth],
                     interRest [ patN1001qrt', pat0N_123qrt ],
                     interRest [ note (-4) eigth, rest eigth, rest eigth, note (-1) eigth, note 1 qrt, patN_21qrt ]
                   ] 


wttbp8_9_10 = wttbp8 `tie` wttbp9 `tie` wttbp10

wttbp9 :: [Pulse]
wttbp9 = interRest [ interRest [ note (-1) eigth, rest eigth, rest eigth, note (-1) eigth, note 1 qrt, patN_212qrt ], 
                     interRest [ note (-4) eigth, rest eigth, rest qrt, note 1 qrt, note 0 qrt],
                     interRest [ patN10N10N10qrt'', rest sixt, note 0 eigth ],
                     interRest [ patN10N10qrt', note 1 qrt, patN_21qrt ]
                   ]

wttbp10 :: [Pulse]
wttbp10 = interRest [ interRest [ note (-1) eigth, rest eigth, rest eigth, note (-1) eigth, note 1 qrt, patN_212qrt ],
                      interRest [ note (-4) qrt, rest eigth, rest qrt, note 1 qrt, note 0 qrt], 
                      interRest [ patN10N10N10qrt'', rest sixt, note 0 eigth ],
                      interRest [ patN10N10N10qrt'', rest sixt, note (-4) eigth ]
                    ]

-- 48 
wttbp11 :: [Pulse]
wttbp11 = interRest [ interRest [ patN_545454_qrt', rest eigth, note (-4) eigth], 
                      interRest [ patN_3334_qrt, note (-7) (dot qrt), rest sixt, note (-4) eigth],
                      ties [interRest [ patN_544345567_qrt ], interRest [ note (-7) (ddot qrt), rest sixt, rest qrt, rest eigth, note (-4) eigth]]
                    ]

wttbp12_13_14 = wttbp12 `tie` wttbp13 `tie` wttbp14

wttbp12 :: [Pulse]
wttbp12 = interRest [ interRest [ patN_11141_0N10qrt' ],
                      interRest [ patN10N10N1qrt', rest eigth, rest eigth, note 0 eigth],
                      interRest [ patN1001qrt', pat0N_123qrt ],
                      interRest [ note (-4) eigth, rest eigth, rest eigth, note (-1) eigth, note 1 qrt, patN2N1qrt ]
                    ]

wttbp13 :: [Pulse] 
wttbp13 = interRest [ interRest [ note (-1) qrt, rest eigth, rest eigth, note (-1) eigth, note 1 qrt, patN_212qrt ],
                      interRest [ note (-4) eigth, rest eigth, rest qrt, note 1 qrt, note 0 qrt],
                      interRest [ patN10N10N10qrt'', rest sixt, note 0 eigth],
                      interRest [ patN10N10qrt', note 1 qrt, patN2N1qrt ]
                    ]

wttbp14 :: [Pulse]
wttbp14 = interRest [ interRest [ note (-1) eigth, rest eigth, rest eigth, note (-1) eigth, note 1 qrt, patN_212qrt ],
                      interRest [ note (-4) eigth, rest eigth, rest qrt, note 1 qrt, note 0 qrt],
                      interRest [ patN10N10N10qrt'', rest sixt, note 0 eigth],
                      interRest [ patN10N1N110qrt'', rest sixt, rest eigth]
                    ]

wttbp15 :: [Pulse]
wttbp15 = interRest [ interRest [ pat1212qrt', pat10N1N2 ], 
                      interRest [ note (-3) eigth, rest eigth, note (-1) qrt, note (-2) qrt, note (-3) qrt ],
                      interRest [ pat1212qrt', pat10N1N2 ],
                      interRest [ note (-3) eigth, rest eigth, note (-1) qrt, note (-2) qrt, note (-3) qrt ]                               
                    ]

wttbp16 :: [Pulse]
wttbp16 = interRest [ interRest [ pat1212qrt', note 1 qrt, patN_11qrt],
                      interRest [ note 2 qrt, note 1 qrt, note 0 half],
                      interRest [ rest eigth, patN101qrt, rest qrt, patN101N1qrt ],
                      interRest [ rest eigth, patN101qrt, rest qrt, patN_11_01N1qrt ]
                    ]

-- 72 of pdf
wttbp17 :: [Pulse]
wttbp17 = interRest [ interRest [ rest eigth, patN101qrt, rest qrt, patN_11_01N1qrt],
                      interRest [ rest eigth, patN101qrt, rest qrt, patN_11_01N1qrt],
                      interRest [ rest eigth, patN101qrt, rest qrt, patN101N1qrt ],
                      interRest [ rest eigth, patN101qrt, rest qrt, patN_11_01N1qrt]
                    ]

--76
wttbp18 :: [Pulse]
wttbp18 = interRest [ interRest [ rest eigth, patN101qrt, rest qrt, patN_11_01N1qrt],
                      interRest [ rest eigth, patN101qrt, rest qrt, patN_11_01N1qrt],
                      interRest [ rest eigth, pat678qrt, rest qrt, (pat678qrt <> (note 6 qrt))],
                      interRest [ rest eigth, pat678qrt, rest qrt, pat66786qrt ]
                    ]


wttbp19 :: [Pulse]
wttbp19 = interRest [ interRest [ rest eigth, pat678qrt, rest qrt, pat66786qrt],
                      interRest [ note 3 qrt, note 5 qrt, note 6 qrt, rest eigth, note (-1) qrt],
                      interRest [ note 1 qrt, note (-2) eigth, note (-1) qrt, rest qrt, note (-1) eigth],
                      interRest [ note 1 qrt, patN_212qrt, note (-4) eigth, rest eigth, rest qrt]
                    ]


wttbp20 :: [Pulse]
wttbp20 = interRest [ ties [interRest [ note 1 qrt, note 0 qrt, patN10N10N1qrt ],
                            interRest [ patN10qrt', rest sixt, note 0 eigth, patN10N10qrt' ]],
                      interRest [ note 1 qrt, note (-2) eigth, note (-1) qrt, rest qrt, note (-1) eigth],
                      interRest [ note 1 qrt, patN_212qrt, note (-4) eigth, rest eigth, rest qrt]
                    ]

wttbp21 :: [Pulse]
wttbp21 = interRest [ ties [interRest [ note 1 qrt, note 0 qrt, patN10N10N1qrt], interRest [patN10qrt', rest sixt, note 0 eigth, patN10N10qrt']],
                            interRest [ note 1 (dot qrt), note 0 (dot qrt), rest qrt ],
                            interRest [ rest eigth, patN101qrt, rest qrt, patN101N1qrt]
                    ]                       


wttbp22 :: [Pulse]
wttbp22 = interRest [ interRest [ rest eigth, patN101qrt, rest qrt, patN_11_01N1qrt],
                      interRest [ rest eigth, patN101qrt, rest qrt, patN_11_01N1qrt],
                      interRest [ rest eigth, patN101qrt, rest qrt, patN_11_0qrt, rest qrt],
                      interRest [ note 1 qrt, note 0 qrt, note 3 half ]
                    ]                            









--------------------------------------------------------------------
---PATTERNS of the form: patxyza.. which are connected by bars and used often
  -- When ' at end: has internal modifications like duration multiplication

  -- N_xyz_ means all xyz are negative



pat31qrt = concat [ note 3 qrt, note 1 qrt ]

pat10N1N2qrt = concat [ note 1 qrt, note 0 qrt, note (-1) qrt, note (-2) qrt ]

pat10N1N2N1qrt = concat [ note 1 qrt, note 0 qrt, note (-1) qrt, note (-2) qrt, note (-1) qrt ]

pat010N1N2N4qrt = concat [ note 0 qrt, note 1 qrt, note 0 qrt, note (-1) qrt, note (-2) qrt, note (-4) qrt ]

patN1001qrt' = concat [ note (-1) qrt, note 0 qrt, note 0 (dot qrt), note 1 qrt ]

patN_11_0qrt = concat [ note (-1) qrt, note (-1) qrt, note 0 qrt ]

patN10qrt' = concat [ note (-1) qrt, note 0 (dot qrt) ]

pat66786qrt = concat [ note 6 qrt, note 6 qrt, note 7 qrt, note 8 qrt, note 6 qrt ]

pat678qrt = concat [ note 6 qrt, note 7 qrt, note 8 qrt ]

patN_11_01N1qrt = concat [ note (-1) qrt, note (-1) qrt, note 0 qrt, note 1 qrt, note (-1) qrt ]

patN101N1qrt = concat [ note (-1) qrt, note 0 qrt, note 1 qrt, note (-1) qrt]

patN101qrt = concat [ note (-1) qrt, note 0 qrt, note 1 qrt ]

patN_11qrt = concat [ note (-1) qrt, note (-1) qrt]

patN10N1N110qrt'' = concat [ note (-1) qrt, note 0 qrt, note (-1) qrt, note 0 qrt, note 1 (dot qrt), note 0 (dot qrt)]

patN10N10qrt' = concat [ note (-1) qrt, note 0 qrt, note (-1) qrt, note 0 (dot qrt)] 

pat1212qrt' = concat [ note 1 qrt, note 2 (dot qrt), note 1 qrt, note 2 qrt ]

patN2N1qrt = concat [ note (-2) qrt, note (-1) qrt ]

pat636qrt' = concat [ note 6 (0.5 * 1.5), note 3 (0.5 * 1.5), note 6 0.5 ]

pat535qrt' = concat [ note 5 (0.5 * 1.5), note 3 (0.5 * 1.5), note 6 0.5 ]

pat6543qrt = concat [ note 6 0.5, note 5 0.5, note 4 0.5, note 3 0.5 ]

pat56543qrt' = concat [ note 5 0.5, note 6 (0.5 * 1.5), note 5 0.5, note 4 0.5, note 3 0.5 ]

pat45qrt = concat [ note 4 0.5, note 5 0.5 ]

patN1014 = concat [ note (-1) 0.5, note 0 0.5, note 1 0.5, note 4 0.5 ]

pat00qrt' = concat [ note 0 qrt, note 0 (dot qrt) ]
pat00qrt = concat [ note 0 qrt, note 0 qrt ]

pat10N1N2N1 = concat [ note 1 qrt, note 0 qrt, note (-1) qrt, note (-2) qrt, note (-1) qrt ]

pat01qrt = concat [ note 0 qrt, note 1 qrt ]

pat10N1N2 = concat [ note 1 qrt, note 0 qrt, note (-1) qrt, note (-2) qrt ]

pat010N1N2N4 = concat [ note 0 qrt, note 1 qrt, note 0 qrt, note (-1) qrt, note (-2) qrt, note (-4) qrt ]

patN10N10N1qrt' = concat [ note (-1) qrt, note 0 qrt, note (-1) qrt, note 0 qrt, note (-1) (dot qrt) ]

patN10N10N1qrt = concat [ note (-1) qrt, note 0 qrt, note (-1) qrt, note 0 qrt, note (-1) qrt ]

patN_11141_0N10qrt' = concat [ note (-1) qrt, note (-1) qrt, note (-1) qrt, note (-4) qrt, note (-1) (dot qrt), note 0 qrt, note (-1) qrt, note 0 qrt ]

patN10N10N10qrt'' = concat [ note (-1) qrt, note 0 qrt, note (-1) qrt, note 0 qrt, note (-1) (dot qrt), note 0 (dot qrt)]

--- ALL negative 
patN_21qrt = concat [ note (-2) qrt, note (-1) qrt ]

patN_545454_qrt' = concat [ note (-5) qrt, note (-4) qrt, note (-5) qrt, note (-4) qrt, note (-5) qrt, note (-4) (dot qrt) ]

patN_3334_qrt = concat [ note (-3) qrt, note (-3) qrt, note (-3) qrt, note (-4) qrt ]

patN100N1qrt' = concat [ note (-1) qrt, note 0 qrt, note 0 (dot qrt), note 1 qrt ]

pat0N_123qrt = concat [ note 0 qrt, note (-1) qrt, note (-2) qrt, note (-3) qrt ]

patN_212qrt = concat [ note (-2) qrt, note (-1) qrt, note (-2) qrt ]

patN_544345567_qrt = concat [ note (-5) qrt, note (-4) qrt, note (-4) qrt, note (-3) qrt, note (-4) qrt, note (-5) qrt, note (-5) qrt, note (-6) qrt, note (-7) qrt ]

--------------------------------------------------------------------
--- GROUPS 

-- FORMAT: pat_pat , eg grp636_535


--grp636_535' = interRest [ pat636qrt', pat535qrt' ]

