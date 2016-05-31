import TextAdventure

main :: IO ()
main = run myAdventure myGameState

-- Game state:
myGameState :: GameState
myGameState = defaultGameState

-- Adventure:
myAdventure :: Adventure
myAdventure =
    Node [PrintLines intro
         ,Prompt "name" "What is your name?"
         ,Print "Hello, %(name)! Your adventure begins..."
         ,Pause
         ,HR
         ,Print "Which direction will you go?"
         ]
         $dispatcher$
             [("left", wentLeft)
             ,("right", wentRight)
             ]

intro :: [String]
intro = ["You've decided to set out on an adventure."
        ,"You've left your house and taken the path to a crossroads."]

wentLeft :: Adventure
wentLeft = Node [PrintLines ["You went left."
                            ,"You see a strange object."
                            ,"Do you pick it up?"]]
                $dispatcher$
                    [("yes", Node [Print ("You pick it up and instantly " ++
                        "attain enlightenment. You win!")]
                        gameOver
                     )
                    ,("no", Node [Print ("Your skepticism precedes you. " ++
                        "You turn to leave the object behind " ++
                        "and you see a treasure chest apparate out of thin " ++
                        "air. Do you wish to open it?")]
                        $dispatcher$
                            [("yes", openedChest)
                            ,("no", didntOpenChest)
                            ]
                     )
                    ]

wentRight :: Adventure
wentRight = Node [PrintLines ["You went right."
                             ,"A giant boar gores you."
                             ,"You lose!"
                             ]]
                 gameOver

openedChest :: Adventure
openedChest = Node [Print "You found the treasure! You win!"] gameOver

didntOpenChest :: Adventure
didntOpenChest = Node [PrintLines ["You have denied too many opportunities!"
                                  ,"You are being punished for your ungratefulness."
                                  ,"You lose!"
                                  ]]
                      gameOver
