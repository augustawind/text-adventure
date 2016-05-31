import TextAdventure

main :: IO ()
main = run myAdventure myGameState

-- Game state:
myGameState :: GameState
myGameState = defaultGameState { getLineChar = '~' }

-- Adventure:
myAdventure :: Adventure
myAdventure =
    Node [PrintLines ["You've decided to set out on an adventure."
                     ,"You've left your house and taken the path to a crossroads."]
         ,Prompt "name" [] "What is your name?"
         ,Print "Hello, %(name)! Your adventure begins..."
         ,Pause
         ,HR
         ,Print "Which direction will you go?"]
    $dispatcher$
        [("left",
           Node [PrintLines ["You went left."
                            ,"You see a strange object."
                            ,"Do you pick it up?"]]
           $dispatcher$
               [("yes",
                   Node [Print ("You pick it up and instantly " ++
                                "attain enlightenment. You win!")]
                   gameOver
                )
               ,("no",
                   Node [Print ("Your skepticism precedes you. " ++
                                "You turn to leave the object behind " ++
                                "and you see a treasure chest apparate out of thin " ++
                                "air. Do you wish to open it?")]
                   $dispatcher$
                       [("yes",
                           Node [Print "You found the treasure! You win!"] gameOver)

                       ,("no",
                           Node [PrintLines ["You have denied too many opportunities!"
                                            ,"You are being punished for your ungratefulness."
                                            ,"You lose!"]]
                           gameOver
                        )]
                )]
         )
        ,("right",
           Node [Print "You went right."
                ,Prompt "lastWords" ["help", "fuck"] "Any last words?"
                ,PrintLines ["A giant boar gores you."
                            ,"Before you die, you say \"%(lastWords)\"."
                            ,"You lose!"]]
           gameOver
         )]
