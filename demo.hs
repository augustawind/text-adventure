import TextAdventure

main :: IO ()
main = run myAdventure myGameState

-- Game state:
myGameState :: GameState
myGameState = defaultGameState

-- Adventure:
myAdventure :: Adventure
myAdventure =
    Node (Sequence [PrintLines intro
                   ,Prompt "name" "What is your name?"
                   ,Print "Hello, %(name)! Your adventure begins..."
                   ,Pause
                   ,HR
                   ,Print "Which direction will you go?"
                   ])
         $dispatcher$
             [("left",
                 Node (Print "You went left. You found the treasure! You win!")
                      gameOver)
             ,("right",
                 Node (Print "You went right. A giant boar gores you. You die.")
                      gameOver)
             ]

intro :: [String]
intro = ["You've decided to set out on an adventure."
        ,"You've left your house and taken the path to a crossroads."]
