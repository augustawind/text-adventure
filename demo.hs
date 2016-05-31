import TextAdventure

main :: IO ()
main = run myAdventure myGameState

-- Game state:
myGameState :: GameState
myGameState = defaultGameState { getLineChar = '~' }

-- Custom game over.
gameOver :: Nexus
gameOver = EndGame "Game over!"

-- Adventure:
myAdventure :: Adventure
myAdventure =
    Node [PrintLines ["You've decided to set out on an adventure."
                     ,"You've left your house and taken the path to a crossroads."]
         ,Prompt "name" [] "What is your name?"
         ,Print "Hello, %(name)! Your adventure begins..."
         ,Pause
         ,HR]

    $ ask "Which direction will you go?" $

        [("left",
           Node [PrintLines ["You went left."
                            ,"You see a strange object."]]

           $ ask "Do you pick it up?" $

               [("yes",
                   Node [Print ("You pick it up and instantly " ++
                                "attain enlightenment. You win!")]
                   gameOver
                )
               ,("no",
                   Node [Print ("Your skepticism precedes you. " ++
                                "You turn to leave the object behind " ++
                                "and you see a treasure chest apparate out of thin air.")]

                   $ ask "Do you wish to open it?" $

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
