// This Program runs a game of Ludo for up to 4 computer compettitors. Different strategies can be chosen for each CPU.

// Rules
// Players take turns rolling one six-sided die. Each player has four pieces that can move on a board with 40 fields in a plus-shaped arena:
// (Ref A: https://de.wikipedia.org/wiki/Mensch_%C3%A4rgere_Dich_nicht#/media/Datei:Mensch_%C3%A4rgere_dich_nicht_4.svg)
// The pieces start on the four home-fields (noted as "0" in the program) of the respective color and >have< to be set on the start-field when the player rolls a "6".
// The player's own start field >has< to be evacuated as soon as the play can do so!
// Stepping on a field currently occupied by an oposing players piece removes that piece (it is added to the player's home-field). If a player fails to remove an
// opposing piece (because he made a move that didn't remove an opponent's piece) the piece that could have removed the opposing piece is removed itself. (But he can consciously decide to do so)
// Each player has a target zone with four fields. When all four pieces have reached this zone, the player wins the game. It is not possible to pass
// pieces in the target zone. (passing pieces is possible on the outer fields)
// Additional rules:
// 1. When a player has no piece that can be moved he rolls the die up to 3 times (until he gets a "6").
// 2. It's not legal for a player to pass his own start-field (e.g. to kick out the final piece of an opponent).
// 3. When rolling a "6" the player gets another roll regardless if he could do a valid move.

// Definitions:
// House of all players is defined by number "0"
// Target zone of player 0 = 41 through 44
// Target zone of player 1 = 51 through 54
// Target zone of player 2 = 61 through 64
// Target zone of player 3 = 71 through 74






type Strategy =
    | Offensive // Rush win (remove opponents when possible)
    | Defensive // move last piece (remove opponents when possible)
    | Assassin // Stay behind opponents when possible (remobe opponents when possible)
    | Traitor // Sacrifice pieces to rush to victory (only remove opponents with leading piece)

type Player = { Name: string; Strategy: Strategy }


// Checks if start-position has to be evacuated
let evacuateStartPostion activePlayer (gameState: int array array) dieRoll =
    (Array.contains (activePlayer * 10 + 1) gameState.[activePlayer])
    && (Array.contains 0 gameState[activePlayer])
    && not (Array.contains (activePlayer * 10 + 1 + dieRoll) gameState[activePlayer])

let calculateMove strategy (gameState: int array array) activePlayer dieRoll =
    let activePlayerPosition = gameState.[activePlayer]
    let move = activePlayerPosition
    // Player has to evacute start-position if necessary
    if evacuateStartPostion activePlayer gameState dieRoll then
        let index =
            Array.findIndex (fun e -> e = activePlayer * 10 + 1) activePlayerPosition


        move.[index] <- move.[index] + dieRoll
        move
    else
        match strategy with
        | _ -> move







let main argv =
    // Init Game
    let numberOfPlayers = 4

    let players =
        [ { Name = "Max"; Strategy = Offensive }
          { Name = "Anna"; Strategy = Offensive }
          { Name = "Michael"
            Strategy = Offensive }
          { Name = "Phil"; Strategy = Offensive } ]

    let mutable gameState =
        [| [| 0; 0; 0; 0 |]; [| 0; 0; 0; 0 |]; [| 0; 0; 0; 0 |]; [| 0; 0; 0; 0 |] |]

    // Game Loop
    let mutable notWon = true
    let randomNumberGenerator = System.Random()
    let mutable dieRoll = 0
    let mutable activePlayer = 3
    let mutable playerMove = [| 0; 0; 0; 0 |]
    let mutable dieCast = 3

    let nextTurn =
        // Player gets another turn if he rolls a "6" or has no pieces in the game and has not yet rolled 3 times.
        if
            dieRoll = 6
            || (Array.forall (fun e -> e = 0) gameState.[activePlayer] && dieCast < 3)
        then
            dieCast <- dieCast + 1
        else
            activePlayer <- (activePlayer + 1) % numberOfPlayers
            dieCast <- 0

    while notWon do
        nextTurn
        // 1 roll dice
        dieRoll <- randomNumberGenerator.Next(1, 6)
        dieCast <- dieCast + 1
        // 2 player calculates move
        playerMove <- calculateMove players[activePlayer].Strategy gameState activePlayer dieRoll
        // 3 check player move
        // Wrong move -> "Error message"
        // Correct move -> validate move and update game state
        gameState.[activePlayer] <- playerMove
        // 4 Check if game is over
        notWon <- not (Array.exists (fun e -> Array.forall (fun a -> a > 40) e) gameState)



    let winnerIndex =
        Array.findIndex (fun e -> Array.forall (fun a -> a > 40) e) gameState

    printfn "The winner is: %s" players[winnerIndex].Name
    0
