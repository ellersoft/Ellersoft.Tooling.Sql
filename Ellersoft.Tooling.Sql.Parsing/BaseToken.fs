namespace Ellersoft.Tooling.Sql.Parsing

open Ellersoft.Tooling.Sql.Parsing.Objects

module BaseToken =
    let classify (input : string) =
        let mapCharType (char : char) =
            match char with
            | '0' -> Number N0
            | '1' -> Number N1
            | '2' -> Number N2
            | '3' -> Number N3
            | '4' -> Number N4
            | '5' -> Number N5
            | '6' -> Number N6
            | '7' -> Number N7
            | '8' -> Number N8
            | '9' -> Number N9
            | '(' -> Grouping (Open Parenthesis)
            | ')' -> Grouping (Close Parenthesis)
            | '[' -> Grouping (Open Bracket)
            | ']' -> Grouping (Close Bracket)
            | '{' -> Grouping (Open Brace)
            | '}' -> Grouping (Close Brace)
            | ' ' | '\r' | '\n' | '\t' -> BaseToken.Separator Whitespace
            | ',' -> BaseToken.Separator Comma
            | '.' -> BaseToken.Separator Period
            | 'a' -> Letter (Lower LA)
            | 'b' -> Letter (Lower LB)
            | 'c' -> Letter (Lower LC)
            | 'd' -> Letter (Lower LD)
            | 'e' -> Letter (Lower LE)
            | 'f' -> Letter (Lower LF)
            | 'g' -> Letter (Lower LG)
            | 'h' -> Letter (Lower LH)
            | 'i' -> Letter (Lower LI)
            | 'j' -> Letter (Lower LJ)
            | 'k' -> Letter (Lower LK)
            | 'l' -> Letter (Lower LL)
            | 'm' -> Letter (Lower LM)
            | 'n' -> Letter (Lower LN)
            | 'o' -> Letter (Lower LO)
            | 'p' -> Letter (Lower LP)
            | 'q' -> Letter (Lower LQ)
            | 'r' -> Letter (Lower LR)
            | 's' -> Letter (Lower LS)
            | 't' -> Letter (Lower LT)
            | 'u' -> Letter (Lower LU)
            | 'v' -> Letter (Lower LV)
            | 'w' -> Letter (Lower LW)
            | 'x' -> Letter (Lower LX)
            | 'y' -> Letter (Lower LY)
            | 'z' -> Letter (Lower LZ)
            | 'A' -> Letter (Upper LA)
            | 'B' -> Letter (Upper LB)
            | 'C' -> Letter (Upper LC)
            | 'D' -> Letter (Upper LD)
            | 'E' -> Letter (Upper LE)
            | 'F' -> Letter (Upper LF)
            | 'G' -> Letter (Upper LG)
            | 'H' -> Letter (Upper LH)
            | 'I' -> Letter (Upper LI)
            | 'J' -> Letter (Upper LJ)
            | 'K' -> Letter (Upper LK)
            | 'L' -> Letter (Upper LL)
            | 'M' -> Letter (Upper LM)
            | 'N' -> Letter (Upper LN)
            | 'O' -> Letter (Upper LO)
            | 'P' -> Letter (Upper LP)
            | 'Q' -> Letter (Upper LQ)
            | 'R' -> Letter (Upper LR)
            | 'S' -> Letter (Upper LS)
            | 'T' -> Letter (Upper LT)
            | 'U' -> Letter (Upper LU)
            | 'V' -> Letter (Upper LV)
            | 'W' -> Letter (Upper LW)
            | 'X' -> Letter (Upper LX)
            | 'Y' -> Letter (Upper LY)
            | 'Z' -> Letter (Upper LZ)
            | c -> BaseToken.Symbol c
        input.ToCharArray() |> Array.map mapCharType
        
    let group (tokens : BaseToken []) =
        let compatibleToken (a : BaseToken) (b : BaseToken) =
            match a, b with
            | Letter _, Letter _ -> true
            | Number _, Number _ -> true
            | Symbol _, Symbol _ -> true
            | BaseToken.Separator _, BaseToken.Separator _ -> true
            | Letter _, Number _ -> true
            | Number _, Letter _ -> true
            | Letter _, Symbol _ -> true
            | Symbol _, Letter _ -> true
            | Number _, Symbol _ -> true
            | Symbol _, Number _ -> true
            | _ -> false
        tokens
        |> Array.fold (fun acc token ->
            match acc with
            | [|  |] -> [| [| token |] |]
            | acc ->
                if acc.[acc.Length - 1].[0] |> compatibleToken token then
                    acc.[acc.Length - 1] <- [|token|] |> Array.append (acc.[acc.Length - 1])
                    acc
                else
                    [| [| token |] |] |> Array.append acc
        ) [|  |]
