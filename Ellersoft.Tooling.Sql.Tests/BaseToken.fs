module BaseToken

open Ellersoft.Tooling.Sql.Parsing
open Ellersoft.Tooling.Sql.Parsing.Objects
open Xunit

[<Fact>]
let ``Tokenize simple CREATE TABLE script into base character tokens`` () =
    let expected = [|
        BaseToken.Letter (Upper LC)
        BaseToken.Letter (Upper LR)
        BaseToken.Letter (Upper LE)
        BaseToken.Letter (Upper LA)
        BaseToken.Letter (Upper LT)
        BaseToken.Letter (Upper LE)
        BaseToken.Separator Whitespace
        BaseToken.Letter (Upper LT)
        BaseToken.Letter (Upper LA)
        BaseToken.Letter (Upper LB)
        BaseToken.Letter (Upper LL)
        BaseToken.Letter (Upper LE)
        BaseToken.Separator Whitespace
        BaseToken.Letter (Upper LT)
        BaseToken.Letter (Lower LE)
        BaseToken.Letter (Lower LS)
        BaseToken.Letter (Lower LT)
        BaseToken.Separator Whitespace
        BaseToken.Grouping (Open Parenthesis)
        BaseToken.Letter (Upper LI)
        BaseToken.Letter (Lower LD)
        BaseToken.Separator Whitespace
        BaseToken.Letter (Upper LI)
        BaseToken.Letter (Upper LN)
        BaseToken.Letter (Upper LT)
        BaseToken.Separator Whitespace
        BaseToken.Letter (Upper LN)
        BaseToken.Letter (Upper LO)
        BaseToken.Letter (Upper LT)
        BaseToken.Separator Whitespace
        BaseToken.Letter (Upper LN)
        BaseToken.Letter (Upper LU)
        BaseToken.Letter (Upper LL)
        BaseToken.Letter (Upper LL)
        BaseToken.Separator Whitespace
        BaseToken.Letter (Upper LI)
        BaseToken.Letter (Upper LD)
        BaseToken.Letter (Upper LE)
        BaseToken.Letter (Upper LN)
        BaseToken.Letter (Upper LT)
        BaseToken.Letter (Upper LI)
        BaseToken.Letter (Upper LT)
        BaseToken.Letter (Upper LY)
        BaseToken.Grouping (Open Parenthesis)
        BaseToken.Number N1
        BaseToken.Separator Comma
        BaseToken.Separator Whitespace
        BaseToken.Number N1
        BaseToken.Grouping (Close Parenthesis)
        BaseToken.Separator Comma
        BaseToken.Separator Whitespace
        BaseToken.Letter (Upper LC)
        BaseToken.Letter (Upper LO)
        BaseToken.Letter (Upper LN)
        BaseToken.Letter (Upper LS)
        BaseToken.Letter (Upper LT)
        BaseToken.Letter (Upper LR)
        BaseToken.Letter (Upper LA)
        BaseToken.Letter (Upper LI)
        BaseToken.Letter (Upper LN)
        BaseToken.Letter (Upper LT)
        BaseToken.Separator Whitespace
        BaseToken.Letter (Upper LP)
        BaseToken.Letter (Upper LK)
        BaseToken.Symbol '_'
        BaseToken.Letter (Upper LT)
        BaseToken.Letter (Lower LE)
        BaseToken.Letter (Lower LS)
        BaseToken.Letter (Lower LT)
        BaseToken.Separator Whitespace
        BaseToken.Letter (Upper LP)
        BaseToken.Letter (Upper LR)
        BaseToken.Letter (Upper LI)
        BaseToken.Letter (Upper LM)
        BaseToken.Letter (Upper LA)
        BaseToken.Letter (Upper LR)
        BaseToken.Letter (Upper LY)
        BaseToken.Separator Whitespace
        BaseToken.Letter (Upper LK)
        BaseToken.Letter (Upper LE)
        BaseToken.Letter (Upper LY)
        BaseToken.Separator Whitespace
        BaseToken.Grouping (Open Parenthesis)
        BaseToken.Letter (Upper LI)
        BaseToken.Letter (Lower LD)
        BaseToken.Grouping (Close Parenthesis)
        BaseToken.Grouping (Close Parenthesis)    
    |]
    let input = "CREATE TABLE Test (Id INT NOT NULL IDENTITY(1, 1), CONSTRAINT PK_Test PRIMARY KEY (Id))"
    let actual = input |> BaseToken.classify :> BaseToken seq
    Assert.Equal(expected, actual)

[<Fact>]
let ``Tokenize and group simple CREATE TABLE script`` () =
    let expected = [|
        [| BaseToken.Letter (Upper LC)
           BaseToken.Letter (Upper LR)
           BaseToken.Letter (Upper LE)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LT)
           BaseToken.Letter (Upper LE) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LT)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LB)
           BaseToken.Letter (Upper LL)
           BaseToken.Letter (Upper LE) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LT)
           BaseToken.Letter (Lower LE)
           BaseToken.Letter (Lower LS)
           BaseToken.Letter (Lower LT) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Grouping (Open Parenthesis) |]
        [| BaseToken.Letter (Upper LI)
           BaseToken.Letter (Lower LD) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LI)
           BaseToken.Letter (Upper LN)
           BaseToken.Letter (Upper LT) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LN)
           BaseToken.Letter (Upper LO)
           BaseToken.Letter (Upper LT) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LN)
           BaseToken.Letter (Upper LU)
           BaseToken.Letter (Upper LL)
           BaseToken.Letter (Upper LL) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LI)
           BaseToken.Letter (Upper LD)
           BaseToken.Letter (Upper LE)
           BaseToken.Letter (Upper LN)
           BaseToken.Letter (Upper LT)
           BaseToken.Letter (Upper LI)
           BaseToken.Letter (Upper LT)
           BaseToken.Letter (Upper LY) |]
        [| BaseToken.Grouping (Open Parenthesis) |]
        [| BaseToken.Number N1 |]
        [| BaseToken.Separator Comma
           BaseToken.Separator Whitespace |]
        [| BaseToken.Number N1 |]
        [| BaseToken.Grouping (Close Parenthesis) |]
        [| BaseToken.Separator Comma
           BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LC)
           BaseToken.Letter (Upper LO)
           BaseToken.Letter (Upper LN)
           BaseToken.Letter (Upper LS)
           BaseToken.Letter (Upper LT)
           BaseToken.Letter (Upper LR)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LI)
           BaseToken.Letter (Upper LN)
           BaseToken.Letter (Upper LT) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LP)
           BaseToken.Letter (Upper LK)
           BaseToken.Symbol '_'
           BaseToken.Letter (Upper LT)
           BaseToken.Letter (Lower LE)
           BaseToken.Letter (Lower LS)
           BaseToken.Letter (Lower LT) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LP)
           BaseToken.Letter (Upper LR)
           BaseToken.Letter (Upper LI)
           BaseToken.Letter (Upper LM)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LR)
           BaseToken.Letter (Upper LY) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LK)
           BaseToken.Letter (Upper LE)
           BaseToken.Letter (Upper LY) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Grouping (Open Parenthesis) |]
        [| BaseToken.Letter (Upper LI)
           BaseToken.Letter (Lower LD) |]
        [| BaseToken.Grouping (Close Parenthesis) |]
        [| BaseToken.Grouping (Close Parenthesis) |]    
    |]
    let input = "CREATE TABLE Test (Id INT NOT NULL IDENTITY(1, 1), CONSTRAINT PK_Test PRIMARY KEY (Id))"
    let actual = input |> BaseToken.classify |> BaseToken.group :> BaseToken [] seq
    Assert.Equal(expected, actual)

//[<Fact>]
//let ``Parse simple CREATE TABLE script (ID, PK Constraint)`` () =
//    let expected = Script.Create "TABLE Test (Id INT NOT NULL IDENTITY(1, 1), CONSTRAINT PK_Test PRIMARY KEY (Id))"
//    let input = "CREATE TABLE Test (Id INT NOT NULL IDENTITY(1, 1), CONSTRAINT PK_Test PRIMARY KEY (Id))"
//    let actual = input |> Parser.parse
//    Assert.StrictEqual(expected, actual)
