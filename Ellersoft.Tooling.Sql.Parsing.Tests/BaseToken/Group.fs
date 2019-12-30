module BaseToken.Group

open Ellersoft.Tooling.Sql.Parsing
open Ellersoft.Tooling.Sql.Parsing.Objects
open Xunit

[<Fact>]
let ``Classify and group simple CREATE TABLE script`` () =
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
        [| BaseToken.Grouping (Open Bracket) |]
        [| BaseToken.Letter (Lower LD)
           BaseToken.Letter (Lower LB)
           BaseToken.Letter (Lower LO) |]
        [| BaseToken.Grouping (Close Bracket) |]
        [| BaseToken.Separator Period |]
        [| BaseToken.Grouping (Open Bracket) |]
        [| BaseToken.Letter (Upper LT)
           BaseToken.Letter (Lower LE)
           BaseToken.Letter (Lower LS)
           BaseToken.Letter (Lower LT) |]
        [| BaseToken.Grouping (Close Bracket) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Grouping (Open Parenthesis) |]
        [| BaseToken.Grouping (Open Bracket) |]
        [| BaseToken.Letter (Upper LI)
           BaseToken.Letter (Lower LD) |]
        [| BaseToken.Grouping (Close Bracket) |]
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
        [| BaseToken.Grouping (Open Bracket) |]
        [| BaseToken.Letter (Upper LP)
           BaseToken.Letter (Upper LK)
           BaseToken.Symbol '_'
           BaseToken.Letter (Upper LT)
           BaseToken.Letter (Lower LE)
           BaseToken.Letter (Lower LS)
           BaseToken.Letter (Lower LT) |]
        [| BaseToken.Grouping (Close Bracket) |]
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
        [| BaseToken.Grouping (Open Bracket) |]
        [| BaseToken.Letter (Upper LI)
           BaseToken.Letter (Lower LD) |]
        [| BaseToken.Grouping (Close Bracket) |]
        [| BaseToken.Grouping (Close Parenthesis) |]
        [| BaseToken.Grouping (Close Parenthesis) |]    
    |]
    let input = "CREATE TABLE [dbo].[Test] ([Id] INT NOT NULL IDENTITY(1, 1), CONSTRAINT [PK_Test] PRIMARY KEY ([Id]))"
    let actual = input |> BaseToken.classify |> BaseToken.group :> BaseToken [] seq
    Assert.Equal(expected, actual)

[<Fact>]
let ``Classify and group simple declaration with single-quote value`` () =
    let expected = [|
        [| BaseToken.Letter (Upper LD)
           BaseToken.Letter (Upper LE)
           BaseToken.Letter (Upper LC)
           BaseToken.Letter (Upper LL)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LR)
           BaseToken.Letter (Upper LE) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Symbol '@'
           BaseToken.Letter (Upper LT)
           BaseToken.Letter (Lower LE)
           BaseToken.Letter (Lower LS)
           BaseToken.Letter (Lower LT) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Letter (Upper LV)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LR)
           BaseToken.Letter (Upper LC)
           BaseToken.Letter (Upper LH)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LR) |]
        [| BaseToken.Grouping (Open Parenthesis) |]
        [| BaseToken.Number N2
           BaseToken.Number N0 |]
        [| BaseToken.Grouping (Close Parenthesis) |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Symbol '=' |]
        [| BaseToken.Separator Whitespace |]
        [| BaseToken.Special SingleQuote |]
        [| BaseToken.Letter (Upper LT)
           BaseToken.Letter (Lower LE)
           BaseToken.Letter (Lower LS)
           BaseToken.Letter (Lower LT) |]
        [| BaseToken.Special SingleQuote |]
    |]
    let input = "DECLARE @Test VARCHAR(20) = 'Test'"
    let actual = input |> BaseToken.classify |> BaseToken.group :> BaseToken [] seq
    Assert.Equal(expected, actual)
