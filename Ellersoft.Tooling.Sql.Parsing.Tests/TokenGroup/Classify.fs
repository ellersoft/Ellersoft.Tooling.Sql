module TokenGroup.Classify

open Ellersoft.Tooling.Sql.Parsing
open Ellersoft.Tooling.Sql.Parsing.Objects
open Xunit

[<Fact>]
let ``Classify groups for simple CREATE TABLE script`` () =
    let expected = [|
        TokenGroup.String ([|
            BaseToken.Letter (Upper LC)
            BaseToken.Letter (Upper LR)
            BaseToken.Letter (Upper LE)
            BaseToken.Letter (Upper LA)
            BaseToken.Letter (Upper LT)
            BaseToken.Letter (Upper LE) |])
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.String ([|
            BaseToken.Letter (Upper LT)
            BaseToken.Letter (Upper LA)
            BaseToken.Letter (Upper LB)
            BaseToken.Letter (Upper LL)
            BaseToken.Letter (Upper LE) |])
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.Group (OpenCloseQuoteGrouping.Open Bracket)
        TokenGroup.String ([|
           BaseToken.Letter (Lower LD)
           BaseToken.Letter (Lower LB)
           BaseToken.Letter (Lower LO) |])
        TokenGroup.Group (OpenCloseQuoteGrouping.Close Bracket)
        TokenGroup.Separation [| BaseToken.Separator Period |]
        TokenGroup.Group (OpenCloseQuoteGrouping.Open Bracket)
        TokenGroup.String ([|
            BaseToken.Letter (Upper LT)
            BaseToken.Letter (Lower LE)
            BaseToken.Letter (Lower LS)
            BaseToken.Letter (Lower LT) |])
        TokenGroup.Group (OpenCloseQuoteGrouping.Close Bracket)
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.Group (OpenCloseQuoteGrouping.Open Parenthesis)
        TokenGroup.Group (OpenCloseQuoteGrouping.Open Bracket)
        TokenGroup.String ([|
            BaseToken.Letter (Upper LI)
            BaseToken.Letter (Lower LD) |])
        TokenGroup.Group (OpenCloseQuoteGrouping.Close Bracket)
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.String ([|
            BaseToken.Letter (Upper LI)
            BaseToken.Letter (Upper LN)
            BaseToken.Letter (Upper LT) |])
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.String ([|
            BaseToken.Letter (Upper LN)
            BaseToken.Letter (Upper LO)
            BaseToken.Letter (Upper LT) |])
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.String ([|
            BaseToken.Letter (Upper LN)
            BaseToken.Letter (Upper LU)
            BaseToken.Letter (Upper LL)
            BaseToken.Letter (Upper LL) |])
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.String ([|
            BaseToken.Letter (Upper LI)
            BaseToken.Letter (Upper LD)
            BaseToken.Letter (Upper LE)
            BaseToken.Letter (Upper LN)
            BaseToken.Letter (Upper LT)
            BaseToken.Letter (Upper LI)
            BaseToken.Letter (Upper LT)
            BaseToken.Letter (Upper LY) |])
        TokenGroup.Group (OpenCloseQuoteGrouping.Open Parenthesis)
        TokenGroup.String [| BaseToken.Number N1 |]
        TokenGroup.Separation [|
            BaseToken.Separator Comma
            BaseToken.Separator Whitespace |]
        TokenGroup.String [| BaseToken.Number N1 |]
        TokenGroup.Group (OpenCloseQuoteGrouping.Close Parenthesis)
        TokenGroup.Separation [|
            BaseToken.Separator Comma
            BaseToken.Separator Whitespace |]
        TokenGroup.String ([|
            BaseToken.Letter (Upper LC)
            BaseToken.Letter (Upper LO)
            BaseToken.Letter (Upper LN)
            BaseToken.Letter (Upper LS)
            BaseToken.Letter (Upper LT)
            BaseToken.Letter (Upper LR)
            BaseToken.Letter (Upper LA)
            BaseToken.Letter (Upper LI)
            BaseToken.Letter (Upper LN)
            BaseToken.Letter (Upper LT) |])
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.Group (OpenCloseQuoteGrouping.Open Bracket)
        TokenGroup.String ([|
            BaseToken.Letter (Upper LP)
            BaseToken.Letter (Upper LK)
            BaseToken.Symbol '_'
            BaseToken.Letter (Upper LT)
            BaseToken.Letter (Lower LE)
            BaseToken.Letter (Lower LS)
            BaseToken.Letter (Lower LT) |])
        TokenGroup.Group (OpenCloseQuoteGrouping.Close Bracket)
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.String ([|
            BaseToken.Letter (Upper LP)
            BaseToken.Letter (Upper LR)
            BaseToken.Letter (Upper LI)
            BaseToken.Letter (Upper LM)
            BaseToken.Letter (Upper LA)
            BaseToken.Letter (Upper LR)
            BaseToken.Letter (Upper LY) |])
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.String ([|
            BaseToken.Letter (Upper LK)
            BaseToken.Letter (Upper LE)
            BaseToken.Letter (Upper LY) |])
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.Group (OpenCloseQuoteGrouping.Open Parenthesis)
        TokenGroup.Group (OpenCloseQuoteGrouping.Open Bracket)
        TokenGroup.String ([|
            BaseToken.Letter (Upper LI)
            BaseToken.Letter (Lower LD) |])
        TokenGroup.Group (OpenCloseQuoteGrouping.Close Bracket)
        TokenGroup.Group (OpenCloseQuoteGrouping.Close Parenthesis)
        TokenGroup.Group (OpenCloseQuoteGrouping.Close Parenthesis)    
    |]
    let input = "CREATE TABLE [dbo].[Test] ([Id] INT NOT NULL IDENTITY(1, 1), CONSTRAINT [PK_Test] PRIMARY KEY ([Id]))"
    let actual = input |> BaseToken.classify |> BaseToken.group |> TokenGroup.classify :> TokenGroup seq
    Assert.Equal(expected, actual)

[<Fact>]
let ``Classify and group simple declaration with single-quote value`` () =
    let expected = [|
        TokenGroup.String [|
           BaseToken.Letter (Upper LD)
           BaseToken.Letter (Upper LE)
           BaseToken.Letter (Upper LC)
           BaseToken.Letter (Upper LL)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LR)
           BaseToken.Letter (Upper LE) |]
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.String [|
           BaseToken.Symbol '@'
           BaseToken.Letter (Upper LT)
           BaseToken.Letter (Lower LE)
           BaseToken.Letter (Lower LS)
           BaseToken.Letter (Lower LT) |]
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.String [|
           BaseToken.Letter (Upper LV)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LR)
           BaseToken.Letter (Upper LC)
           BaseToken.Letter (Upper LH)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LR) |]
        TokenGroup.Group (OpenCloseQuoteGrouping.Open Parenthesis)
        TokenGroup.String [|
           BaseToken.Number N2
           BaseToken.Number N0 |]
        TokenGroup.Group (OpenCloseQuoteGrouping.Close Parenthesis)
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.Other [| BaseToken.Symbol '=' |]
        TokenGroup.Separation [| BaseToken.Separator Whitespace |]
        TokenGroup.Group (OpenCloseQuoteGrouping.Quote Quotes.SingleQuote)
        TokenGroup.String [|
           BaseToken.Letter (Upper LT)
           BaseToken.Letter (Lower LE)
           BaseToken.Letter (Lower LS)
           BaseToken.Letter (Lower LT) |]
        TokenGroup.Group (OpenCloseQuoteGrouping.Quote Quotes.SingleQuote)
    |]
    let input = "DECLARE @Test VARCHAR(20) = 'Test'"
    let actual = input |> BaseToken.classify |> BaseToken.group |> TokenGroup.classify :> TokenGroup seq
    Assert.Equal(expected, actual)
