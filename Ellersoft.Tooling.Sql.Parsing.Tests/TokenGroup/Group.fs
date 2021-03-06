﻿module TokenGroup.Group

open Ellersoft.Tooling.Sql.Parsing
open Ellersoft.Tooling.Sql.Parsing.Objects
open Xunit

[<Fact>]
let ``Classify and group structures for simple CREATE TABLE script`` () =
    let expected = [|
        StructuredTokenGroup.String ([|
            BaseToken.Letter (Upper LC)
            BaseToken.Letter (Upper LR)
            BaseToken.Letter (Upper LE)
            BaseToken.Letter (Upper LA)
            BaseToken.Letter (Upper LT)
            BaseToken.Letter (Upper LE) |])
        StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
        StructuredTokenGroup.String ([|
            BaseToken.Letter (Upper LT)
            BaseToken.Letter (Upper LA)
            BaseToken.Letter (Upper LB)
            BaseToken.Letter (Upper LL)
            BaseToken.Letter (Upper LE) |])
        StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
        StructuredTokenGroup.Group (QuoteGrouping.Grouping Bracket, [|
            StructuredTokenGroup.String ([|
               BaseToken.Letter (Lower LD)
               BaseToken.Letter (Lower LB)
               BaseToken.Letter (Lower LO) |])
        |])
        StructuredTokenGroup.Separation [| BaseToken.Separator Period |]
        StructuredTokenGroup.Group (QuoteGrouping.Grouping Bracket, [|
            StructuredTokenGroup.String ([|
                BaseToken.Letter (Upper LT)
                BaseToken.Letter (Lower LE)
                BaseToken.Letter (Lower LS)
                BaseToken.Letter (Lower LT) |])
        |])
        StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
        StructuredTokenGroup.Group (QuoteGrouping.Grouping Parenthesis, [|
            StructuredTokenGroup.Group (QuoteGrouping.Grouping Bracket, [|
                StructuredTokenGroup.String ([|
                    BaseToken.Letter (Upper LI)
                    BaseToken.Letter (Lower LD) |])
            |])
            StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
            StructuredTokenGroup.String ([|
                BaseToken.Letter (Upper LI)
                BaseToken.Letter (Upper LN)
                BaseToken.Letter (Upper LT) |])
            StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
            StructuredTokenGroup.String ([|
                BaseToken.Letter (Upper LN)
                BaseToken.Letter (Upper LO)
                BaseToken.Letter (Upper LT) |])
            StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
            StructuredTokenGroup.String ([|
                BaseToken.Letter (Upper LN)
                BaseToken.Letter (Upper LU)
                BaseToken.Letter (Upper LL)
                BaseToken.Letter (Upper LL) |])
            StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
            StructuredTokenGroup.String ([|
                BaseToken.Letter (Upper LI)
                BaseToken.Letter (Upper LD)
                BaseToken.Letter (Upper LE)
                BaseToken.Letter (Upper LN)
                BaseToken.Letter (Upper LT)
                BaseToken.Letter (Upper LI)
                BaseToken.Letter (Upper LT)
                BaseToken.Letter (Upper LY) |])
            StructuredTokenGroup.Group (QuoteGrouping.Grouping Parenthesis, [|
                StructuredTokenGroup.String [| BaseToken.Number N1 |]
                StructuredTokenGroup.Separation [|
                    BaseToken.Separator Comma
                    BaseToken.Separator Whitespace |]
                StructuredTokenGroup.String [| BaseToken.Number N1 |]
            |])
            StructuredTokenGroup.Separation [|
                BaseToken.Separator Comma
                BaseToken.Separator Whitespace |]
            StructuredTokenGroup.String ([|
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
            StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
            StructuredTokenGroup.Group (QuoteGrouping.Grouping Bracket, [|
                StructuredTokenGroup.String ([|
                    BaseToken.Letter (Upper LP)
                    BaseToken.Letter (Upper LK)
                    BaseToken.Symbol '_'
                    BaseToken.Letter (Upper LT)
                    BaseToken.Letter (Lower LE)
                    BaseToken.Letter (Lower LS)
                    BaseToken.Letter (Lower LT) |])
            |])
            StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
            StructuredTokenGroup.String ([|
                BaseToken.Letter (Upper LP)
                BaseToken.Letter (Upper LR)
                BaseToken.Letter (Upper LI)
                BaseToken.Letter (Upper LM)
                BaseToken.Letter (Upper LA)
                BaseToken.Letter (Upper LR)
                BaseToken.Letter (Upper LY) |])
            StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
            StructuredTokenGroup.String ([|
                BaseToken.Letter (Upper LK)
                BaseToken.Letter (Upper LE)
                BaseToken.Letter (Upper LY) |])
            StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
            StructuredTokenGroup.Group (QuoteGrouping.Grouping Parenthesis, [|
                StructuredTokenGroup.Group (QuoteGrouping.Grouping Bracket, [|
                    StructuredTokenGroup.String ([|
                        BaseToken.Letter (Upper LI)
                        BaseToken.Letter (Lower LD) |])
                |])
            |])
        |])
    |]
    let input = "CREATE TABLE [dbo].[Test] ([Id] INT NOT NULL IDENTITY(1, 1), CONSTRAINT [PK_Test] PRIMARY KEY ([Id]))"
    let actual = input |> BaseToken.classify |> BaseToken.group |> TokenGroup.classify |> TokenGroup.group :> StructuredTokenGroup seq
    Assert.Equal(expected, actual)


[<Fact>]
let ``Classify and group structures for simple declaration with single-quote value`` () =
    let expected = [|
        StructuredTokenGroup.String [|
           BaseToken.Letter (Upper LD)
           BaseToken.Letter (Upper LE)
           BaseToken.Letter (Upper LC)
           BaseToken.Letter (Upper LL)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LR)
           BaseToken.Letter (Upper LE) |]
        StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
        StructuredTokenGroup.String [|
           BaseToken.Symbol '@'
           BaseToken.Letter (Upper LT)
           BaseToken.Letter (Lower LE)
           BaseToken.Letter (Lower LS)
           BaseToken.Letter (Lower LT) |]
        StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
        StructuredTokenGroup.String [|
           BaseToken.Letter (Upper LV)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LR)
           BaseToken.Letter (Upper LC)
           BaseToken.Letter (Upper LH)
           BaseToken.Letter (Upper LA)
           BaseToken.Letter (Upper LR) |]
        StructuredTokenGroup.Group (QuoteGrouping.Grouping Parenthesis, [|
            StructuredTokenGroup.String [|
                BaseToken.Number N2
                BaseToken.Number N0 |]
        |])
        StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
        StructuredTokenGroup.Other [| BaseToken.Symbol '=' |]
        StructuredTokenGroup.Separation [| BaseToken.Separator Whitespace |]
        StructuredTokenGroup.Group (QuoteGrouping.Quote Quotes.SingleQuote, [|
            StructuredTokenGroup.String [|
               BaseToken.Letter (Upper LT)
               BaseToken.Letter (Lower LE)
               BaseToken.Letter (Lower LS)
               BaseToken.Letter (Lower LT) |]
        |])
    |]
    let input = "DECLARE @Test VARCHAR(20) = 'Test'"
    let actual = input |> BaseToken.classify |> BaseToken.group |> TokenGroup.classify |> TokenGroup.group :> StructuredTokenGroup seq
    Assert.Equal(expected, actual)
