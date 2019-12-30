namespace Ellersoft.Tooling.Sql.Parsing

open Ellersoft.Tooling.Sql.Parsing.Objects

module TokenGroup =
    let classify (groups : BaseToken [] []) =
        groups
        |> Array.map (fun group ->
            match group.[0] with
            | Letter _ 
            | Symbol '_'
            | Symbol '@'
            | Number _ -> TokenGroup.String group
            | Separator _ -> TokenGroup.Separation group
            | Grouping (Open group) -> TokenGroup.Group (OpenCloseQuoteGrouping.Open group)
            | Grouping (Close group) -> TokenGroup.Group (OpenCloseQuoteGrouping.Close group)
            | Special SingleQuote -> TokenGroup.Group (OpenCloseQuoteGrouping.Quote Quotes.SingleQuote)
            | Special DoubleQuote -> TokenGroup.Group (OpenCloseQuoteGrouping.Quote Quotes.DoubleQuote)
            | _ -> TokenGroup.Other group)

    let group (groups : TokenGroup []) : StructuredTokenGroup [] =
        let rec mapGroups (search : OpenCloseQuoteGrouping option) (remGroups : TokenGroup []) (stackedGroups : StructuredTokenGroup []) =
            match remGroups with
            | [|  |] -> [| |], stackedGroups
            | groups ->
                let recurse groups = Array.append stackedGroups >> mapGroups search groups
                match groups.[0] with
                | TokenGroup.String s -> [| StructuredTokenGroup.String s |] |> recurse groups.[1..]
                | TokenGroup.Separation s -> [| StructuredTokenGroup.Separation s |] |> recurse groups.[1..]
                | TokenGroup.Other s -> [| StructuredTokenGroup.Other s |] |> recurse groups.[1..]
                | TokenGroup.Group (OpenCloseQuoteGrouping.Open g) ->
                    let rem, groups = mapGroups (Some (OpenCloseQuoteGrouping.Close g)) groups.[1..] [|  |]
                    [| StructuredTokenGroup.Group (QuoteGrouping.Grouping g, groups) |] |> recurse rem
                | TokenGroup.Group (OpenCloseQuoteGrouping.Quote g) ->
                    match search with
                    | Some (OpenCloseQuoteGrouping.Quote s) when s = g -> groups.[1..], stackedGroups
                    | _ ->
                        let rem, groups = mapGroups (Some (OpenCloseQuoteGrouping.Quote g)) groups.[1..] [|  |]
                        [| StructuredTokenGroup.Group (QuoteGrouping.Quote g, groups) |] |> recurse rem
                | TokenGroup.Group g ->
                    match search with
                    | Some s when s = g -> groups.[1..], stackedGroups
                    | _ ->
                        let gRes =
                            match g with
                            | OpenCloseQuoteGrouping.Open g -> Grouping (Open g)
                            | OpenCloseQuoteGrouping.Close g -> Grouping (Close g)
                            | OpenCloseQuoteGrouping.Quote g ->
                                Special (
                                    match g with
                                    | Quotes.SingleQuote -> SingleQuote
                                    | Quotes.DoubleQuote -> DoubleQuote)
                        [| StructuredTokenGroup.Other ([| gRes |]) |] |> recurse groups.[1..]
        mapGroups None groups [|  |] |> snd
        