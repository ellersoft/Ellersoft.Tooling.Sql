namespace Ellersoft.Tooling.Sql.Parsing

open Ellersoft.Tooling.Sql.Parsing.Objects

module TokenGroup =
    let classify (groups : BaseToken [] []) =
        let rec mapGroups (groups : BaseToken [] []) (acc : TokenGroup []) =
            match groups with
            | [|  |] -> acc
            | rem ->
                let newGroups =
                    match rem.[0].[0] with
                    | Letter _ 
                    | Symbol '_'
                    | Number _ -> [| TokenGroup.String rem.[0] |]
                    | Separator _ -> [| TokenGroup.Separation rem.[0] |]
                    | Grouping group -> [| TokenGroup.Group group |]
                    | _ -> [| TokenGroup.Other rem.[0] |]
                let groups = newGroups |> Array.append acc
                mapGroups rem.[1..] groups
        mapGroups groups [|  |]

    let group (groups : TokenGroup []) : StructuredTokenGroup [] =
        let rec mapGroups (search : Grouping option) (remGroups : TokenGroup []) (stackedGroups : StructuredTokenGroup []) =
            match remGroups with
            | [|  |] -> [| |], stackedGroups
            | groups ->
                let recurse groups = Array.append stackedGroups >> mapGroups search groups
                match groups.[0] with
                | TokenGroup.String s -> [| StructuredTokenGroup.String s |] |> recurse groups.[1..]
                | TokenGroup.Separation s -> [| StructuredTokenGroup.Separation s |] |> recurse groups.[1..]
                | TokenGroup.Other s -> [| StructuredTokenGroup.Other s |] |> recurse groups.[1..]
                | TokenGroup.Group (Close g) ->
                    match search with
                    | Some s when s = g -> groups.[1..], stackedGroups
                    | _ -> [| StructuredTokenGroup.Other ([| Grouping (Close g) |]) |] |> recurse groups.[1..]
                | TokenGroup.Group (Open g) ->
                    let rem, groups = mapGroups (Some g) groups.[1..] [|  |]
                    [| StructuredTokenGroup.Group (g, groups) |] |> recurse rem
        mapGroups None groups [|  |] |> snd
        