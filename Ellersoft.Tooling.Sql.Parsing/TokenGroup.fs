namespace Ellersoft.Tooling.Sql.Parsing

open Ellersoft.Tooling.Sql.Parsing.Objects

module TokenGroup =
    let classify (groups : BaseToken [] []) =
        groups
        |> Array.map (fun group ->
            match group.[0] with
            | Letter _ 
            | Symbol '_'
            | Number _ -> TokenGroup.String group
            | Separator _ -> TokenGroup.Separation group
            | Grouping group -> TokenGroup.Group group
            | _ -> TokenGroup.Other group)

    let group (groups : TokenGroup []) : StructuredTokenGroup [] =
        let rec mapGroups (search : OpenCloseGrouping option) (remGroups : TokenGroup []) (stackedGroups : StructuredTokenGroup []) =
            match remGroups with
            | [|  |] -> [| |], stackedGroups
            | groups ->
                let recurse groups = Array.append stackedGroups >> mapGroups search groups
                match groups.[0] with
                | TokenGroup.String s -> [| StructuredTokenGroup.String s |] |> recurse groups.[1..]
                | TokenGroup.Separation s -> [| StructuredTokenGroup.Separation s |] |> recurse groups.[1..]
                | TokenGroup.Other s -> [| StructuredTokenGroup.Other s |] |> recurse groups.[1..]
                | TokenGroup.Group (Open g) ->
                    let rem, groups = mapGroups (Some (Close g)) groups.[1..] [|  |]
                    [| StructuredTokenGroup.Group (g, groups) |] |> recurse rem
                | TokenGroup.Group g ->
                    match search with
                    | Some s when s = g -> groups.[1..], stackedGroups
                    | _ -> [| StructuredTokenGroup.Other ([| Grouping g |]) |] |> recurse groups.[1..]
        mapGroups None groups [|  |] |> snd
        