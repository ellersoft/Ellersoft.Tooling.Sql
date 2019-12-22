namespace Ellersoft.Tooling.Sql.Parsing.Objects

type TokenGroup =
    | String of BaseToken []
    | Separation of BaseToken []
    | Group of OpenCloseGrouping
    | Other of BaseToken []
    
type StructuredTokenGroup =
    | String of BaseToken []
    | Separation of BaseToken []
    | Group of Grouping * StructuredTokenGroup []
    | Other of BaseToken []
    