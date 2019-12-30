namespace Ellersoft.Tooling.Sql.Parsing.Objects

type Quotes = | SingleQuote | DoubleQuote

type OpenCloseQuoteGrouping =
    | Open of Grouping
    | Close of Grouping
    | Quote of Quotes

type QuoteGrouping =
    | Grouping of Grouping
    | Quote of Quotes
    
type TokenGroup =
    | String of BaseToken []
    | Separation of BaseToken []
    | Group of OpenCloseQuoteGrouping
    | Other of BaseToken []
    
type StructuredTokenGroup =
    | String of BaseToken []
    | Separation of BaseToken []
    | Group of QuoteGrouping * StructuredTokenGroup []
    | Other of BaseToken []
    