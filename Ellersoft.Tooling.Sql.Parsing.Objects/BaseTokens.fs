namespace Ellersoft.Tooling.Sql.Parsing.Objects

type Letter = | LA | LB | LC | LD | LE | LF | LG | LH | LI | LJ | LK | LL | LM | LN | LO | LP | LQ | LR | LS | LT | LU | LV | LW | LX | LY | LZ    
type CasedLetter = | Upper of Letter | Lower of Letter
    
type Number = | N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
type Separator = | Whitespace | Comma | Period

type Grouping = | Parenthesis | Bracket | Brace
type OpenCloseGrouping = | Open of Grouping | Close of Grouping

type Special = | SingleQuote | DoubleQuote

type BaseToken =
    | Letter of CasedLetter
    | Separator of Separator
    | Number of Number
    | Grouping of OpenCloseGrouping
    | Special of Special
    | Symbol of char
