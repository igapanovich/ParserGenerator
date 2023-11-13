namespace ParserGenerator.ParserDefinition

type ProductionDefinitionCases<'s when 's: comparison> =
    | Single of 's list
    | Many of Set<string * 's list>

type ProductionDefinition<'s when 's: comparison> =
    { from: 's
      cases: ProductionDefinitionCases<'s> }

type Typing<'s> = { terminal: 's; type_: string }

type DefinitionItem<'s when 's: comparison> =
    | Typing of Typing<'s>
    | Production of ProductionDefinition<'s>

type ParserDefinition<'s when 's: comparison> =
    { typings: Set<Typing<'s>>
      productions: Set<ProductionDefinition<'s>> }
