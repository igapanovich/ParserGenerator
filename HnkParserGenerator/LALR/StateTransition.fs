namespace HnkParserGenerator.LALR

type internal StateTransition<'symbol when 'symbol : comparison> = {
    sourceState : State<'symbol>
    symbol : 'symbol
    destinationState : State<'symbol>
    }