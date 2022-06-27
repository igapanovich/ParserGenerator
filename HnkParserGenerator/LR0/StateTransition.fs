namespace HnkParserGenerator.LR0

type internal StateTransition<'symbol when 'symbol : comparison> = {
    sourceState : State<'symbol>
    symbol : 'symbol
    destinationState : State<'symbol>
    }