{
module Parser (parseRule, parseCommand) where

import Zen
}

%name rule      rule
%name command   command
%tokentype      { Token }
%monad          { Either String }

%token
  'red'         { T "red"       }
  'green'       { T "green"     }
  'blue'        { T "blue"      }
  'circle'      { T "circle"    }
  'square'      { T "square"    }
  'triangle'    { T "triangle"  }
  'non'         { T "non-"      }
  'thing'       { T "thing"     }
  'touches'     { T "touches"   }
  'before'      { T "before"    }
  'count'       { T "count"     }
  'same'        { T "same"      }
  'different'   { T "different" }
  'of'          { T "of"        }
  'and'         { T "and"       }
  'color'       { T "color"     }
  'shape'       { T "shape"     }
  'no'          { T "no"        }
  '='           { T "="         }
  '/='          { T "/="        }
  '<'           { T "<"         }
  '<='          { T "<="        }
  '>'           { T ">"         }
  '>='          { T ">="        }
  NUM           { I $$          }
  '_'           { T "_"         }
  'guess'       { T "guess"     }
  'giveup'      { T "giveup"    }

%%

rule                                 :: { Rule }
  : pol prop                            { Rule $1 (Exist $2) }
  | pol prop 'touches' prop prop2       { Rule $1 (ExistAdjacent $2 $4 $5) }
  | pol prop 'before'  prop prop2       { Rule $1 (ExistBefore   $2 $4 $5) }
  | countTerm op term                   { $2 $1 $3 }

op
  : '='                                 { \l r -> Rule Yes (Compare Eq  l r) }
  | '<'                                 { \l r -> Rule Yes (Compare Lt  l r) }
  | '<='                                { \l r -> Rule Yes (Compare Leq l r) }
  | '/='                                { \l r -> Rule No  (Compare Eq  l r) }
  | '>'                                 { \l r -> Rule No  (Compare Leq l r) }
  | '>='                                { \l r -> Rule No  (Compare Lt  l r) }

pol                                  :: { Polarity }
  : 'no'                                { No }
  | {- empty -}                         { Yes }

prop                                 :: { Prop }
  : constraint(color) constraint(shape) { Object $1 $2 }
  | constraint(color)                   { Object $1 Unconstrained }
  | constraint(shape)                   { Object Unconstrained $1 }
  | 'thing'                             { Object Unconstrained Unconstrained }

prop2                                :: { Prop2 }
  : 'of' qualColor                      { Object $2 Unspecified }
  | 'of' qualShape                      { Object Unspecified $2 }
  | 'of' qualColor 'and' qualShape      { Object $2 $4 }
  | 'of' qualShape 'and' qualColor      { Object $4 $2 }
  | {- empty -}                         { Object Unspecified Unspecified }

qualColor                            :: { Constraint2 Color }
  : 'same' 'color'                      { Same }
  | 'different' 'color'                 { Different }

qualShape                            :: { Constraint2 Shape }
  : 'same' 'shape'                      { Same }
  | 'different' 'shape'                 { Different }


constraint(p)                        :: { Constraint p }
  : p                                   { Is Yes $1 }
  | 'non' p                             { Is No  $2 }

color                                :: { Color     }
  : 'red'                               { Red       }
  | 'green'                             { Green     }
  | 'blue'                              { Blue      }

shape                                :: { Shape     }
  : 'circle'                            { Circle    }
  | 'triangle'                          { Triangle  }
  | 'square'                            { Square    }

term                                 :: { Term }
  : NUM                                 { Const $1 }
  | countTerm                           { $1 }

countTerm                            :: { Term }
  : 'count' prop                        { Count $2 }

model                                :: { Model }
  : object                              { $1 : replicate 4 Empty }
  | object object                       { [$1,$2] ++ replicate 3 Empty }
  | object object object                { [$1,$2,$3] ++ replicate 2 Empty }
  | object object object object         { [$1,$2,$3,$4,Empty] }
  | object object object object object  { [$1,$2,$3,$4,$5] }

object                               :: { Thing }
  : '_'                                 { Empty }
  | color shape                         { Full (Object $1 $2) }

command                              :: { Command }
  : 'guess' rule                        { Guess $2 }
  | 'giveup'                            { Giveup }
  | model                               { Check $1 }

{

happyError :: [Token] -> Either String a
happyError xs =
  case xs of
    []    -> Left "Unexpected end of input"
    x : _ -> Left ("Unexpected token " ++ show x)

parseRule :: String -> Either String Rule
parseRule = rule . lexer

parseCommand :: String -> Either String Command
parseCommand = command . lexer


}
