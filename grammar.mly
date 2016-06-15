%{ open Syntax %}

%start <Syntax.term> parse_term
%start <Syntax.item> parse_item

%token AND AS CASE CLASS ELSE FROM IF IMPORT INTERFACE LET MODULE SWITCH THEN IN
       DO

%token LEFT_PAREN RIGHT_PAREN LEFT_BRACKET RIGHT_BRACKET LEFT_BRACE RIGHT_BRACE
       COLON COMMA SEMICOLON EQUAL PLUS MINUS DOT STAR SLASH GREATER_EQUAL
       LESS_EQUAL GREATER LESS EQUAL_EQUAL BANG_EQUAL PIPE_PIPE AT
       AMPERSAND_AMPERSAND

%token <string> ID STRING
%token <int> NUMBER

%token EOF

(* Associatifity and precedence, from low to high *)
%left IN ELSE COLON

%left PIPE_PIPE
%left AMPERSAND_AMPERSAND
%left GREATER_EQUAL LESS_EQUAL GREATER LESS EQUAL_EQUAL BANG_EQUAL
%left PLUS MINUS
%left STAR SLASH

%left LEFT_PAREN (* call *)
(*
%nonassoc __unary_precedence__
%left DOT
*)

%%

parse_item: item EOF { $1 }

item:
| LET left=pattern EQUAL right=term
  { Let (left, right) }
| DO term=term
  { Do term }

parse_term: term EOF { $1 }

if_else(__term__):
| IF condition=term THEN consequence=term ELSE alternative=__term__
  { IfElse (condition, consequence, alternative) }

make_term(__term__):
| ID { Identifier $1 }
| STRING { String $1 }
| NUMBER { Number $1 }
| parenthesised(term) { $1 }
| if_else(__term__) { $1 }
| LET left=pattern EQUAL right=term IN body=__term__
  { LetIn (left, right, body) }
| left=__term__ operator=infix_operator right=__term__
  { Infix (left, operator, right) }
| caller=__term__ arguments=parenthesised(comma_separated(term))
  { Call (caller, arguments) }

term_no_case:
| make_term(term_no_case) { $1 }

pattern: ID { $1 }

term:
| make_term(term) { $1 }
| nonempty_list(case) { CaseFunction $1 }
| SWITCH subject=term_no_case
    cases=nonempty_list(case) { Switch (subject, cases) }

case:
| CASE pattern=pattern COLON consequence=term_no_case
  { (pattern, consequence) }











%inline infix_operator:
| PLUS                { Plus           }
| MINUS               { Minus          }
| STAR                { Times          }
| SLASH               { Divide         }
| LESS                { Less           }
| GREATER             { Greater        }
| LESS_EQUAL          { LessOrEqual    }
| GREATER_EQUAL       { GreaterOrEqual }
| EQUAL_EQUAL         { Equal          }
| BANG_EQUAL          { NotEqual       }
| AMPERSAND_AMPERSAND { And            }
| PIPE_PIPE           { Or             }


parenthesised(BODY): LEFT_PAREN   body=BODY RIGHT_PAREN   { body }
bracketed(BODY):     LEFT_BRACKET body=BODY RIGHT_BRACKET { body }
braced(BODY):        LEFT_BRACE   body=BODY RIGHT_BRACE   { body }

comma_separated(ITEM): (* Comma-separated list with optional trailing comma *)
  | list=reverse_list(terminated(ITEM, COMMA)) last=ITEM?
    { List.rev (match last with None -> list | Some item -> item :: list) }

(* empty: {} *)

reverse_list(ITEM):
  | (* empty *) { [] }
  | rest=reverse_list(ITEM) item=ITEM { item :: rest }
