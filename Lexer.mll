{
  open Grammar

  let hex_digits = "0123456789abcdefABCDEF"
  let is_hex_digit = String.contains hex_digits

  let parse_identifier = function
    | "and"       -> AND
    | "as"        -> AS
    | "case"      -> CASE
    | "class"     -> CLASS
    | "else"      -> ELSE
    | "from"      -> FROM
    | "if"        -> IF
    | "in"        -> IN
    | "import"    -> IMPORT
    | "interface" -> INTERFACE
    | "let"       -> LET
    | "module"    -> MODULE
    | "then"      -> THEN
    | "switch"    -> SWITCH
    | other       -> ID other

  exception Error of string

  let errorf format_string =
    Printf.ksprintf (fun string -> raise (Error string)) format_string
}

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let line_comment = "#" (_ # '\n')*

let hex_digit = ['a'-'f' 'A'-'F' '0'-'9']

(* Digits *)
let nonzero_dec = ['1'-'9']
let dec_digit = ['0'-'9']
let oct_digit = ['0'-'7']
let hex_digit = ['a'-'f' 'A'-'F'] | dec_digit

(* Number Literals *)
let dec_literal = (dec_digit | '_')+
let exponent = ('E' | 'e') ('-' | '+')? dec_literal
let float_suffix = (exponent | '.' dec_literal exponent?)
let number_literal = nonzero_dec (dec_digit | '_')* float_suffix?
                   | '0' (       (dec_digit | '_')* float_suffix?
                         | 'b'   ('1' | '0' | '_')+
                         | 'o'   (oct_digit | '_')+
                         | 'x'   (hex_digit | '_')+
                         )

rule read = parse
  | [' ' '\t' '\r']+ { read lexbuf }
  | '\n' { Lexing.new_line lexbuf; read lexbuf }
  | "{"  { LEFT_BRACE    }
  | "}"  { RIGHT_BRACE   }
  | "["  { LEFT_BRACKET  }
  | "]"  { RIGHT_BRACKET }
  | "("  { LEFT_PAREN    }
  | ")"  { RIGHT_PAREN   }
  | ":"  { COLON         }
  | ","  { COMMA         }
  | ";"  { SEMICOLON     }
  | "="  { EQUAL         }
  | "+"  { PLUS          }
  | "-"  { MINUS         }
  | "."  { DOT           }
  | "*"  { STAR          }
  | "/"  { SLASH         }
  | ">=" { GREATER_EQUAL }
  | "<=" { LESS_EQUAL    }
  | ">"  { GREATER       }
  | "<"  { LESS          }
  | "==" { EQUAL_EQUAL   }
  | "!=" { BANG_EQUAL    }
  | "&&" { AMPERSAND_AMPERSAND }
  | "||" { PIPE_PIPE     }
  | "@"  { AT            }
  | line_comment { read lexbuf }
  | '"' { read_string (Buffer.create 16) lexbuf }
  | number_literal as source { NUMBER (int_of_string source) }
  | identifier as id { parse_identifier id }
  | eof { EOF }
  | _ { raise Grammar.Error }

and read_string buffer = parse
  | '"' { STRING (Buffer.contents buffer) }

  | "\\x" (_ as first) (_ as second) as source
    {
      if is_hex_digit first && is_hex_digit second then
        (Buffer.add_string buffer (Scanf.unescaped source);
        read_string buffer lexbuf)
      else
        errorf "Bad escape sequence: %s" source;
    }
  | "\\" (_ as char) as source
    { if not (String.contains "nrt\\\"" char) then
        errorf "Bad escape sequence: %s" source;
      Buffer.add_string buffer (Scanf.unescaped source);
      read_string buffer lexbuf }

  | _ # '"' as char
    { if char = '\n' then Lexing.new_line lexbuf;
      Buffer.add_char buffer char;
      read_string buffer lexbuf }
