let rec strList (sl : string list) : unit =
  match sl with
  | [] -> print_string ""
  | hd::tl -> print_string (hd^"\n"); strList tl

let rec toInt (str: string)(i: int): string =
  match i with
  | 0 -> ""
  | _ -> (toInt str (i-1))^(Char.escaped (String.get str i))

let checkInt (str: string): string =
  try int_of_string str; str
  with _ -> ":error:"

let zero (str: string): string =
  match str with
  | "0" -> "0"
  | _ -> ("-"^str)

let intError (command: string): string =
  if ((String.get command 0) = '-') then
          (zero (checkInt (toInt command ((String.length command)-1))))
  else checkInt command

let strError (command: string): string =
  if ((String.get command 0) = '"') then
          if ((String.get command ((String.length command)-1)) = '"') then command
          else ":error:"
  else ":error:"

let char_ (c: char): bool =
  if (((Char.code c) >= 65) && ((Char.code c) <= 90) || ((Char.code c) >= 97) && ((Char.code c) <= 122))
          then true
  else false

let digit_ (c: char): bool =
  if (((Char.code c) >= 48) && ((Char.code c) <= 57))
          then true
  else false

let nameChar (c: char): char =
  if (c = '_' || char_ c || digit_ c) then 't'
  else 'f'

let nameError (command: string): string =
  if (((String.get command 0) = '_') || (char_ (String.get command 0)))
          then let boolStr = (String.map (nameChar) command) in
                  if (String.contains boolStr 'f') then ":error:"
                  else command
  else ":error:"

let boolError (command: string): string =
  match command with
  | ":true:" -> command
  | ":false:" -> command
  | _ -> ":error:"

let pushError (command: string): string =
  match command with
  | ":unit:" -> command
  | _ -> ":error:"

let popError (stack: string list): bool =
  try List.hd stack; true
  with _ -> false

let addError (stack: string list): string list =
  match stack with
  | [] -> (":error:"::stack);
  | hd::tl -> try let int1 = int_of_string hd in
                  match tl with
                  | [] -> (":error:"::stack)
                  | hd2::tl2 -> try let int2 = int_of_string hd2 in
                                          let sum = int1 + int2 in
                                                  let str_sum = string_of_int sum in (str_sum::tl2)
                                with _ -> (":error:"::stack)
              with _ -> (":error:"::stack)

let rec command_ (commands: string list) (stack: string list): string list =
 match commands with
 | [] -> stack
 | hd::tl -> (match hd with
             | "pushi" -> (match tl with
                           | [] -> command_ [] (":error:"::stack)
                           | v::tl2 -> command_ tl2 ((intError v)::stack))
             | "pushs" -> (match tl with
                           | [] -> command_ [] (":error:"::stack)
                           | v::tl2 -> command_ tl2 ((strError v)::stack))
             | "pushn" -> (match tl with
                          | [] -> command_ [] (":error:"::stack)
                          | v::tl2 -> command_ tl2 ((nameError v)::stack))
             | "pushb" -> (match tl with
                          | [] -> command_ [] (":error:"::stack)
                          | v::tl2 -> command_ tl2 ((boolError v)::stack))
             | "push" -> (match tl with
                          | [] -> command_ [] (":error:"::stack)
                          | v::tl2 -> command_ tl2 ((pushError v)::stack))
             | "pop" -> ((if (popError stack) then command_ tl (List.tl stack)
                          else command_ tl (":error:"::stack)))
             | "add" -> (let stack = addError stack in command_ tl stack)
             | _ -> command_ tl (":error:"::stack))


let Interpreter (inFile: string) (outFile: string): unit =
  let ic = open_in inFile in
  let rec read_file (com_list: string list) =
          try
                  let l = input_line ic in
                  print_string (l^"\n");
                  let command = (List.nth (String.split_on_char ' ' l) 0) in
                  print_string (command^"\n");
                  if ((String.length l)-(String.length command) = 0)
                          then
                               match command with
                               | "pop" -> (read_file (command::com_list))
                               | "add" -> (read_file (command::com_list))
                               | _ -> read_file com_list
                  else
                       let com_list = (command::com_list) in
                       let start = ((String.length command)+1) in
                       let new_length = (String.length l)-start in
                       let com_list = ((String.sub l start new_length)::com_list) in
                       read_file com_list;
          with
          | End_of_file -> List.rev com_list in

  let commands = read_file [] in
  strList commands;
  let stack = command_ commands [] in

  strList stack;

  let oc = open_out outFile in
  let rec complete (out_stack: string list): unit =
          match out_stack with
          | [] -> ()
          | hd::tl -> if ((String.get hd 0) = '"') then ((Printf.fprintf oc "%s\n" (String.sub hd 1 ((String.length hd)-2))); complete tl)
                      else (Printf.fprintf oc "%s\n" hd); complete tl
  in
  complete stack;;
