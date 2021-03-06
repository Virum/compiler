let (>>) f g x = g (f x)
module Sexp = Core_kernel.Std.Sexp
module Color = Terminal.Text.Color
module Template = Terminal.Template

module AssertionResult = struct
  type t = Success | Failure | Error

  let is_negative = function
    | Success -> false
    | Failure | Error -> true

  let to_string = function
    | Success -> Color.green  "."
    | Failure -> Color.red    "F"
    | Error   -> Color.yellow "E"

  let of_bool = function
    | true  -> Success
    | false -> Failure
end

module TestResult = struct
  type t = {description: string; results: AssertionResult.t list ref}

  let template =
    Template.(text "└── " ^ red (var `description) ^ text ": " ^ var `results)

  let is_negative {description; results={contents=assertion_results}} =
    List.exists AssertionResult.is_negative assertion_results

  let to_string {description; results={contents=assertion_results}} =
    let string_list = List.map AssertionResult.to_string assertion_results in
    Template.render template ~vars:(function
      | `description -> description
      | `results -> String.concat "" (List.rev string_list))
end

module SuiteResult = struct
  type t = TestResult.t list ref

  let is_negative ({contents=test_result_list}: t) =
    List.exists TestResult.is_negative test_result_list

  let to_string ({contents=test_result_list}: t) =
    let test_result_list =
      List.filter TestResult.is_negative test_result_list in
    let string_list = List.map TestResult.to_string test_result_list in
    String.concat "\n" string_list ^ "\n"
end

let global_suite_result = ref []

let ongoing_test_result () =
  let {contents=test_result_list} = global_suite_result in
  List.hd test_result_list

let prepend (list_ref: 'a list ref) item =
  let list = !list_ref in
  list_ref := item :: list

let add_assertion_result assertion_result =
  let {TestResult.results=assertion_result_list_ref; _} =
    ongoing_test_result () in
  prepend assertion_result_list_ref assertion_result;
  print_string (AssertionResult.to_string assertion_result)

let report () =
  print_newline ();
  if SuiteResult.is_negative global_suite_result then
    print_string (SuiteResult.to_string global_suite_result)

let () = at_exit report

let (=>) left right =
  add_assertion_result (AssertionResult.of_bool (left = right))

let test description test =
  prepend global_suite_result TestResult.({description; results=(ref [])});
  try test () with _ -> add_assertion_result AssertionResult.Error

module Assertion = struct
  let create to_sexp left right =
    let to_string = to_sexp >> Sexp.to_string_hum in
    let is_success = Sexp.equal (to_sexp left) (to_sexp right) in
    add_assertion_result (AssertionResult.of_bool is_success);
    if not is_success then
      Template.(print (
        (text "\n  Expected: ") ^ (green (text (to_string right))) ^
        (text "\n  Got:      ") ^ (red   (text (to_string left ))) ^ (text "\n")
      ))
end
