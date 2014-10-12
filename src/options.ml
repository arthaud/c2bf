let type_checking = ref true
let debug = ref false
let output = ref "a.bf"
let optimization = ref 2

let options = [
    "--no-type-check", Arg.Unit (fun () -> type_checking := false), "Do not check types";
    "-d", Arg.Unit (fun () -> debug := true), "Debug mode";
    "-o", Arg.Set_string output, "Set the output file (default: a.bf)";
    "-O", Arg.Set_int optimization, "Change optimization level (default: 2)";
]
