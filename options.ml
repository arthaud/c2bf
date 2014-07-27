let type_checking = ref true
let output = ref "a.bf"

let options = [
    "--no-type-check", Arg.Unit (fun () -> type_checking := false), "Do not check types";
    "-o", Arg.Set_string output, "Set the output file (default: a.bf)";
]
