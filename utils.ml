let error_msg msg =
	Printf.printf "error: %s\n" msg;
	exit 1

let expect_some msg opt =
	match opt with
	| Some x -> x
	| None -> error_msg msg

let parse_int str =
	expect_some "string is not int" (int_of_string_opt str)

let parse_float str =
	expect_some "string is not float" (float_of_string_opt str)

let clamp a b x =
	min b @@ max a x

let rec repeat n f =
	if n = 0 then ()
	else begin
		f n;
		repeat (pred n) f
	end

let get_terminal_width () =
	(expect_some "could not read terminal size" (Terminal_size.get_columns ())) - 1

let get_terminal_height () =
	(expect_some "could not read terminal size" (Terminal_size.get_rows ())) - 1
