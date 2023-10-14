
let flush () =
	flush stdout

let cursor_move_to_line_start () =
	Printf.printf "\x1b[G"

let cursor_move_y dy =
	if dy > 0 then
		Printf.printf "\x1b[%dB" dy
	else if dy < 0 then
		Printf.printf "\x1b[%dA" (-dy)

let cursor_move_up dy =
	cursor_move_y (-dy)

let cursor_move_down dy =
	cursor_move_y dy

let cursor_move_x dx =
	if dx > 0 then
		Printf.printf "\x1b[%dC" dx
	else if dx < 0 then
		Printf.printf "\x1b[%dD" (-dx)

let cursor_move_right dx =
	cursor_move_x dx

let cursor_move_left dx =
	cursor_move_x (-dx)

let cursor_move dx dy =
	cursor_move_x dx;
	cursor_move_x dy

let cursor_save () =
	Printf.printf "\x1b[s"

let cursor_restore () =
	Printf.printf "\x1b[u"

let set_fg (r, g, b) =
	Printf.printf "\x1b[38;2;%d;%d;%dm" r g b

let set_bg (r, g, b) =
	Printf.printf "\x1b[48;2;%d;%d;%dm" r g b

let reset_style () =
	Printf.printf "\x1b[0m"
	
