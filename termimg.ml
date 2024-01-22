let gray_of_color (c : Color.rgb) =
	int_of_float ((0.299 *. float c.r) +. (0.487 *. float c.g) +. (0.214 *. float c.b))

let char_of_gray map gray  =
	let inc = 256.0 /. float (Array.length map) in
	let i = float gray /. inc in
	map.(int_of_float i)

let term_color_of_image_color (ic : Color.rgb) =
	(ic.r, ic.g, ic.b)

let max3 a b c =
	if a > b && a > c then a
	else if b > a && b > c then b
	else c

let min3 a b c =
	if a < b && a < c then a
	else if b < a && b < c then b
	else c

let hsl_of_rgb (c : Color.rgb) =
	let r' = float c.r /. 255. in
	let g' = float c.g /. 255. in
	let b' = float c.b /. 255. in
	let cmax = max3 r' g' b' in
	let cmin = min3 r' g' b' in
	let d = cmax -. cmin in
	let h =
		if d = 0. then 0.
		else if cmax = b' then 60. *. (((r' -. g') /. d) +. 4.)
		else if cmax = r' then 60. *. (mod_float ((g' -. b') /. d) 6.)
		else if cmax = g' then 60. *. (((b' -. r') /. d) +. 2.)
		else assert false
	in let l = (cmax +. cmin) /. 2. in
	let s =
		match d with
		| 0. -> 0.
		| _ -> d /. (1. -. (abs_float (2. *. l -. 1.)))
	in (h, s, l)

let rgb_of_hsl (h, s, l) =
	let c = (1. -. abs_float (2. *. l -. 1.)) *. s in
	let x = c *. (1. -. abs_float ((mod_float (h /. 60.) 2.) -. 1.)) in
	let m = l -. c /. 2. in
	let r', g', b' =
		match truncate (h /. 60.) with
		| 0 -> (c, x, 0.)
		| 1 -> (x, c, 0.)
		| 2 -> (0., c, x)
		| 3 -> (0., x, c)
		| 4 -> (x, 0., c)
		| 5 -> (c, 0., x)
		| _ -> assert false
	in let r, g, b = (
		truncate @@ (r' +. m) *. 255., 
		truncate @@ (g' +. m) *. 255., 
		truncate @@ (b' +. m) *. 255.
	) in
	{ Color.r = r; g; b; }

let image_to_rgba32 = function
	| Images.Rgba32 img -> img
	| Images.Rgb24 img -> Rgb24.to_rgba32 img
	| Images.Index8 img -> Index8.to_rgba32 img
	| Images.Index16 img -> Index16.to_rgba32 img
	| _ -> assert false

let fill_size_in_container sw sh cw ch =
	let ar = float sw /. float sh in
	let wlw = float cw in
	let wlh = float cw /. ar in
	let hlw = float ch *. ar in
	let hlh = float ch in
	if (wlw <= float cw) && (wlh <= float ch)
	then (int_of_float wlw, int_of_float wlh)
	else (int_of_float hlw, int_of_float hlh)

let help_message = "\
termimg
usage: termimg [file] [options]
default: termimg [file] -a -d variety
options:
  -s [width] [height]                  - image size in characters
  -e [width] [height]                  - fits image in container
  -a                                   - fits the image to terminal size
  -l                                   - stretches image to terminal size
  -m [char-map-name]                   - char map name to use to draw image
  -u [char-map]                        - custom char map ex: '-u \" .:\"'
  -d                                   - double resolution instead of char map
  -c                                   - output colors
  -t [contrast]                        - contrast -255 to 255
  -z [hue shift]                       - hue shift
  -i [saturation]                      - sets saturation of image
  -b [brightness]                      - sets brightness of image
  -k                                   - keeps image/gif playing
  -f [framerate]                       - set framerate to play gif
  -1                                   - only play gif once
  -r                                   - repeat gif in reverse
char-maps:
  block {█}                            - one solid block - best used with '-c'
  blocks { ░▒▓█}                       - four shades of blocks
  vblocks { ▏▎▍▌▋▊▉█}                  - vertical blocks
  hblocks { ▁▂▃▄▅▆▇█}                  - horizontial blocks
  dots { `.,'\"}                        - different dots
  specials { .,-+=*?$%&#@}             - special characters
  variety { `.',:;\"^*sctb%8DGNMW#@}    - variety of characters
  operators {!@#$%^&*-+~?}             - operator symbols
  numbers [0-9]                        - numbers
  letters [a-z A-Z]                    - letters
  uletters [A-Z]                       - uppercase letters
  half {:@}                            - two shades
  all                                  - all ascii characters
"

let help_msg () =
	print_string help_message; exit 0

type size_type =
	| ExactSize of int * int
	| ContainerSize of int * int
	| AutoSize
	| FillSize

type options = {
	file : string;
	size : size_type;
	map : string array;
	double_res : bool;
	color : bool;
	live : bool;
	reverse : bool;
	once : bool;
	framerate : int;
	contrast : float;
	saturation : float option;
	hue_shift : float option;
	brightness : float option;
}

let default_options = {
	file = "";
	size = AutoSize;
	map = Cmaps.variety;
	double_res = false;
	color = false;
	live = false;
	reverse = false;
	once = false;
	framerate = 0;
	contrast = 0.;
	saturation = None;
	hue_shift = None;
	brightness = None;
}

let parse_args args =
	let rec aux args opts =
		match args with
		| "-s" :: w :: h :: rest -> aux rest { opts with size = ExactSize (Utils.parse_int w, Utils.parse_int h) }
		| "-e" :: w :: h :: rest -> aux rest { opts with size = ContainerSize (Utils.parse_int w, Utils.parse_int h) }
		| "-a" :: rest -> aux rest { opts with size = AutoSize }
		| "-l" :: rest -> aux rest { opts with size = FillSize }
		| "-m" :: map_name :: rest -> aux rest { opts with map = Cmaps.map_of_name map_name }
		| "-u" :: map :: rest -> aux rest { opts with map = Cmaps.map_of_string map }
		| "-d" :: rest -> aux rest { opts with double_res = true }
		| "-c" :: rest -> aux rest { opts with color = true }
		| "-t" :: contrast :: rest -> aux rest { opts with contrast = Utils.parse_float contrast }
		| "-z" :: hue_shift :: rest -> aux rest { opts with hue_shift = Some (Utils.parse_float hue_shift) }
		| "-i" :: saturation :: rest -> aux rest { opts with saturation = Some (Utils.parse_float saturation) }
		| "-b" :: brightness :: rest -> aux rest { opts with brightness = Some (Utils.parse_float brightness) }
		| "-k" :: rest -> aux rest { opts with live = true }
		| "-f" :: framerate :: rest -> aux rest { opts with framerate = Utils.parse_int framerate }
		| "-1" :: rest -> aux rest { opts with once = true }
		| "-r" :: rest -> aux rest { opts with reverse = true }
		| "-h" :: _ -> help_msg ()
		| "--help" :: _ -> help_msg ()
		| _ :: rest -> aux rest opts
		| [] -> opts
	in match args with
	| _ :: file :: rest -> aux rest { default_options with file = file }
	| _ -> help_msg ()

let print_image_char_map opts img =
	let prev_color : Color.rgb ref = ref { Color.r = 0; g = 1; b = 0; } in
	for y = 0 to img.Rgba32.height - 1 do
		for x = 0 to img.Rgba32.width - 1 do
			let px = Rgba32.unsafe_get img x y in
			let gray = gray_of_color px.color in
			let chr = char_of_gray opts.map gray in
			if px.alpha = 0 then begin
				print_string " ";
			end else begin
				if opts.color && !prev_color <> px.color then begin
					Terminal.set_fg @@ term_color_of_image_color px.color;
					prev_color := px.color
				end;
				print_string chr;
			end;
		done;
		if y <> img.Rgba32.height - 1 then Printf.printf "\n"
	done;
	Terminal.flush ()

let print_image_double_res opts img =
	let prev_fg : Color.rgb ref = ref { Color.r = 0; g = 1; b = 0; } in
	let prev_bg : Color.rgb ref = ref { Color.r = 0; g = 1; b = 0; } in
	for y = 0 to (img.Rgba32.height - 1) / 2 do
		for x = 0 to img.Rgba32.width - 1 do
			let px0 = Rgba32.unsafe_get img x (y * 2) in
			let px1 = Rgba32.unsafe_get img x (y * 2 + 1) in
			if !prev_bg <> px0.color then begin
				Terminal.set_bg @@ term_color_of_image_color px0.color;
				prev_bg := px0.color
			end;
			if !prev_fg <> px1.color then begin
				Terminal.set_fg @@ term_color_of_image_color px1.color;
				prev_fg := px1.color
			end;
			print_string "▄";
		done;
		if y <> img.Rgba32.height - 1 then Printf.printf "\n"
	done;
	Terminal.flush ()

let print_image opts img =
	match opts with
	| { double_res = true } -> print_image_double_res opts img
	| _ -> print_image_char_map opts img

let image_apply_filters opts img =
	if
		opts.contrast = 0. &&
		opts.hue_shift = None &&
		opts.saturation = None &&
		opts.brightness = None
	then img
	else begin
		let factor = (259. *. (opts.contrast +. 255.)) /. (255. *. (259. -. opts.contrast)) in
		for y = 0 to img.Rgba32.height - 1 do
			for x = 0 to img.Rgba32.width - 1 do
				let px = Rgba32.unsafe_get img x y in
				let color1 = px.color in
				let (h, s, l) = hsl_of_rgb color1 in
				let h =
					match opts.hue_shift with
					| Some shift -> mod_float (abs_float (h +. shift)) 360.
					| None -> h
				in let s = Option.value opts.saturation ~default:s in
				let l = Option.value opts.brightness ~default:l in
				let color = rgb_of_hsl (h, s, l) in
				let new_color = {
					Color.color = {
						r = Utils.clamp 0 255 @@ truncate @@ factor *. (float color.r -. 128.) +. 128.;
						g = Utils.clamp 0 255 @@ truncate @@ factor *. (float color.g -. 128.) +. 128.;
						b = Utils.clamp 0 255 @@ truncate @@ factor *. (float color.b -. 128.) +. 128.;
					};
					alpha = px.alpha;
				} in
				Rgba32.unsafe_set img x y new_color;
			done;
		done;
		img
	end

let load_image opts src_img =
	let tw, th = Utils.get_terminal_width (), Utils.get_terminal_height () in
	let src_img = image_to_rgba32 src_img in
	let src_w, src_h = (src_img.Rgba32.width, src_img.Rgba32.height) in
	let w, h =
		match opts.size with
		| ExactSize (w, h) -> w, h
		| ContainerSize (w, h) -> fill_size_in_container src_w src_h w h
		| AutoSize -> fill_size_in_container src_w src_h (tw / 2) th
		| FillSize -> (tw / 2), th
	in let w, h = w * 2, if opts.double_res then h * 2 else h in
	Rgba32.resize None src_img w h
	|> image_apply_filters opts

let load_image_file opts =
	load_image opts (Images.load opts.file [])

type dispose_method =
	| NoDispose
	| PreservePrevious
	| RestoreBackground

let gif_frame_get_dispose_method frame =
	let disposeMode =
		frame.Gif.frame_extensions
		|> List.find_map (fun ext ->
			match ext with
			| Gif.GifGraphics [s] ->
				let fields = int_of_char s.[0] in
				Some (0b111 land (fields lsr 2))
			| _ -> None
		)
	in match disposeMode with
	| Some 0b000 -> NoDispose
	| Some 0b001 -> PreservePrevious
	| Some 0b010 -> RestoreBackground
	| _ -> Utils.error_msg "undefined gif dispose method"

let load_gif_file opts =
	let blit_next_frame prev_img next_frame =
		let next_img = next_frame.Gif.frame_bitmap in
		let res_img = Index8.copy prev_img in
		let _ =
			Index8.map
				(fun a b -> if a = next_img.Index8.transparent then b else a)
				next_img 0 0
				res_img next_frame.Gif.frame_left next_frame.Gif.frame_top
				next_img.Index8.width next_img.Index8.height
		in res_img
	in let rec load_frames acc frames =
		match acc, frames with
		| _, [] -> acc
		| [], f :: rest -> load_frames ((f.Gif.frame_bitmap, f.Gif.frame_delay) :: acc) rest
		| (p, _) :: _, f :: rest ->
			begin match gif_frame_get_dispose_method f with
			| PreservePrevious -> load_frames ((blit_next_frame p f, f.Gif.frame_delay) :: acc) rest
			| _ -> load_frames ((f.Gif.frame_bitmap, f.Gif.frame_delay) :: acc) rest
			end
	in let gif = Gif.load opts.file [] in
	load_frames [] gif.frames
	|> List.rev
	|> Parany.Parmap.parmapi 4 (fun i (img, delay) -> (load_image opts (Images.Index8 img), (if delay = 0 then 8 else delay), i))
	|> List.sort (fun (_, _, a) (_, _, b) -> a - b)
	|> List.map (fun (img, delay, _) -> (img, delay))

let play_gif opts imgs =
	let (first, _) = List.hd imgs in
	Utils.repeat first.Rgba32.height (fun _ -> print_string "\n");
	Terminal.cursor_move_up (first.Rgba32.height - 1);
	let draw () =
		let draw_frame (img, frame_delay) =
			let start_time = Sys.time () in
			Terminal.cursor_save ();
			print_image opts img;
			Terminal.cursor_restore ();
			let waste_time = (Sys.time ()) -. start_time in
			let delay =
				if opts.framerate = 0
				then float_of_int frame_delay /. 100.
				else 1. /. float_of_int opts.framerate
			in Unix.sleepf @@ max 0. (delay -. waste_time)
		in List.iter draw_frame imgs;
		if opts.reverse then List.iter draw_frame @@ List.rev imgs
	in let rec draw_loop () =
		draw (); draw_loop ()
	in draw ();
	if opts.once = false then draw_loop ();
	Terminal.flush ()

let run opts =
	if opts.live && String.ends_with ~suffix:".gif" opts.file then begin
		play_gif opts @@ load_gif_file opts;
	end else begin
		let img = load_image_file opts in
		print_image opts img;
		if opts.live then ignore (read_line ())
		else print_string "\n"
	end

let () = run (parse_args (Array.to_list Sys.argv))
