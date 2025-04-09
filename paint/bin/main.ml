open Claudius

let boot s =
	let w, h = Screen.dimensions s in
	let buffer = Framebuffer.init (w, h) (fun _ _ -> 0) in
	let cols = (Palette.size (Screen.palette s)) in
	let size = h / cols in
	Framebuffer.filled_rect
		(w - (size + 1))
		0
		size
		h
		(cols - 1)
		buffer;
	for i = 0 to (cols - 1) do
		Framebuffer.filled_rect
			(w - size)
			(i * size)
			size
			size
			i
			buffer
	done;
	buffer

let last_coord : (int * int) option ref = ref None
let col = ref 15

let tick _ s buffer (inputs : Base.input_state) : Framebuffer.t =

	let w, h = Screen.dimensions s in
	let cols = (Palette.size (Screen.palette s)) in
	let size = h / cols in


	List.iter (fun e ->
		match e with
		| Mouse.Button_up (Left, _) -> (
			last_coord := None;
		)
		| Mouse.Button_down (Left, (x, y)) -> (
			if (x >= (w - size)) then (
				col := y / size
			);
			last_coord := Some (x, y)
		)
		| Mouse.Drag (Left, (x, y)) -> (

			match !last_coord with
			| None -> (
			)
			| Some (ox, oy) -> (
				last_coord := Some (x, y);
				Framebuffer.draw_line ox oy x y (!col) buffer
			)
		)
		| _ -> ()
	) ( Mouse.get_events inputs.mouse);
	buffer

let () =
	Palette.generate_mac_palette () |>
	Screen.create 320 200 4 |>
	Base.run "Claudius Paint" (Some boot) tick
