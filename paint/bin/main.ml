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

	match (Mouse.is_button_pressed inputs.mouse Mouse.Left) with
	| false -> (
		last_coord := None;
		buffer
	)
	| true -> (
		let x, y = Mouse.get_position inputs.mouse in

		let in_palette = x >= (w - size) in

		match !last_coord with
		| None -> (
			last_coord := (match in_palette with false -> Some (x, y) | true -> None);
			col := (match in_palette with true -> y / size | false -> !col);
			buffer
		)
		| Some (ox, oy) -> (
			last_coord := Some (x, y);
			Framebuffer.draw_line ox oy x y (!col) buffer;
			buffer
		)
	)

let () =
	Palette.generate_mac_palette () |>
	Screen.create 320 200 4 |>
	Base.run "Claudius Paint" (Some boot) tick
