open Claudius

type tool =
	| Paint
	| Pen
	| Rect
let tools = [Paint ; Pen ; Rect]

let last_coord : (int * int) option ref = ref None
let col = ref 15
let before : Framebuffer.t option ref = ref None
let tool = ref Paint

let draw_initial_screen s buffer =
	let w, h = Screen.dimensions s in
	Framebuffer.map_inplace (fun _ -> 0) buffer;
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
	Framebuffer.draw_line size 0 size (h - 1) 15 buffer;
	List.iteri (fun i t ->
		Framebuffer.draw_line
		0
		((i + 1) * size)
		size
		((i + 1) * size)
		15
		buffer;
		match t with
		| Paint -> Framebuffer.filled_circle (size / 2) ((i * size) + (size / 2)) 4.0 15 buffer
		| Pen -> Framebuffer.filled_circle (size / 2) ((i * size) + (size / 2)) 0.5 15 buffer
		| Rect -> Framebuffer.filled_rect 2 ((i * size) + 3) (size - 5) (size - 6) 15 buffer
	) tools


let boot s =
	let w, h = Screen.dimensions s in
	let buffer = Framebuffer.init (w, h) (fun _ _ -> 0) in
	draw_initial_screen s buffer;
	buffer

let tick _ s buffer (inputs : Base.input_state) : Framebuffer.t =

	let w, h = Screen.dimensions s in
	let cols = (Palette.size (Screen.palette s)) in
	let size = h / cols in

	List.iter (fun e ->
		match e with
		| Event.MouseButtonUp (Left, _) -> (
			last_coord := None;
			before := None;
		)
		| Event.MouseButtonDown (Left, (x, y)) -> (
			if (x >= (w - size)) then (
				col := y / size
			);
			if (x <= size) then (
				tool := (match (y / size) with
					| 2 -> Rect
					| 1 -> Pen
					| _ -> Paint)
			);
			if (x > size) && (x < (w - size)) then (
				last_coord := Some (x, y);
				before := Some (Framebuffer.map (fun x -> x) buffer);
				match !tool with
				| Paint -> Framebuffer.filled_circle x y 4.0 (!col) buffer
				| _ -> ()
			)
		)
		| Event.MouseDrag (Left, (x, y)) -> (
			match !tool with
			| Paint -> Framebuffer.filled_circle x y 4.0 (!col) buffer
			| Pen -> (
				match !last_coord with
				| None -> (
				)
				| Some (ox, oy) -> (
					last_coord := Some (x, y);
					Framebuffer.draw_line ox oy x y (!col) buffer
				)
			)
			| Rect -> (
				match !last_coord with
				| None -> (
				)
				| Some (ox, oy) -> (
					match !before with
					| None -> ()
					| Some before_buffer -> (
						Framebuffer.map2_inplace (fun _ x -> x) buffer before_buffer
					);
					let rx = if ox > x then x else ox
					and ry = if oy > y then y else oy
					and width = abs(ox - x)
					and height = abs(oy - y) in
					Framebuffer.filled_rect rx ry width height (!col) buffer
				)
			)
		)
		| Event.KeyUp Key.C -> (
			draw_initial_screen s buffer
		)
		| _ -> ()
	) inputs.events ;
	buffer

let () =
	Palette.generate_mac_palette () |>
	Screen.create 320 200 4 |>
	Base.run "Claudius Paint" (Some boot) tick
