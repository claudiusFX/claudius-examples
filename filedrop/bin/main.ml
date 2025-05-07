open Claudius
open Giflib

let image : GIF.t option ref = ref None

let draw_gif t s gif fb =
	let c = GIF.image_count gif in
	let frame = ((t / 2) mod c) in
	let i = GIF.get_image gif frame in

	let new_pal = Palette.of_list ( 0x000000 :: 0xFFFFFF :: List.map (fun (r, g, b) ->
		b + (g * 256) + (r * 256 * 256)
	) (Array.to_list (Image.palette i))) in
	Screen.update_palette s new_pal;

	let sw, sh = Screen.dimensions s
	and gw, gh = GIF.dimensions gif
	and iw, ih = Image.dimensions i
	and ixoff, iyoff = Image.offset i
	and pixels = Image.pixels i in
	let transparent = match (Image.transparent i) with
	  | None -> -1
		| Some x -> x
	in
	let sxoff = (sw - gw) / 2
	and syoff = (sh - gh) / 2 in
	for x = 0 to (iw - 1) do
	  for y = 0 to (ih - 1) do
		let v = pixels.(x + (y * iw)) in
		if (v != transparent) then
		Framebuffer.pixel_write (x + sxoff + ixoff) (y + syoff + iyoff) (v + 2) fb
	  done
	done;

  fb

let boot s =
	let fb = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0) in
	let font = Screen.font s in
	ignore (Framebuffer.draw_string 10 10 font "Drop a GIF here" 1 fb);
	fb

let tick t s prev (inputs : Base.input_state) =

	let filename = List.fold_left (fun acc ev ->
		match ev with
		| Event.DropFile pth -> Some pth
		| _ -> acc
	) None inputs.events in

	let updated = match filename with
	| None -> false
	| Some filename -> (
		(match Filename.extension filename with
		| ".gif" -> image := Some (GIF.from_file filename)
		| _ -> image := None);
		true
	)
	in

	match !image, updated with
	| None, true -> boot s
	| None, false -> prev
	| Some img, false -> (
		match GIF.image_count img with
		| 1 -> prev
		| _ ->  draw_gif t s img prev
	)
	| Some img, true -> (
		Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0) |>
		draw_gif t s img
	)

let () =
	Palette.of_list (0x000000 :: 0xFFFFFF :: (Palette.to_list (Palette.generate_plasma_palette 256))) |>
	Screen.create 640 480 1 |>
	Base.run "File drop test" (Some boot) tick
