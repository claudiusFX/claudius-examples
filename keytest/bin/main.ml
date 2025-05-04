open Claudius

let pos = ref (0, 0)

let tick _t s _prev (inputs : Base.input_state) =
  let fb = Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0) in
  let font = Screen.font s in
  List.iteri (fun i c ->
    let s = Printf.sprintf "0x%08x" (Base.PlatformKey.to_backend_keycode c) in
    ignore(Framebuffer.draw_string 5 (i * 12) font s 8 fb)
  ) (Base.KeyCodeSet.to_list inputs.keys);

  let dx, dy = List.fold_right (
    fun c (x, y) ->
      let w, h = Screen.dimensions s in
      match c with
      | Key.Right -> (x + 1, y)
      | Key.Left -> (x - 1, y)
      | Key.Down -> (x, y + 1)
      | Key.Up -> (x, y - 1)
      | Key.Space -> (w / 2, h / 2)
      | _ -> (x, y)
  ) (Base.KeyCodeSet.to_list inputs.keys) !pos in

  Framebuffer.filled_circle dx dy 5.0 8 fb;
  pos := (dx, dy);
  fb

(* ----- *)

let () =
  (* match Font.load_psf_font "thirdparty/tamzen-font/psf/TamzenForPowerline10x20.psf" with
  | Error (reason) -> Printf.printf "Failed to read: %s" reason
  | Ok font -> ( *)
    Palette.of_list (List.rev (Palette.to_list (Palette.generate_plasma_palette 16))) |>
    Screen.create 640 480 1  |>
    Base.run "Keyboard test" None tick
  (* ) *)
