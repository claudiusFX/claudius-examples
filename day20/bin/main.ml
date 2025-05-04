open Claudius

let tick t s fb _i =
  Unix.sleepf 0.005;
  let special = ((t mod 200) != 0) in
  Framebuffer.map_inplace (fun p ->
    match p with
    | 0 -> 0
    | 255 -> if special then 255 else 254
    | p -> p - 1
  ) fb;
  let font = Screen.font s in
  let x = Random.int 8 and y = Random.int 8 in
  let ch = char_of_int (Random.int 256) in
  let col = if (((t / 200) mod 8) == x) then 255 else 254 in
  let _ = Framebuffer.draw_char ((x * 22) + 20) ((y * 22) + 15) font ch col fb in ();
fb

let () =
  match Font.of_file "thirdparty/tamzen-font/psf/TamzenForPowerline10x20.psf" with
  | Error (reason) -> Printf.printf "Failed to read: %s" reason
  | Ok font -> (
    Palette.of_list (List.rev (0xff0000 :: (Palette.to_list (Palette.generate_mono_palette 255)))) |>
    Screen.create ~font 200 200 2 |>
    Base.run "Genuary 20/23: Generative Typography and 8x8" None tick
  )
