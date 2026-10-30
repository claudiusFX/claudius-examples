open Claudius
open Claudius_sdl

let tick t s _p _i =
  let ft = (float_of_int t) /. 1000. in
  let w, h = Screen.dimensions s in
  let buf = Framebuffer.init (w, h) (fun _ _ -> 0) in

  let ellipse_width = 75.0 +. (75.0 *. sin ft) in
  Framebuffer.filled_ellipse (w / 3) (h / 3) ellipse_width 110.0 50 buf;
  buf

let () =
  Palette.generate_plasma_palette 18 |>
  Screen.create "Spinning Ellipse" 640 480 |>
  Base.run tick
