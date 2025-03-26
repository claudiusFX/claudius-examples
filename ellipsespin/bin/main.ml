open Claudius

let tick t s _p _i =
  let ft = (float_of_int t) /. 1000. in
  let w, h = Screen.dimensions s in
  let buf = Framebuffer.init (w, h) (fun _ _ -> 0) in

  Framebuffer.draw_ellipse (w / 2) (h / 2) (150.0 *. sin ft) 110.0 15 buf;
  buf

let () =
  Palette.generate_plasma_palette 18 |>
  Screen.create 640 480 1 |>
  Base.run "Spinning Ellipse" None tick
