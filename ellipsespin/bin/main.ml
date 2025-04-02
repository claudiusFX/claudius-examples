open Claudius

let tick t s _p _i =
  let ft = (float_of_int t) /. 1000. in
  let w, h = Screen.dimensions s in
  let buf = Framebuffer.init (w, h) (fun _ _ -> 0) in

  let ellipse_width = 75.0 +. (75.0 *. sin ft) in 
  Framebuffer.draw_ellipse (w / 3) (h / 3) ellipse_width 110.0 50 buf;
  buf

let () =
  Palette.generate_plasma_palette 18 |>
  Screen.create 640 480 1 |>
  Base.run "Spinning Ellipse" None tick
