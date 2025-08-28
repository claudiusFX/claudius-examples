open Claudius

type point = { x : float; y : float }

let generate_points dimensions =
  let width, height = dimensions in
  let f_width, f_height = (float_of_int width, float_of_int height) in
  Random.init 42;
  List.init 42 (fun _i ->
      let x = Random.float f_width -. (f_width /. 2.)
      and y = Random.float f_height -. (f_height /. 2.) in
      { x; y })

let move_points t points =
  let ft = float_of_int t in
  List.map
    (fun p ->
      let distance = Float.sqrt ((p.x *. p.x) +. (p.y *. p.y)) in
      let angle = Float.atan2 p.y p.x in
      let new_distance = distance +. ft in
      let i_dist = int_of_float new_distance in
      let wrapped_dist = i_dist mod 300 in
      let f_wrapped_dist = float_of_int wrapped_dist in
      let f_wrapped_dist = f_wrapped_dist *. f_wrapped_dist /. 100. in
      let x = f_wrapped_dist *. Float.cos angle
      and y = f_wrapped_dist *. Float.sin angle in
      { x; y })
    points

let tick t s _f _e =
  let fb = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0) in
  let img = (Screen.pictures s).(0) in
  let w, h = Screen.dimensions s in
  let img_w = Picture.original_width img
  and img_h = Picture.original_height img in

  let points = generate_points (Screen.dimensions s) |> move_points t in

  List.iter
    (fun p ->
      let x, y = (int_of_float p.x, int_of_float p.y) in
      let distance_from_origin = Float.sqrt ((p.x *. p.x) +. (p.y *. p.y)) in
      let scale = distance_from_origin /. 200. in
      Framebuffer.draw_picture img ~scale
        (x - (img_w / 2) + (w / 2))
        (y - (img_h / 2) + (h / 2))
        fb)
    points;

  fb

let () =
  let image_filenames = [ "flying_camels/images/logo48.png" ] in
  Palette.generate_classic_vga_palette ()
  |> Screen.create ~image_filenames 640 480 1
  |> Base.run "Flying camels" None tick
