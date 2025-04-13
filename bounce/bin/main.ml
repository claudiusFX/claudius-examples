open Claudius

let gravity = 0.5 
let energy_loss = 0.90 

let x_velocity = 2
let y_velocity = ref 0.0
let y_position = ref 100.0
let radius = 55
let color = 15


let tick t (screen : Screen.t) (_prev : Framebuffer.t) (_inputs : Base.input_state) : Framebuffer.t =
>>>>>>> File-updates
  let width, height = Screen.dimensions screen in
  let buffer = Framebuffer.init (width, height) (fun _x _y -> 0) in

  let total_distance = width in
  let x_position = (t * x_velocity) mod total_distance in

  y_velocity := !y_velocity +. gravity;
  y_position := !y_position +. !y_velocity;

  if !y_position >= float_of_int (height - radius) then begin
    y_velocity := !y_velocity *. -.energy_loss;  
    y_position := float_of_int (height - radius);

    if abs_float !y_velocity < 0.01 then
      y_velocity := 0.0;
  end;

  let squash = if !y_position >= float_of_int (height - radius) then
    1.0 +. (min (abs_float !y_velocity /. 20.0) 0.2) 
  else
    1.0
  in

  let y_radius = float_of_int radius /. squash in
  let x_radius = float_of_int radius *. squash in

  Framebuffer.draw_ellipse x_position (int_of_float !y_position) x_radius y_radius color buffer;

  buffer

let () =
  Palette.generate_plasma_palette 18
  |> Screen.create 640 480 1
  |> Base.run "Bouncing Ball with Squash" None tick