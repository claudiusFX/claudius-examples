open Claudius

(* Declarations *)
let gravity     = 0.5
let energy_loss = 0.90
let max_width  = 640.0
let max_height = 480.0
let radius     = 55.0
let color      = 15
let initial_y = 100.0
let initial_x = max_width /. 2.0

(* Declarations for changeable statess *)
let y_pos  = ref initial_y
let y_vel  = ref 0.0
let x_pos  = ref initial_x
let x_vel  = ref 2.0
let filled = ref true

(* Squash and stopping parameters :) Must stop after some time. This is to make sure that the whole infinitesimal bounce effect (Rolling) is avoided *)
let squash_factor              = 0.25
let fps                        = 60.0
let stop_after_frames          = int_of_float (2.0 *. fps) 
let bounce_vis_vel_threshold   = 2.0                        
let last_bounce_t = ref 0

(* where the key states are being tracked *)
let was_space_pressed = ref false
let was_x_pressed = ref false

(* Drawing *)
let draw_ball ~x ~y ~xr ~yr ~filled fb =
  if filled then
    Framebuffer.filled_ellipse x y xr yr color fb
  else
    let thickness = 3 in
    for i = 0 to thickness - 1 do
      Framebuffer.draw_ellipse x y (xr +. float_of_int i) (yr +. float_of_int i) color fb
    done

(* Main update function *)
let tick t s _prev (inputs : Base.input_state) =
  let fb = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0) in

  (* Handle key presses *)
  if Base.KeyCodeSet.mem Key.Space inputs.keys && not !was_space_pressed then begin
    y_pos := initial_y;
    x_pos := radius +. (Random.float (max_width -. 2.0 *. radius));
    x_vel := if !x_pos > max_width /. 2.0 then -.2.0 else 2.0;
    y_vel := 0.0;
    last_bounce_t := t;
    was_space_pressed := true 
  end;

  if Base.KeyCodeSet.mem Key.X inputs.keys && not !was_x_pressed then begin
    filled := not !filled;
    was_x_pressed := true  
  end;

  (* Update physics *)
  y_vel := !y_vel +. gravity;
  y_pos := !y_pos +. !y_vel;
  x_pos := !x_pos +. !x_vel;

  (* for wall boundary collison *)
  if !x_pos >= max_width -. radius then begin
    x_vel := -. !x_vel;
    x_pos := max_width -. radius
  end else if !x_pos <= radius then begin
    x_vel := -. !x_vel;
    x_pos := radius
  end;

  (* for buttom and top collisions *)
  let draw_x = int_of_float !x_pos in
  let draw_y = int_of_float !y_pos in

  if !y_pos >= max_height -. radius then begin
    y_vel := -. !y_vel *. energy_loss;
    y_pos := max_height -. radius;

    if abs_float !y_vel > bounce_vis_vel_threshold then
      last_bounce_t := t;

    let squash = 1.0 +. min (abs_float !y_vel /. 20.0) squash_factor in
    let yr = radius /. squash in
    let xr = radius *. squash in
    draw_ball ~x:draw_x ~y:draw_y ~xr ~yr ~filled:!filled fb
  end else if !y_pos <= radius then begin
    y_vel := -. !y_vel *. energy_loss;
    y_pos := radius;

    draw_ball ~x:draw_x ~y:draw_y ~xr:radius ~yr:radius ~filled:!filled fb
  end else begin
    draw_ball ~x:draw_x ~y:draw_y ~xr:radius ~yr:radius ~filled:!filled fb
  end;

  (* Stop after visual motion ends *)
  if t - !last_bounce_t > stop_after_frames then begin
    x_vel := 0.0;
    y_vel := 0.0
  end;

  (* Reset key states *)
  if not (Base.KeyCodeSet.mem Key.Space inputs.keys) then
    was_space_pressed := false; 
  if not (Base.KeyCodeSet.mem Key.X inputs.keys) then
    was_x_pressed := false;  

  fb

let () =
  Random.self_init ();
  Palette.generate_plasma_palette 18 
  |> Screen.create (int_of_float max_width) (int_of_float max_height) 1
  |> Base.run "Bounce Ball Mini Game" None tick