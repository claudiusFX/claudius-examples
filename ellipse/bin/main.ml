  open Claudius

  let tick (_t : int) (screen : Screen.t) (_prev : Framebuffer.t) (_inputs : Base.KeyCodeSet.t) : Framebuffer.t =
  let tick (_t : int) (screen : Screen.t) (_prev : Framebuffer.t) (_inputs : Base.input_state) : Framebuffer.t =
    let width, height = Screen.dimensions screen in
    let buffer = Framebuffer.init (width, height) (fun _x _y -> 0) in
  
    
    let center_x = width / 2 in
    let center_y = height / 2 in  
    let rx = 160. in
    let ry = 120. in
    let color = 15 in
    Framebuffer.draw_ellipse center_x center_y rx ry color buffer;
  
    buffer
  
  let () =
    Palette.generate_plasma_palette 18 |>
    Screen.create 640 480 1 |>
    Base.run "Basic Ellipse" None tick
  