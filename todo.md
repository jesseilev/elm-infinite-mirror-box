
# Todo


## Features
- F1 user can scroll y / mousewheel / pinch to zoom in and out
- sightline is truncated to short preview length
- user can click to fire a photon
- when user clicks to fire, an animation plays as it bounces along the bounce path
- target is rendered on the screen
- when user successfully hits target, success animation plays

## Internal
- compute 

## Bugs


## Feature F1
### Bugs
- various computations get messed up when `zoomScale =/= 1`
    - user position
    - pivot point for computing mouse drag angle
- 




- non-pixel units
- compute sightline bounce
    - hand-compute intersection point and angle?
- is it hacky using Frame as a proxy for `(Point, Direction)`? 
    - not really, the docs pretty much literally describe Frame this way
