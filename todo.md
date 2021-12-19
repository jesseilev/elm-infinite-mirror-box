
# Todo


## Features
- render success animation 
    - subtasks
- item collision detection
- ray interrupted on item collision
- when user successfully hits target, success animation plays
- label angles
- multple room arrangement challenges
- when you complete one room, it moves to the next
- angle subtending
- make styles match brilliant styleguide
- text prompt
- cursor shows drag-to-rotate thingy
- sightpath animated the dotted line towards the player

## Internal
- compute reflection bounce path
- 
- model things so that the link between bounce path segments and sightline segments is organic and first class or something. this is too vague to be a todo other than like "spend some time thinking about this"
- 

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
