
# Todo


## Features
- label distance of straight portion during animation
- label distance during regular
- only label angles that aren't on top of each other
- multple room arrangement challenges
- when you complete one room, it moves to the next
- text feedback when you take a pic and fail
- responsive
- better visual feedback, mouse hovers, cursor hand, etc
- cursor shows drag-to-rotate thingy
- sightpath animates the dotted line towards the player
- make styles match brilliant styleguide
- angle subtending
- use requestanimationframe for animations

## Internal
- compute reflection bounce path
- 
- model things so that the link between bounce path segments and sightline segments is organic and first class or something. this is too vague to be a todo other than like "spend some time thinking about this"
- 

## Bugs
- mouse drag position is off kilter wrt player position
- clicking during a transition starts the transition over
- room reflection animation sometimes collapses into a single point
- camera emoji is tilted wrong in reflected rooms
- time subscriptions keep firing even after a transition animation is complete?
- when you click the final animation step it gets weird
- when sightray hits a corner the bounce escapes the room

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
