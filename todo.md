
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
- RayStart unnecessary
- rename MirrorBounce MirrorIntersection?
- Sightray.fromRoomAndProjectedPath is kind of hacky because recursive calls return unfinished sightrays
- Sightray.updateEndPos take a RayEnd but should maybe just take an entire Sightray
- move Sightray.segmentSamplePoints to Shared
- include angle info in MirrorBounce?
- include neighbor points in MirrorBounce?
- Sightray view attrs use default and merge with attrs passed in

## Bugs
- mouse drag position is off kilter wrt player position
- clicking during a transition starts the transition over
- room reflection animation sometimes collapses into a single point
- camera emoji is tilted wrong in reflected rooms
- time subscriptions keep firing even after a transition animation is complete?
- when you click the final animation step it gets weird
- when sightray hits a corner the bounce escapes the room





- non-pixel units
- compute sightline bounce
    - hand-compute intersection point and angle?
- is it hacky using Frame as a proxy for `(Point, Direction)`? 
    - not really, the docs pretty much literally describe Frame this way
