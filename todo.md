
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
- rename MirrorBounce MirrorIntersection?
- Sightray.fromRoomAndProjectedPath is kind of hacky because recursive calls return unfinished sightrays
- include angle info in MirrorBounce?
- include neighbor points in MirrorBounce?
- Sightray view attrs use default and merge with attrs passed in
- Room should not have state / update
- Room model should not include viewerDirection, status, sightDistance
- Room model should have playerItem instead of playerPos, targetItem instead of targetPos
- Room.projectedSightline can live in parent module
- Main.updatePhotoAttempt is checking itemHit itself, rather than looking at RayEnd
- angle arc view stuff in Main should move to Sightray
- ray endpoint label in Main should move to Sightray
- make getters instead of using .accessors eg ray.startPos
- settle on name "viewer" vs "player"
- RoomItem is kind of pointless. emoji should just be an argument to the view. point detection can just be a function in shared

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
