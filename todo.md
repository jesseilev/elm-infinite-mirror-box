
# Todo


## Features
- 1 show beam in animation
- 1 convey success better
- 2 split each animation step into 2 substeps
- 2 label distance of straight portion during animation
- 2 animation only shows one angle at a time
- 3 label ray endpoint with distance / photo attempt feedback
- 4 multple room arrangement challenges
- 2 responsive
- 1 better visual feedback, mouse hovers, cursor hand, etc
- 1 sightpath animates the dotted line towards the player
- 1 put back the camera emoji and rotation thing
- 3 use requestanimationframe for animations
- 1 animation easing functions 
- 1 after success animation you can drag to look around again and the hallway responds

## Internal
- 1 Diagram module
- rename MirrorBounce MirrorIntersection?
- Sightray.fromRoomAndProjectedPath is kind of hacky because recursive calls return unfinished sightrays
- 2 include angle info in MirrorBounce? (and neighbor points?)
- 1 Main.updatePhotoAttempt is checking itemHit itself, rather than looking at RayEnd
- ray endpoint label in Main should move to Sightray
- 1 make getters instead of using .accessors eg ray.startPos
- 1 settle on name "viewer" vs "player"
- 2 RoomItem is kind of pointless. emoji should just be an argument to the view. point detection can just be a function in shared

## Bugs
- 1 clicking during a transition starts the transition over
- 2 room reflection animation sometimes collapses into a single point
- 1 camera emoji is tilted wrong in reflected rooms
- 1 when you click the final animation step it gets weird
- 1 when sightray hits a corner the bounce escapes the room





- non-pixel units
- compute sightline bounce
    - hand-compute intersection point and angle?
- is it hacky using Frame as a proxy for `(Point, Direction)`? 
    - not really, the docs pretty much literally describe Frame this way
