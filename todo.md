
# Todo

## Bugs
- target hit detection stops triggering after first attempt
- some rays not projecting properly
- 2 room reflection animation sometimes collapses into a single point
- 1 camera emoji is tilted wrong in reflected rooms
- 1 when sightray hits a corner the bounce escapes the room

## Features
- 2 label distance of straight portion during animation
- 3 label ray endpoint with distance / photo attempt feedback
- 1 put back the camera emoji and rotation thing
- 2 responsive
- roomitem cursor
- 1 after success animation you can drag to look around again and the hallway responds
- 1 better visual feedback, mouse hovers, cursor hand, etc
- 3 use requestanimationframe for animations

## Internal
- sightray view projectionattrs is stupid
- reusable helper for svg text
- roomitem view in focus nonesense
- Sightray.fromRoomAndProjectedPath is kind of hacky because recursive calls return unfinished sightrays
- rename MirrorBounce MirrorIntersection?
- 2 include angle info in MirrorBounce? (and neighbor points?)
- ray endpoint label in Main should move to Sightray
- mouseclickat 
- 1 make getters instead of using .accessors eg ray.startPos
- 1 settle on name "viewer" vs "player"
- 2 RoomItem is kind of pointless. emoji should just be an argument to the view. point detection can just be a function in shared
- put all the constants in the constants record or else dont use it at all

## Housekeeping
- fix git username
- publish on netlify or gh pages etc
- might need index.html and associated elm changes
- answer essay qs
- clean up code
- document code





- non-pixel units
- compute sightline bounce
    - hand-compute intersection point and angle?
- is it hacky using Frame as a proxy for `(Point, Direction)`? 
    - not really, the docs pretty much literally describe Frame this way
