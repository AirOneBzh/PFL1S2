open Point;;

let infc p1 p2 = if p1.y < p2.y then true else if p1.y == p2.y && p1.x <= p2.x then true else false;;
let det q0 q1 q2 = (q1.x - q0.x)*(q2.y - q0.y) - (q2.x - q0.x) * (q1.y - q0.y);;

let sca q0 q1 q2 = (q1.x - q0.x)*(q2.x - q0.x) + (q1.y - q0.y)*(q2.y - q0.y);;

let infg w p1 p2 = if p1 == w || p1 == p2 || (p1 <> w && p2 <> w && p1 <> p2 && (det w p1 p2) > 0) || (p1 <> w && p2 <> w && p1 <> p2 && (det w p1 p2) == 0 && (sca p1 w p2) < 0) then true else false;;
