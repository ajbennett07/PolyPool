(* ::Package:: *)

(*Primitive Surface Integral*)
arr = Partition[#,10]& @ Table[RandomReal[100], {i,100}];
chunk = Pi * 0.1^2;
Total @ Flatten[chunk*arr];
(*Raycasting General Notes*)
(*
First Principles: Intersections are calculated using FindInstance[{x,y} \[Element] Object1[] && {x,y} \[Element] Object2[], {x,y}]

Generating a Ray is done with HalfLine[u,v]. Generate a v from theta with AngleVector[theta] or using column vectors.
Remember correct brackets!!!!
*)
FindInstance[{x,y} \[Element] HalfLine[{0,0}, AngleVector[Pi/4]] && {x,y} \[Element] Line[{{1,0}, {0,1}}], {x,y}];
FindInstance[{u,v} \[Element] HalfLine[{0,0}, AngleVector[Pi/4]] && {u,v} \[Element] Circle[{2,2},1], {u,v}];

(*Polygon generate*)
n = RandomInteger[{3,6}];
poly = RandomPolygon[n];
boundary = RegionBoundary[poly];
sides = Line /@ Partition[#,2,1]& @ Partition[#,2]& @ (Flatten @ (List @@ boundary));
centroid = RegionCentroid[poly];
ray = HalfLine[centroid,5*AngleVector[RandomReal[{0,2*Pi}]]];
points = Flatten @ FindInstance[{x,y}\[Element]ray && {x,y} \[Element] #,{x,y}]& /@  sides;
breakPoint = Flatten @ First @ SortBy[EuclideanDistance[centroid,#]&] @ ({x,y} /. DeleteElements[#,{{}}]& @ points);
collisionSide = Extract[#,{1}]& @ First @ SortBy[sides, RegionDistance[#,breakPoint]&];
castVec = breakPoint - centroid;
sideVec = collisionSide[[2]]-collisionSide[[1]];
angle = VectorAngle[sideVec,castVec];
newRay = 2Pi - angle;
castInfo = fullRayCast[poly, Pi/6]

rotateByAngle[{x_,y_}, theta_] := (
	{x*Cos[theta]-y*Sin[theta],x*Sin[theta]+y*Cos[theta]}
);

intersectionWithRay[line_,ray_] := (
	FindInstance[{u,v}\[Element]ray && {u,v}\[Element]line, {u,v}]
);

(*Calculates raycast from an arbitrary starting point and angle in a polygon*)
getIntersectionPolygon[poly_, point_, phi_] := Module[{currentSide,ray,sides,collisions,pointOfCollision},
	sides = Line /@ Partition[#,2,1]& @ Partition[#,2]& @ (Flatten @ (List @@ RegionBoundary[poly]));
	currentSide = Extract[#,{1}]& @ First @ SortBy[RegionDistance[#,point]&] @ sides;
	ray = HalfLine[point,AngleVector[{5,phi}]];
	collisions = Flatten @ FindInstance[{x,y}\[Element]ray && {x,y} \[Element] #,{x,y}]& /@  DeleteElements[sides,{Line@@currentSide}];
	pointOfCollision = Flatten @ First @ SortBy[EuclideanDistance[centroid,#]&] @ ({x,y} /. DeleteElements[#,{{}}]& @ collisions);
	Return @ {pointOfCollision, currentSide,point}
];

(*Returns the angle of reflection necessary for next raycast*)
calculateAngleOfReflection[point_, side_, origin_] := Module[{castVec,sideVec},
	castVec = point - origin;
	sideVec = side[[2]] - side[[1]];
	Pi - VectorAngle[sideVec,castVec]
];
