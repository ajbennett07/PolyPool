#!/usr/bin/env wolframscript
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
angle = VectorAngle[sideVec,castVec]
newRay = 2Pi - angle;
$RecursionLimit
RaycastPolygon[poly,breakPoint,newRay,0]
Graphics[{White,poly,Red,Point[centroid],Black,Thickness-> 0.005, boundary, Blue, Point[breakPoint]}]




RaycastPolygon[poly_, point_, phi_, recur_] := Module[{currentSide,ray,sides,collisions,pointOfCollision,collisionSide,castVec,sideVec},
	sides = Line /@ Partition[#,2,1]& @ Partition[#,2]& @ (Flatten @ (List @@ RegionBoundary[poly]));
	currentSide = Extract[#,{1}]& @ First @ SortBy[RegionDistance[#,point]&] @ sides;
	ray = HalfLine[point,10*AngleVector[phi]];
	collisions = Flatten @ FindInstance[{x,y}\[Element]ray && {x,y} \[Element] #,{x,y}]& /@  DeleteElements[sides,{Line@@currentSide}];
	pointOfCollision = Flatten @ First @ SortBy[EuclideanDistance[centroid,#]&] @ ({x,y} /. DeleteElements[#,{{}}]& @ collisions);
	collisionSide = Extract[#,{1}]& @ First @ SortBy[sides, RegionDistance[#,pointOfCollision]&];
	castVec = pointOfCollision - point;
	sideVec = collisionSide[[2]]-collisionSide[[1]];
	If[recur<20, Return @
	{{pointOfCollision,collisionSide}, RaycastPolygon[poly,pointOfCollision,2Pi-VectorAngle[sideVec,castVec],recur+1]},
	{}]
];

(*
	collisions = Flatten @ FindInstance[{x,y}\[Element]ray && {x,y} \[Element] #,{x,y}]& /@  (Delete[sides,currentSide]);
	pointOfCollision = Flatten @ First @ SortBy[EuclideanDistance[centroid,#]&] @ ({x,y} /. DeleteElements[#,{{}}]& @ collisions);
	collisionSide = Extract[#,{1}]& @ First @ SortBy[sides, RegionDistance[#,pointOfCollision]&];
	castVec = pointOfCollision - point;
	sideVec = collisionSide[[2]]-collisionSide[[1]];
	{{pointOfCollision,collisionSide}, RaycastPolygon[poly,pointOfCollision,2Pi-VectorAngle[sideVec,castVec],recur+1]}
*)



