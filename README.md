- Use an IndexedSeq to sort the coordinates by x and y.
- import scala.collection.Searching.search is an implicit class on things that are Seq like
- Use an Ordering to sort the IndexedSeq. Do this twice, once for x and once for y

- Searching should specify whether the searched point coincides with a hole
- search method returns either a Found or an InsertionPoint. 
  - recieving a found implies the indexed Seq



1. Take boundary and simplify a lot
2. Densify the simplified boundary
3. Filter points that touch the boundary (shouldn't be any)
4. Take the new densified points
5. Extend each point in the 4 natural basis directions
6. Check to see if the intersection of the projected line and the original polygon is 1 dimensional
7. If at least two of the extended lines have only trivial intersection with the boundary, then keep this point and the extended lines.
8. Form the rectangle from these extended lines
9. Find the union of all rectangles formed this way. Call this polygon p*
10. Find the difference between the OrthogonalPolygon and p*
