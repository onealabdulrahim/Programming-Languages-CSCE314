/*
** CSCE 314-599: Homework 9
** Oneal Abdulrahim

** Resources used:
** https://stackoverflow.com
** http://www.skylit.com/javamethods/faqs/javaindos.html
** https://docs.oracle.com/javase/9/
** Lecture Slides
** Ken Arnold et al, The Java Programming Language (4e)

*/

import java.util.Objects;

public class Triangle extends Shape implements Comparable<Shape>{
    private final Point A, B, C; // points
    

    public Triangle() {
        this(new Point(), new Point(), new Point());
    }

    public Triangle(Point a, Point b, Point c) {
        A = a;
        B = b;
        C = c;
    }
    
    /**
     * Implements the abstract area method from Shape, calculates the area
     * of the triangle. Uses Heron's formula:
     *      s = (a+b+c)/2
     *      A = sqrt(s*(s-a)*(s-b)*(s-c)) 
     * where a, b, c are side lengths.
     * @return The area of the triangle as a double
     */
    public double area() {
        double[] sides = {Point.distance(A, B), Point.distance(B, C), Point.distance(A, C)}; // 3 sides
        // Heron's formula
        
        double s = (sides[0] + sides[1] + sides[2]) / 2;
        return Math.sqrt(s * (s - sides[0]) * (s - sides[1]) * (s - sides[2]));
    }

    /**
     * Implements the abtract position method from Shape, calculates the
     * center point of the triangle (centroid), as recommended by the specs.
     * @return The center point of the triangle as a Point object.
     */
    public Point position() {
        // Centroid for a triangle
        // C = ((x1+x2+x3)/3 , (y1+y2+y3)/3)
        return new Point((A.x + B.x + C.x) / 3, (A.y + B.y + C.y) / 3);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Triangle ").append(A).append("-").append(B).append("-").append(C);
        
        return sb.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Triangle))
            return false;
        
        Triangle triangle = (Triangle) obj;
        return (triangle.A == this.A)
            && (triangle.B == this.B)
            && (triangle.C == this.C);
    }

    @Override
    public int hashCode() {
        return Objects.hash(A, B, C);
    }
}