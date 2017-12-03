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

public class Rectangle extends Shape {
    private final Point A, B; // top-left and bottom-right
    

    public Rectangle() {
        this(new Point(), new Point());
    }

    public Rectangle(Point a, Point b) {
        A = a;
        B = b;
    }
    
    /**
     * Implements the abstract area method from Shape, calculates the area
     * of the rectangle.
     *      A = a * b
     * where a, b are side lengths
     * @return The area of the rectangle as a double
     */
    public double area() {
        double[] sides = {Point.distance(A, new Point(B.x, A.y)) // l
                        , Point.distance(new Point(B.x, A.y), B)}; // w
        return sides[0] * sides[1];
    }

    /**
     * Implements the abtract position method from Shape, calculates the
     * center point of the Rectangle (centroid), as recommended by the specs.
     * @return The center point of the Rectangle as a Point object.
     */
    public Point position() {
        return new Point((A.x + B.x) / 2, (A.y + B.y) / 2);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Rectangle ").append(A).append("-").append(B);
        
        return sb.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Rectangle))
            return false;
        
        Rectangle rectangle = (Rectangle) obj;
        return (rectangle.A == this.A)
            && (rectangle.B == this.B);
    }

    @Override
    public int hashCode() {
        return Objects.hash(A, B);
    }
}