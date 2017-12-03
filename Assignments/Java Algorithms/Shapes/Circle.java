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

public class Circle extends Shape {
    private final Point C; // center
    private final double R; // radius
    

    public Circle() {
        this(new Point(), 1.0);
    }

    public Circle(Point c, double r) {
        this.C = c;
        this.R = r;
    }
    
    /**
     * Implements the abstract area method from Shape, calculates the area
     * of the Circle.
     *      A = π * R^2
     * where R is the radius
     * @return The area of the Circle as a double
     */
    public double area() {
        return Math.PI * Math.pow(R, 2);
    }

    /**
     * Implements the abtract position method from Shape, returns the
     * center point of the Circle (centroid).
     * @return The center point of the Circle as a Point object.
     */
    public Point position() {
        return this.C;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Circle ").append(C).append(", radius = ").append(R);
        
        return sb.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Circle))
            return false;
        
        Circle circle = (Circle) obj;
        return (circle.C == this.C)
            && (circle.R == this.R);
    }

    @Override
    public int hashCode() {
        return Objects.hash(C, R);
    }
}