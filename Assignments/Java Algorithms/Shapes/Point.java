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

public class Point{
    public double x;
    public double y;

    public Point() {
        this(0, 0);
    }

    public Point (double x, double y) {
        this.x = x;
        this.y = y;
    }
    
    /**
     * Finds a distance between two points over 2D coordinates
     * @param a     The first point
     * @param b     The second point
     * @return      The distance between the points as a double
     */
    public static double distance(Point a, Point b) {
        return Math.sqrt(Math.pow((b.x - a.x), 2) + Math.pow((b.y - a.y), 2));
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append('(').append(x).append(", ").append(y).append(')');
        return sb.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Point))
            return false;
        
        Point point = (Point) obj;
        return (point.x == this.x) && (point.y == this.y);
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }
}