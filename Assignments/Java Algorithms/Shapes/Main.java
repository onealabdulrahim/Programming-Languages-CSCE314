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

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Scanner;

public class Main {

    /**
     * Takes command-line arguments and parses shapes, with no checks
     * for malformed arguments. Assume all inputs are properly formed. See
     * readme for information on command line inputs.
     * @param s         String-array of input arguments
     * @return result   Shape array with added instances
     */
    public static Shape[] parseInputs(String[] s) {
        ArrayList<Shape> result = new ArrayList<Shape>();
        
        for (String current : s) {
            switch (current.charAt(0)) {
                case 'c': Scanner circle = new Scanner(current);
                          circle.next(); // skip the char
                          result.add(new Circle(new Point(circle.nextDouble()
                                                        , circle.nextDouble())
                                              , circle.nextDouble()));
                          break;

                case 'r': Scanner rect = new Scanner(current);
                          rect.next(); // skip the char
                          result.add(new Rectangle(new Point(rect.nextDouble()
                                                           , rect.nextDouble())
                                                 , new Point(rect.nextDouble()
                                                           , rect.nextDouble())));
                          break;

                case 't': Scanner tri = new Scanner(current);
                          tri.next(); // skip the char
                          result.add(new Triangle(new Point(tri.nextDouble()
                                                          , tri.nextDouble())
                                                , new Point(tri.nextDouble()
                                                          , tri.nextDouble())
                                                , new Point(tri.nextDouble()
                                                          , tri.nextDouble())));
                          break;
            }
        }

        return result.toArray(new Shape[result.size()]);
    }
    public static void main(String[] args) {
        Shape[] shapes = parseInputs(args);
        double area = AreaCalculator.calculate(shapes);
        System.out.printf("The total area for the %d objects is %.2f units squared\n\n\n"
                        , shapes.length, area);
        
        Arrays.sort(shapes);
        int count = 0;
        for (Shape s: shapes) {
            System.out.println(++count + ") "+s+"\t\t area="+s.area());
        }
    }
}