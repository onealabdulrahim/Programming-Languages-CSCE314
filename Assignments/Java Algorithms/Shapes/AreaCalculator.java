/*
** CSCE 314-599: Homework 9
** Oneal Abdulrahim

** Resources used:
** https://stackoverflow.com
** http://www.skylit.com/javamethods/faqs/javaindos.html
** https://docs.oracle.com/javase/9/
** Lecture Slides
** Ken Arnold et al, The Java Programming Language (4e)

D:\OneDrive\Documents\TAMU\"Computer Science"\"CSCE 314"\Assignments\"Homework 9" */

// Specs call for separate class with only one static method.
public class AreaCalculator {
    public static double calculate(Shape[] shapes) {
        double result = 0;
        for (Shape s : shapes) {
            result += s.area();
        }
        return result;
    }
}