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


public abstract class Shape {
    public abstract double area();
    public abstract Point position();
    public abstract boolean equals(Object obj);
    public abstract int hashCode();
}