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

public abstract class Animal {
    protected String name;
    protected int order;

    public String getName() {
        return this.name;
    }

    public int getOrder() {
        return this.order;
    } 

    abstract String cry();  
}