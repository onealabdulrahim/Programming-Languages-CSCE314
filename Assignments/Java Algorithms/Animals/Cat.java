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

public class Cat extends Animal {

    public Cat(String name) {
        this.name = name;
    }

    // Returns a standard cat's cry, "meow", as a String
    public String cry() {
        return "meow";
    }

    public static void main(String[] args) {
        //Cat myCat = new Cat("Puff");
        //System.out.println(myCat.getName());
    }

}