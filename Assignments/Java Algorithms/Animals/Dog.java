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

public class Dog extends Animal {

    public Dog(String name) {
        this.name = name;
    }

    // Returns a standard dog's cry, "bark", as a String
    public String cry() {
        return "bark";
    }

    public static void main(String[] args) {
        //Dog myDog = new Dog("Rocky");
        //System.out.println(myDog.getName());
    }

}