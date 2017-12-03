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

import java.util.Queue;
import java.util.LinkedList;

public class AnimalShelter {
    public Queue<Animal> animals;
    public Queue<Animal> cats;
    public Queue<Animal> dogs;

    // Default constructor initializes Queues. Unpreferred
    public AnimalShelter() {
        animals = new LinkedList<Animal>();
        cats = new LinkedList<Animal>();
        dogs = new LinkedList<Animal>();
    }
    
    // Constructor initializes Queues as LinkedLists & adds first animal
    public AnimalShelter(Animal first) {
        animals = new LinkedList<Animal>();
        cats = new LinkedList<Animal>();
        dogs = new LinkedList<Animal>();

        animals.add(first);
        if (first instanceof Cat) {
            cats.add(first);
        } else {
            dogs.add(first);
        }
    }

    /**
     * Adds a cat to the proper queue
     * @param   Cat Instance of Cat to be added
     * @return
     */
    public void addCat(Cat c) {
        animals.add(c);
        cats.add(c);
    }

    /**
    * Adds a dog to the proper queue
    * @param   Dog Instance of dog to be added
    * @return
    */
    public void addDog(Dog d) {
        animals.add(d);
        dogs.add(d);
    }

    /**
    * "Adopts" oldest animal (FIFO), "popping" it from the proper queues.
    * @param
    * @return
    */
    public void adopt() {
        if (animals.isEmpty()) {
            System.out.println("No animals left to adopt!!");
            return;
        }
        Animal adopted = animals.remove();
        if (adopted instanceof Cat) {
            System.out.println("Meet your new cat, " + adopted.getName());
        } else {
            System.out.println("Meet your new dog, " + adopted.getName());
        }   
    }

    /**
     * Adopts a cat, removing it from the proper queues. If head of the general
     * animal queue is also a cat, pop it from that queue.
     * @param
     * @return
     */
    public void adoptCat() {
        if (cats.isEmpty()) {
            System.out.println("No cats left to adopt!!");
            return;
        }
        if (animals.peek() instanceof Cat) {
            animals.remove();
        }
        System.out.println("Meet your new cat, " + cats.remove().getName());
    }

    /**
     * Adopts a dog, removing it from the proper queues. If head of the general
     * animal queue is also a dog, pop it from that queue.
     * @param
     * @return
     */
    public void adoptDog() {
        if (dogs.isEmpty()) {
            System.out.println("No dogs left to adopt!!");
            return;
        }
        if (animals.peek() instanceof Dog) {
            animals.remove();
        }
        System.out.println("Meet your new dog, " + dogs.remove().getName());
    }

    /**
     * Getter methods for Shelter parameters. Returns the respective sizes
     * of the corresponding queues.
     * @param
     * @return int  The size of the given queue.
     */
    public int remainingAnimals() {return animals.size();}
    public int remainingCats() {return cats.size();}
    public int remainingDogs() {return dogs.size();}
}