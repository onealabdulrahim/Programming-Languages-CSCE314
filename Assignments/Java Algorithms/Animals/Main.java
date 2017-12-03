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

import java.util.Scanner;

public class Main {

    /**
     * Takes command-line arguments and parses cats and dogs, with no checks
     * for malformed arguments. Assume all inputs are properly formed. See
     * readme for information on command line inputs.
     * @param s         String-array of input arguments
     * @return result   AnimalShelter object with added animals
     */
    public static AnimalShelter parseInputs(String[] s) {
        AnimalShelter result = new AnimalShelter();

        for (String current : s) {
            String[] argument = current.split(" "); // split on spaces, size 2
            if (argument[0].equals("c")) { // cat 
                Cat newCat = new Cat(argument[1]);
                result.addCat(newCat);
            } else if (argument[0].equals("d")) { // dog
                Dog newDog = new Dog(argument[1]);
                result.addDog(newDog);
            }
        }

        return result;
    }

    public static void main(String[] args) {
        // Instantiate our AnimalShelter TODO: commandline arguments
        AnimalShelter myShelter = parseInputs(args);

        // Getting user input -- initialize Scanner object
        Scanner getUserData = new Scanner(System.in);
        int userChoice;
        String menuChoices = "\n\n\n1: Add new animal                 \n"
                           + "2: Adopt an animal                \n"
                           + "3: Adopt a cat                    \n"
                           + "4: Adopt a dog                    \n"
                           + "5: Show anaimals in the shelter   \n"
                           + "6: Show cats in the shelter       \n"
                           + "7: Show dogs in the shelter       \n"
                           + "0: Exit                           \n"
                           + "Enter a number: ";

        do {
            // menu options printed to console
            System.out.print(menuChoices);

            // menu choices initialized to next input integer
            userChoice = getUserData.nextInt();

            System.out.println("\n\n\n\n"); // padding

            // act upon choice -- bunch of if-statements
            switch (userChoice) {
                case 1: System.out.println("Adding a new animal... 1.) Cat or 2.) Dog?");
                        int catOrDog = getUserData.nextInt();
                        if (catOrDog == 1) {
                            System.out.println("What's the cat's name?");
                            String name = getUserData.next();
                            Cat newCat = new Cat(name);
                            myShelter.addCat(newCat);
                        } else {
                            System.out.println("What's the dog's name?");
                            String name = getUserData.next();
                            Dog newDog = new Dog(name);
                            myShelter.addDog(newDog);
                        }
                        break;
                case 2: System.out.println("Adopting a new animal!");
                        myShelter.adopt();
                        break;
                case 3: System.out.println("Adopting a cat!");
                        myShelter.adoptCat();
                        break;
                case 4: System.out.println("Adopting a dog!");
                        myShelter.adoptDog();
                        break;
                case 5: System.out.println("Animals in the shelter: ");
                        for (Animal a: myShelter.animals) {
                            System.out.print(a.name + " ");
                        }
                        break;
                case 6: System.out.println("Cats in the shelter: ");
                        for (Animal a: myShelter.cats) {
                            System.out.print(a.name + " ");
                        }
                        break;
                case 7: System.out.println("Dogs in the shelter: ");
                        for (Animal a: myShelter.dogs) {
                            System.out.print(a.name + " ");
                        }
                        break;
                case 0: System.exit(0);
            }
        } while (userChoice >= 1 && userChoice <= 7);
        
        getUserData.close(); // let's not leak resources
    }
}