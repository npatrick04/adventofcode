import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;
import java.util.Vector;

class j1209 {

    // Part 1
    static int predictNext(Vector<Integer> seq) {
        boolean allZeros = true;
        //System.out.println(seq.toString());
        Vector<Integer> nextDown = new Vector<Integer>();
        for (int i = 1; i < seq.size(); i++) {
            int next = seq.get(i) - seq.get(i-1);
            nextDown.add(next);
            if (next != 0) {
                allZeros = false;
            }
        }
        if (allZeros) {
            return seq.lastElement();
        }
        else {
            int next = predictNext(nextDown);
            int result = seq.lastElement() + next; 
            //System.out.printf("Last: %d, next: %d, Return: %d%n", seq.lastElement(), next, result);
            return result;
        }   
    }

    // Part 2
    static int predictNextReverse(Vector<Integer> seq) {
        boolean allZeros = true;
        //System.out.println(seq.toString());
        Vector<Integer> nextDown = new Vector<Integer>();
        for (int i = 1; i < seq.size(); i++) {
            int next = seq.get(i) - seq.get(i-1);
            nextDown.add(next);
            if (next != 0) {
                allZeros = false;
            }
        }
        if (allZeros) {
            return seq.firstElement();
        }
        else {
            int next = predictNextReverse(nextDown);
            int result = seq.firstElement() - next; 
            //System.out.printf("Last: %d, next: %d, Return: %d%n", seq.firstElement(), next, result);
            return result;
        }   
    }

    public static void main(String[] args) {
        BufferedReader reader;
        System.out.println("Java 12/9 advent of code.");
        try {
            reader = new BufferedReader(new FileReader("input"));
            String line = reader.readLine();

            Vector<Vector<Integer>> gameData = new Vector<Vector<Integer>>();

            while (line != null) {
                Scanner scn = new Scanner(line);
                Vector<Integer> rowVector = new Vector<Integer>();

                while (scn.hasNext()) {
                    rowVector.add(scn.nextInt());
                }
                gameData.add(rowVector);
                scn.close();
                line = reader.readLine();
            }
            //System.out.println(gameData.toString());
            reader.close();

            int result = gameData
            .stream()
            .map(seq->predictNextReverse(seq))
            .mapToInt(i->i)
            .sum();

            System.out.printf("Result: %d%n", result);

       } catch (FileNotFoundException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
        } catch (IOException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
    }

     }
}