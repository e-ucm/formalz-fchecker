//https://www.tutorialspoint.com/java/java_arrays.htm

public class TestArray {

   public static void main(String[] args) {
      double[] myList = new double[4];
      myList[0] = 1.9;
      myList[1] = 2.9;
      myList[2] = 3.4;
      myList[3] = 3.5;

      // Print all the array elements
      for (int i = 0; i < myList.length; i++) {
         System.out.println(myList[i] + " ");
      }
     
      // Summing all elements
      double total = 0;
      for (int j = 0; j < myList.length; j++) {
         total += myList[j];
      }
      System.out.println("Total is " + total);
      
      // Finding the largest element
      double max = myList[0];
      for (int k = 1; k < myList.length; k++) {
         if (myList[k] > max) max = myList[k];
      }
      System.out.println("Max is " + max);  
   }
}