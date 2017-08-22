//https://examples.javacodegeeks.com/java-basics/arrays-java-basics/java-string-array-example/

// package com.javacodegeeks.javabasics.stringarray;

public class Arrays3 {

	public static void foo() {

		// declare a string array with initial size
		String[] schoolbag = new String[4];

		// add elements to the array
		schoolbag[0] = "Books";
		schoolbag[1] = "Pens";
		schoolbag[2] = "Pencils";
		schoolbag[3] = "Notebooks";

		// this will cause ArrayIndexOutOfBoundsException
		// schoolbag[4] = "Notebooks";

		// declare a string array with no initial size
		// String[] schoolbag;

		// declare string array and initialize with values in one step
		String[] schoolbag2 = new String[4];
		schoolbag2[0] = "Books";
		schoolbag2[1] = "Pens";
		schoolbag2[2] = "Pencils";
		schoolbag2[3] = "Notebooks";

		// print the third element of the string array
		System.out.println("The third element is: " + schoolbag2[2]);

		// iterate all the elements of the array
		int size = schoolbag2.length;
		System.out.println("The size of array is: " + size);
		for (int i = 0; i < size; i++) {
			System.out.println("Index[" + i + "] = " + schoolbag2[i]);
		}

	}

}