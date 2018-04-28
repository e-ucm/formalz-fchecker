package nl.uu.impress.specchecker.examples;

//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.specchecker.EDSL.*;
import static nl.uu.impress.specchecker.Utils.*;

/**
 * An example of a simple program and its specification, written by a teacher.
 */
public class Swap {
	
	/**
	 * Swap two elements of an array. The indices to swap are given; they are
	 * assumed to be within the array's bound.
	 */
    public static void swap(int[] a, int i, int j) {
    	printIns(a,i,j) ;
        if (a==null || a.length == 0) throw new IllegalArgumentException();
        int temp = a[i];
        a[i] = a[j];
        a[j] = temp;
        printOuts(a) ;
    }
    
    /**
     * Teacher's specification of Swap.
     * Note: the specification also explicitly requires that elements other than
     * the i-th and j-th are left unaffected.
     */
    public static void swap_teacherspec(int[] a, int i, int j) {
        pre(a != null);
        pre(a.length > 0);
        pre(0 <= i && i < a.length);
        pre(0 <= j && j < a.length);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        int[] olda = new int[a.length] ;
        for (int k=0; k<a.length; k++) olda[k] = a[k] ;
        swap(a, i, j);
        post(a[j] == oldai);
        post(a[i] == oldaj);
        post(forall(a, k-> imp(k != i && k != j, a[k] == olda[k]))) ;
    }

}
