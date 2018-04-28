package nl.uu.impress.specchecker.examples;

//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.specchecker.EDSL.*;

/**
 * Examples of incorrect specifications of the program swap that could have been written
 * by students.
 */
public class SwapStudent {
	
	public static void swap_spec1(int[] a, int i, int j) {
        pre(a != null);
        pre(a.length > 0);
        pre(i >= 0);
        pre(j >= 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        Swap.swap(a, i, j);
        post(a[j] == oldai);
        post(a[i] == oldaj);
    }
	
	 public static void swap_spec2(int[] a, int i, int j) {
	    pre(a != null && a[0] == 0);
	    pre(a != null && a.length > 0 && i >= 0 && j > 0);
	    // introducing vars to remember old values
	    int oldai = a[i], oldaj = a[j];
	    Swap.swap(a, i, j);
	    post(a[j] == oldai && a[i] == oldaj);
	}

	public static void swap_spec3(int[] a, int i, int j) {
        pre(a == null);
        pre(a.length > 0);
        pre(i >= 0);
        pre(j >= 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        Swap.swap(a, i, j);
        post(a[j] == oldai);
        post(a[i] == oldaj);
	}

    public static void swap_spec4(int[] a, int i, int j) {
        pre(a.length > 0);
        pre(i >= 0);
        pre(j >= 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        Swap.swap(a, i, j);
        post(a[j] == oldai);
        post(a[i] == oldaj);
    }

}
