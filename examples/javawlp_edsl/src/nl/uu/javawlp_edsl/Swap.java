package nl.uu.javawlp_edsl;

import static nl.uu.impress.EDSL.*;

/**
 * An example of a simple program and its specification, written by a teacher.
 */
public class Swap {
	
	/**
	 * Swap two elements of an array.
	 */
    public static void swap(int[] a, int i, int j) {
        int temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }
    
    /**
     * Teacher's specification of Swap.
     */
    public static void swap_teacherspec(int[] a, int i, int j) {
        pre(a != null);
        pre(a.length > 0);
        pre(0 <= i && i < a.length);
        pre(0 <= j && j < a.length);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        swap(a, i, j);
        post(a[j] == oldai);
        post(a[i] == oldaj);
    }

}
