package nl.uu.javawlp_edsl;

import static nl.uu.impress.EDSL.*;

/**
 * An example of a simple program and its specification, written by a teacher.
 */
public class GetMax {

	/**
	 * A program to obtain the maximum value of an array.
	 */
	public static int getMax(int[] a) {
        if (a.length == 0)
            throw new IllegalArgumentException();
        int m = a[0];
        for (int i = 1; i < a.length; i++)
            m = a[i] > m ? a[i] : m;
        return m;
    }
	
    /**
     * Teacher's specification of getMax.
     */
    public static void getMax_teacherspec(int[] a) {
        pre(a != null && a.length > 0);
        int retval = getMax(a);
        post(exists(a, i -> a[i] == retval) && forall(a, i -> a[i] <= retval));
    }
}
