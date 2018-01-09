package nl.uu.javawlp_edsl;

import static nl.uu.impress.EDSL.*;

/**
 * Examples of incorrect specifications of the program getMax that could have been written
 * by students.
 */
public class GetMaxStudent {

    public static void getMax_spec2(int[] a) {
        pre(a != null && a.length > 0);
        int retval = GetMax.getMax(a);
        post(forall(a, i -> a[i] <= retval));
    }

    public static void getMax_spec3(int[] a) {
        pre(a != null && a.length > 0);
        int retval = GetMax.getMax(a);
        post(exists(a, i -> a[i] == retval) && forall(a, i -> a[i] < retval));
    }
}
