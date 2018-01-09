package nl.uu.javawlp_edsl;

import static nl.uu.impress.EDSL.*;
import static nl.uu.impress.Utils.*;

/**
 * An example of a simple program and its specification, written by a teacher.
 */
public class GetMax {

	/**
	 * A program to obtain the maximum value of an array.
	 */
	public static int getMax(int[] a) {
		printIns(a) ;
        if (a.length == 0)
            throw new IllegalArgumentException();
        int m = a[0];
        for (int i = 1; i < a.length; i++)
            m = a[i] > m ? a[i] : m;
        printOuts(m) ;
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
    

    
    // few tests
    public static void main(String[] args) throws Exception {
    	int[] a = {1} ;
    	getMax_teacherspec(a) ;
    	
    	int[] b = {1,2,2,3} ;
    	getMax_teacherspec(b) ;
    }
}
