package nl.uu.impress.specchecker.examples;

//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.specchecker.EDSL.*;
import static nl.uu.impress.specchecker.Utils.*;

/**
 * An example of a simple program and its specification, written by a teacher.
 */
public class GetMax {

	/**
	 * A program to obtain the maximum value of an array. The array
	 * should contain at least one element.
	 */
	public static int getMax(int[] a) {
		printIns(a) ;
        if (a==null || a.length == 0) throw new IllegalArgumentException();
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
