package nl.uu.impress.specchecker.examples;

//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.specchecker.EDSL.*;
import static nl.uu.impress.specchecker.Utils.*;

public class IsSorted {
	
	/**
	 * A program to check if an array is ascendingly sorted.
	 */
	public static boolean isSorted(int[] a) {
		printIns(a) ;
		if (a==null) throw new IllegalArgumentException() ;
		boolean ok = false ;
		if (a.length <= 1) { ok = true ; }
		else {
			for(int i=1; i<a.length; i++) 
				if (a[i-1]>a[i]) { ok = false ; break ; }
		}
		printOuts(ok) ;
		return ok ;
	}

    /**
     * Teacher's specification of isSorted.
     * Note: this specification does not explicitly require a to be left unchanged.
     */
    public static void isSorted_teacherspec(int[] a) {
    	pre(a != null) ;
    	boolean retval = isSorted(a) ;
    	post(retval == forall(a, i -> forall(a, j -> imp(i<=j, a[i] <= a[j]))));

    }
    
}
