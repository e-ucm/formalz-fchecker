package nl.uu.javawlp_edsl;

import static nl.uu.impress.EDSL.*;

public class IsSorted {
	
	/**
	 * A program to check is an array is sorted.
	 */
	public static boolean isSorted(int[] a) {
		if (a==null) throw new IllegalArgumentException() ;
		if (a.length <= 1) return true ;
		boolean ok = false ;
		for(int i=1; i<a.length; i++) 
			if (a[i-1]>a[i]) return false ;
		return true ;
	}

    public static void isSorted_teacherspec(int[] a) {
    	pre(a != null) ;
    	boolean retval = isSorted(a) ;
    	post(retval == forall(a, i -> forall(a, j -> imp(i<=j, a[i] <= a[j]))));

    }
    
}
