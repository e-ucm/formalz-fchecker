package nl.uu.impress.specchecker.examples;

//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.specchecker.EDSL.*;
import static nl.uu.impress.specchecker.Utils.*;

public class GetMinIndex {
	
	/**
	 * A program to return the first index of a minimum element of an array. The
	 * array should have at least one element.
	 */
	public static int getMinIndex(int[] a) {
		printIns(a) ;
		if (a==null || a.length == 0) throw new IllegalArgumentException();
		int min = a[0] ;
		int minIndex = 0 ;
		int N = a.length ;
		if (N>1) {
			for (int k=1; k<a.length; k++) {
				if (a[k] < min) { // we find a smaller value than the one found so far
					min = a[k] ;
					minIndex = k ;
				}
			}
		}
		printOuts(minIndex) ;
		return minIndex ;
	}

    /**
     * Teacher's specification of getMinIndex.
     * Note: this specification does not explicitly require a to be left unchanged.
     */
    public static void getMinIndex_teacherspec(int[] a) {
        pre(a != null && a.length > 0);
        int retval = getMinIndex(a);
        post(0 <= retval && retval < a.length) ;
        post(forall(a, i -> a[retval] <= a[i])) ;
        post(forall(a, i -> imp(a[i]==a[retval], retval <= i))) ;
    }
	
}
