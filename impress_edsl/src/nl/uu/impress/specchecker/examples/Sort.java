package nl.uu.impress.specchecker.examples;

//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.specchecker.EDSL.*;
import static nl.uu.impress.specchecker.Utils.*;

public class Sort {
	
	/**
	 * To sort an array ascendingly.
	 */
	public static void sort(int[] a) {
		printIns(a) ;
        if (a==null) throw new IllegalArgumentException();
		for (int j=0; j<a.length ; j++) {
			int minIndex = getSmallest(a,j) ;
			// now swap:
			if (j != minIndex) {
				int tmp = a[j] ;
				a[j] = a[minIndex] ;
				a[minIndex] = tmp ;
			}
		}
		printOuts(a) ;
	}
	
	private static int getSmallest(int[] a, int left) {
		int min = a[left] ;
		int minIndex = left ;
		while(left < a.length) {
			if (a[left] < min) {
				min = a[left] ;
				minIndex = left ;
			}
			left++ ;
		}
		return minIndex ;
	}
	
    /**
     * Teacher's specification of sort.
     * Note: the post-condition is left incomplete. It does not require that an element
     * x that occurs k times in the old-a, should also appear k times in the new a.
     */
    public static void sort_teacherspec(int[] a) {
        pre(a != null);
        
        int[] olda = new int[a.length] ;
        for (int k=0; k<a.length; k++) olda[k] = a[k] ;
        sort(a);
        
        // the final a should be sorted:
        post(forall(a, i-> forall(a, j -> imp(i<=j, a[i]<=a[j])))) ;
        
        post(a != null) ;
        post(a.length == olda.length) ;
        post(forall(a, i-> exists(olda, j -> a[i]==olda[j]))) ;
        post(forall(olda, i-> exists(a, j -> olda[i]==a[j]))) ;
    }
    
    // few tests
    public static void main(String[] args) throws Exception {
    	int[] a = {1} ;
    	sort(a) ;
    	
    	int[] b = {2,3,2,1} ;
    	sort(b) ;
    }

}
