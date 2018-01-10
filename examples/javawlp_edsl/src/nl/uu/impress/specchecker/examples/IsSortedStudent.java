package nl.uu.impress.specchecker.examples;

//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.specchecker.EDSL.*;

/**
 * Examples of specifications of the program isSorted that could have been written
 * by students. Mosy of the are incorrects. Spec4 looks different that the teacher's version,
 * but it should be equivalent.
 */
public class IsSortedStudent {

    public static void isSorted_spec1(int[] a) {
    	pre(a != null) ;
    	boolean retval = IsSorted.isSorted(a) ;
    	post(retval == forall(a, i -> forall(a, j -> imp(i<=j, a[i] < a[j]))));

    }

    public static void isSorted_spec2(int[] a) {
    	pre(a != null) ;
    	boolean retval = IsSorted.isSorted(a) ;
    	post(retval == forall(a, i -> forall(a, j -> imp(i<=j, a[i] >= a[j]))));

    }

    public static void isSorted_spec3(int[] a) {
    	pre(a != null) ;
    	boolean retval = IsSorted.isSorted(a) ;
    	post(retval == forall(a, i -> a[i-1] <= a[i]));
    }
    
    public static void isSorted_spec4(int[] a) {
    	pre(a != null) ;
    	boolean retval = IsSorted.isSorted(a) ;
    	post(retval == forall(a, i -> imp(0<i,a[i-1] <= a[i]))) ;
    }
    
    // few tests
    public static void main(String[] args) {
    	int[] a = { 0,1,1 } ;
    	
    }

}
