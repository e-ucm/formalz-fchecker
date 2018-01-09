package nl.uu.impress;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

public class Utils {
	
	private static String showArgs(String s, Object[] x) {
		if (x==null) return s ;
		for (int i=0; i<x.length; i++) {
			if (i>0) s += "," ;
			Object o = x[i] ;
			if (o==null) { s += " null" ; continue ; }
			if (o instanceof Integer) s      += ((Integer) o ).toString() ;
			else if (o instanceof Boolean) s += ((Boolean) o ).toString() ;
			else if (o instanceof Double) s  += ((Double) o ).toString() ;
			else if (o instanceof String) s  += "\"" + ((String) o ).toString() + "\"" ;
			else if (o instanceof int[][]) s += showArray2((int[][]) o ) ;
			else if (o instanceof boolean[][]) s += showArray2((boolean[][]) o ) ;
			else if (o instanceof double[][]) s  += showArray2((double[][]) o ) ;
			else if (o instanceof String[][]) s  += showArray2((String[][]) o ) ;
			else if (o instanceof int[]) s += showArray1((int[]) o ) ;
			else if (o instanceof boolean[]) s += showArray1((boolean[]) o ) ;
			else if (o instanceof double[]) s += showArray1((double[]) o ) ;
			else if (o instanceof String[]) s += showArray1((String[]) o ) ;
			else if (o instanceof Set) s += showSet((Set) o) ;
		}
		return s ;
	}
	
	private static String showWorker(Object[] a) {
		String s = "" ;
		for (int i = 0; i < a.length; i++) {
			if (i>0) s += "," ;
			if (a[i]==null) s += "null" ;
			else s += a[i] ;
		}
		return s ;
	}
	
	private static String showArray1(int[] a) {
		Object[] a_ = new Object[a.length] ;
		for (int i = 0; i<a.length; i++) a_[i] = a[i] ;
		return "[" + showWorker(a_) + "]" ;
	}
	
	private static String showArray1(boolean[] a) {
		Object[] a_ = new Object[a.length] ;
		for (int i = 0; i<a.length; i++) a_[i] = a[i] ;
		return "[" + showWorker(a_) + "]" ;
	}

	private static String showArray1(double[] a) {
		Object[] a_ = new Object[a.length] ;
		for (int i = 0; i<a.length; i++) a_[i] = a[i] ;
		return "[" + showWorker(a_) + "]" ;
	}
	
	private static String showArray1(String[] a) {
		Object[] a_ = new Object[a.length] ;
		for (int i = 0; i<a.length; i++) a_[i] = a[i] ;
		return "[" + showWorker(a_) + "]" ;
	}
	
	private static String showWorker2(Object[][] a) {
		String s = "[" ;
		for (int i = 0; i < a.length; i++) {
			if (i>0) s += "," ;
			if (a[i]==null) s += "null" ;
			else s += "[" + showWorker(a[i]) + "]" ;
		}
		s += "]" ;
		return s ;
	}
	
	private static String showSet(Set a) { return "{" + showWorker(a.toArray() ) + "}" ; }
	
	private static String showArray2(int[][] a) {
		Object[][] a_ = new Object[a.length][] ;
		for (int i = 0; i<a.length; i++) {
			if (a[i] == null) a_[i] = null ;
			else {
				int[] x = a[i] ;
				a_[i] = new Object[x.length] ;
				for (int k=0; k < x.length; k++) a_[i][k] = x[k] ;
			}
		}
		return showWorker2(a_)  ;
	}
	
	
	private static String showArray2(boolean[][] a) {
		Object[][] a_ = new Object[a.length][] ;
		for (int i = 0; i<a.length; i++) {
			if (a[i] == null) a_[i] = null ;
			else {
				boolean[] x = a[i] ;
				a_[i] = new Object[x.length] ;
				for (int k=0; k < x.length; k++) a_[i][k] = x[k] ;
			}
		}
		return showWorker2(a_)  ;
	}
	
	private static String showArray2(double[][] a) {
		Object[][] a_ = new Object[a.length][] ;
		for (int i = 0; i<a.length; i++) {
			if (a[i] == null) a_[i] = null ;
			else {
				double[] x = a[i] ;
				a_[i] = new Object[x.length] ;
				for (int k=0; k < x.length; k++) a_[i][k] = x[k] ;
			}
		}
		return showWorker2(a_)  ;
	}
	
	private static String showArray2(String[][] a) {
		Object[][] a_ = new Object[a.length][] ;
		for (int i = 0; i<a.length; i++) {
			if (a[i] == null) a_[i] = null ;
			else {
				String[] x = a[i] ;
				a_[i] = new Object[x.length] ;
				for (int k=0; k < x.length; k++) a_[i][k] = x[k] ;
			}
		}
		return showWorker2(a_)  ;
	}

	/**
	 * For printing input-vectors.
	 */
	public static void printIns(Object... x) { System.err.println(showArgs("IN:",x)) ; }

	/**
	 * For printing output-vectors.
	 */
	public static void printOuts(Object... x) { System.err.println(showArgs("OUT:",x)) ; }
	
    /**
     * A generic method to invoke a specification. A specification is assumed to be a public
     * static method, that returns void.
     * @param cname  the full name of the class containing the specification.
     * @param mname  the name of the method that represents the specification.
     * @param args   objects to be passed as arguments to the specification.
     * @throws ClassNotFoundException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InvocationTargetException
     */
    public static void invokeSpec(String cname, String mname, Object ... args) 
    		throws ClassNotFoundException, IllegalAccessException, IllegalArgumentException, InvocationTargetException 
    {
    	Class C = Class.forName(cname) ;
    	Method[] ms = C.getMethods() ;
    	for (Method m : ms) {
    		if (m.getName() == mname) {
    			m.invoke(null,args) ;
    		}
    	}
    }

	public static void main(String[] args) throws Exception {
		int[][] a = {{0,1},{1}} ;
		printIns(a) ;
		
		Set<Integer> S = new HashSet<Integer>() ;
		S.add(1) ; S.add(2) ; S.add(3) ;
		printIns(S) ;
		
		int[] b = {1,2,2,3} ;
    	invokeSpec("nl.uu.javawlp_edsl.GetMax","getMax_teacherspec",b) ;
	}
	
}
