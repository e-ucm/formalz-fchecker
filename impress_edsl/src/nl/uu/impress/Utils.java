package nl.uu.impress;

import java.util.*;

public class Utils {
	
	private static void printArgs(String s, Object[] x) {
		if (x==null) return ;
		for (int i=0; i<x.length; i++) {
			if (i>0) s += "," ;
			Object o = x[i] ;
			if (o==null) { s += " null" ; continue ; }
			if (o instanceof Integer) s += show((Integer) o ) ;
			else if (o instanceof Boolean) s += show((Boolean) o ) ;
			else if (o instanceof Double) s += show((Double) o ) ;
			else if (o instanceof String) s += show((String) o ) ;
			else if (o instanceof int[][]) s += showArray2((int[][]) o ) ;
			else if (o instanceof boolean[][]) s += showArray2((boolean[][]) o ) ;
			else if (o instanceof double[][]) s += showArray2((double[][]) o ) ;
			else if (o instanceof String[][]) s += showArray2((String[][]) o ) ;
			else if (o instanceof int[]) s += showArray1((int[]) o ) ;
			else if (o instanceof boolean[]) s += showArray1((boolean[]) o ) ;
			else if (o instanceof double[]) s += showArray1((double[]) o ) ;
			else if (o instanceof String[]) s += showArray1((String[]) o ) ;
			else if (o instanceof Set) s += showSet((Set) o) ;
		}
		System.err.println(s);
		
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
	
	private static String show(Integer a) { return a.toString() ; }
	private static String show(Boolean a) { return a.toString() ; }
	private static String show(Double a) { return a.toString() ; }
	private static String show(String a) { return "\"" + a + "\"" ; }
	

	public static void printIns(Object... x) {
		printArgs("IN:",x) ;
	}
	
	public static void printOuts(Object... x) {
		printArgs("OUT:",x) ;
	}

	public static void main(String[] args) {
		int[][] a = {{0,1},{1}} ;
		printIns(a) ;
		
		Set<Integer> S = new HashSet<Integer>() ;
		S.add(1) ; S.add(2) ; S.add(3) ;
		printIns(S) ;
	}
	
}
