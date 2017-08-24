public class SimpleArrayExprs {
    
    static public void f1(int[] x, int k) {
        int y = x[k] ;
        return y ;
    }
    
    static public void f2(int[] x, int k) {
        x[k] = x[k] + 1 ;
        return x[k] ;
    }
    
    
    static public void f3(int[][] x, int k) {
        int y = x[k][3] ;
        return y ;
    }
    
    static public void f4(int[][] x, int k) {
        x[k][3] = x[k][3] + 1 ;
        return x[k][3] ;
    }
}
