public class SimpleArrayExprs {
    
    static public void f1(int[] x, int k) {
        int y = x[k] ;
        return y ;
    }
    
    static public void f2(int[] x, int k) {
        x[k] = x[k] + 1 ;
        return x[k] ;
    }
    
    static public void f2b(int[] x, int[] b, int k) {
        x[k] = x[k] + 1 ;
        b[0] = 9 ;
        return x[k] ;
    }
    
    static public void f2c(int[] x, int k) {
        bool[] b = new bool[2] ;
        x[k] = x[k] + 1 ;
        b[0] = false ;
        return x[k] ;
    }

    int[] abc ;    
    int[] x ;
    bool[] b ;
    public void f2d(int k) {
        x[k] = x[k] + 1 ;
        b[0] = false ;
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
