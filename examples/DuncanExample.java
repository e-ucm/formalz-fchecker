public class DuncanExample {
    
    boolean f1(int x, int y) {
        return !(x>0) && (x > -2) ;
    }

    boolean f1_(int x, int y) {
        return !(x>0) && x > -2;
    }
    
    boolean f2(int x, int y) {
        return !(x>0) && (x > 0) ;
    }

    boolean f2_(int x, int y) {
        return !(x>0) && x > 0 ;
    }
    
    boolean f3(int[] a, int k) {
        return (k>9) && (a[k]>0) && (a[k] == a[k+1]) ;
    }
    
    boolean f3_(int[] a, int k) {
        return k>9 && a[k]>0 && a[k] == a[k+1] ;
    }
}
