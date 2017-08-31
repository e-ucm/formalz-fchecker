public class MethodCalls 
{
    static int f1(int x) { x = x + 1 ; return x ; }
    
    // a function that calls f1 3x ... this should not be affected by unrolling limit
    static int f2(int y) {
        int z = f1(y) ;
        z = f1(z) ;
        z = f1(z) ;
        return z ; 
    }
    
    static int recFun(int y) {
        int z = y ;
        if (y==0) return z ;
        int z = recFun(y-1) ;
        return z ; 
    }
    
    // an example of mutual recursive function
    static int mutRecA(int y) {
        if (y==1) return y ;
        int z = mutRecB(y-2) ;
        return z ;
    }

    static int mutRecB(int y) {
        if (y==0) return y ;
        int z = mutRecA(y-1) ;
        return z ;
    }
    
    static int callLibFun1(int y) {
        int z = f1(y) ;
        System.out.println("bla") ;
        return z ;
    }
    
    static int callLibFun2(int y) {
        int z = f1(y) ;
        z = z * System.in.read() + System.in.read() ;
        System.out.println("bla") ;
        return z ;
    }

}
