public class Simple {

    // try for example with postcond: returnValue >= 0
    int f1(int x) { x++ ; return x ; }
    
    // try post-cond returnValue == 0
    int f2(int x) { return x++ ; }
    
    // try post-cond returnValue == 0
    int f3(int x) { return ++x ; }
    
    // An example from the thesis; try post-cond returnValue == 0
    int f4(int x) { int y = x++ - x++ ; return y ; }
    
    int a ;
    // try pcond: returnValue.a >= 0
    Simple g1(int delta) { this.a = this.a + delta ; return this ; }

    // try pcond: returnValue >= 0
    int g2(int delta) { this.a = this.a + delta ; return this.a ; }

    // try pcond: targetObj_.a >= 0
    void g3(int delta) { this.a = this.a + delta ; }

    int Seq(int x) { x = x + 1 ; x = x + 9 ; return x ; }

    int ReturnInTheMiddle(int x) { 
        x = x + 1 ; 
        if (x==0) return x ;
        x = x + 9 ; 
        return x ; }
    
}