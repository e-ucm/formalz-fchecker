public class XField  {
    
    int x ;
    List s ;
    
    public int f1(int y) {
        this.x += y ;
        return this.x ;
    }
    
    public int f2(int y) {
        this.s.val += 1 ;
        this.s.val += y ;
        int z = this.s.val ;
        return z ;
    }
    
    public int f3(int y) {
        this.s.next.val += 1 ;
        this.s.next.val += y ;
        int z = this.s.next.val ;
        return z ;
    }
}

public class List {
    
    int val ;
    List next ;
}
