public class XException {
    
    public static int Uncaught(int x) {
        if (x<0) throw new IllegalArgumentException() ;
        x = x+1 ;
        return x ; 
    }
    
    public static int WithCatch(int x) {
        try {
          if (x<0) throw new IOException() ;
        }
        catch (IOException e) { x=0 ; }  
        x = x+1 ;
        return x ; 
    }
    
    
}
