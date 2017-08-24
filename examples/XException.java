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
        catch (IllegalArgument e) { x=1 ; }  
        catch (IOException e)     { x=0 ; }  
        x = x+1 ;
        return x ; 
    }
    
    public static int WithCatchButNotCaught(int x) {
        try {
          if (x<0) throw new IOException() ;
        }
        catch (IllegalArgument e) { x=1 ; }  
        x = x+1 ;
        return x ; 
    }
    
    public static int WithCatchFinally(int x) {
        try {
          if (x<0) throw new IOException() ;
        }
        catch (IllegalArgumentException e) { x=1 ; }  
        catch (IOException e)     { x=0 ; }
        finally { x=x+3 ; }  
        x = x+1 ;
        return x ; 
    }
    
    public static int WithCatchFinallyButNotCaught(int x) {
        try {
          if (x<0) throw new IOException() ;
        }
        catch (IllegalArgumentException e) { x=1 ; }  
        finally { x=x+3 ; }  
        x = x+1 ;
        return x ; 
    }
    
    public static int WithNestedCatch1(int x) {
        try {
          try {
             if (x<0) throw new IOException() ;
          }
          catch (IllegalArgumentException e) { x=1 ; }  
          x = 9 ;
        }
        catch (IOException e) { x=0 ; }  
        x = x+1 ;
        return x ; 
    }
    
    public static int WithNestedCatch2(int x) {
        try {
          try {
             if (x<0) throw new IOException() ;
          }
          catch (IOException e) { x=0 ; }  
          finally { x=x+4 ; }  
          x = x+9 ;
        }
        catch (IllegalArgumentException e) { x=1 ; }  
        finally { x=x+3 ; }  
        x = x+1 ;
        return x ; 
    }
    
    public static int WithNestedCatch3(int x) {
        try {
          try {
             if (x<0) throw new IOException() ;
          }
          catch (IllegalArgumentException e) { x=1 ; }  
          finally { x=x+4 ; }  
          x = 9 ;
        }
        catch (IOException e) { x=x+5 ; }  
        finally { x=x+3 ; }  
        x = x+1 ;
        return x ; 
    }
    
}
