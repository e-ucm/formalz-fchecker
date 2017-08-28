// Javac will refuse to compile this file because some usage of break and continue causes
// some code to be unreachable. To compile, preceed them with e.g. if(true) break ;
// I don't do this on purpose to keep the generated wlp simple, so make it easier when
// using these programs for debugging wlp.
//

public class Loops 
{

    public static void While(int N){
      int i=0 ;
      while (i<N) { i++ ; }
      i = i+9 ;
    }   
    
    public static void WhileBreak(int N){
      int i=0 ;
      int x = 0 ;
      int y = 0 ;
      while (i<N) { x=1 ; break ; y=1 ; i++ ; }
      i = i+9 ;
      x = x+10 ;
      y = y+11 ;
    }     
    
    public static void WhileContinue(int N){
      int i=0 ;
      int x = 0 ;
      int y = 0 ;
      while (i<N) { x=1 ; i++ ; continue ; y=1 ; }
      i = i+9 ;
      x = x+10 ;
      y = y+11 ;
    } 
    
    public static void WhileNestedContinue(int N){
      int i=0 ;
      int x = 0 ;
      int y = 0 ;
      while (i<N) { i++ ; while(x<N) { x++ ; continue ; y=2 ; } continue ; y=1 ; }
      i = i+9 ;
      x = x+10 ;
      y = y+11 ;
    } 

    public static void WhileWhile(int N){
      int i=0 ;
      while (i<N) { if (i==1) i = i+2 ;  i++ ; }
      int j=0 ;
      while (j<N) { i++ ; j++ ; }
      i = i+9 ;
    }
    
    public static void WhileWhileWhile(int N){
      int i=0 ;
      while (i<N) { if (i==1) i = i+2 ;  
          int k=0 ;
          while (k<N) { if (k==1) i = i+2 ; i++ ; k++ ;}
          i++ ; 
      }
      int j=0 ;
      while (j<N) { i++ ; j++ ; }
      i = i+9 ;
    }

    public static void For(int N){
      int i ;
      int x ;
      for (i=0; i<N; i++ ) { x = x+2 ; }
    }      
    
    public static void ForNested() {      
        int i ;
        for(i = 0; i < 6; i++) {
            for(int j = 0; j < 6; j++) {
                if(true) assert j == 1;
            }
        }
        int x = i;
    }
}