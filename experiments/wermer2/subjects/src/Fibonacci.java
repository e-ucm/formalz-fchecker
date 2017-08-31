import java.io.IOException;

// http://www.sanfoundry.com/csharp-program-generate-fibonocci-series/
// Program to Generate Fibonacci Series
// It reads input from user inputs!
 
class Fibonacci
{
    static void fibonacciInteractive() throws IOException
    {
        int i, count, f1 = 0, f2 = 1, f3 = 0;
        while (true) {
          System.out.print("Enter the Limit : ");
          count = System.in.read() ;
          if (count>6) System.out.println("Too large.") ;
          else if (count==0) System.out.println("Cannot be 0.") ;
          else break ;
        }
        System.out.println(f1);
        System.out.println(f2);
        for (i = 0; i <= count; i++){
           f3 = f1 + f2;
           System.out.println("f3=" + f3);
           f1 = f2;
           f2 = f3;
        }
        System.in.read();
    }
    

}