// http://www.sanfoundry.com/csharp-program-generate-fibonocci-series/

/*
 * C#  Program to Generate Fibonacci Series
 */
 
class Program
{
    static void main(String[] args)
    {
        int i, count, f1 = 0, f2 = 1, f3 = 0;
        System.out.print("Enter the Limit : ");
            count = System.in.read();
            System.out.println(f1);
            System.out.println(f2);
            for (i = 0; i <= count; i++)
            {
                f3 = f1 + f2;
                System.out.println(f3);
                f1 = f2;
                f2 = f3;
            }
            System.in.read();
 
    }
}