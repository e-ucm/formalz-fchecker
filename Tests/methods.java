public static class Main
{
    static int x;
    public static void main(String[] args) 
    {
        C1 c1 = new C1();
        c1.c += 2; //c1.method1();
        x = c1.c;
    }
    
    
    
    public static void method() 
    {
    }
}

public class C1
{
    int c;
    
    public C1()
    {
    }
    
    public void method1()
    {
     //  c++;
    }
}