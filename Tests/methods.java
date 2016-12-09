public static class Main
{
    static int x, y;
    public static void main(String[] args) 
    {
        C1 c1 = new C1();
        c1.method1();
        c1.method1();
        x = c1.c;
    }
    
    
    
    public static int method() 
    {
        return 1;
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
        this.c += 1;
    }
    
    public int method2()
    {
        return 1;
    }
}