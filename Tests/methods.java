public static class Main
{
    static int x, y;
    public static void main(String[] args) 
    {
        C c1 = new C();
        C c2 = new C();
        c1.method1();
        c2.method1();
        x = c1.c + c2.c;
    }
    
    
    
    public static int method() 
    {
        return 1;
    }
}

public class C
{
    int c;
    
    public C()
    {
    }
    
    public void method1()
    {
        this.c += 1;
        if(this.c < 2)
            this.method1();
    }
    
    public int method2()
    {
        return 1;
    }
}