public static class Main
{
    static int x, y;
    public static void main(String[] args) 
    {
        C c1, c2;
       /* c1 = new C(0);
        c2 = new C(1);
        c1.method1(1);*/
        c2.method1(1); 
        x = c1.c + c2.c;
        C.staticMethod();
    }
    
    
    
    public static int method() 
    {
        return 1;
    }
}

public class C
{
    int c;
    
    public C(int init)
    {
        this.c = init;
    }
    
    public static void staticMethod()
    {
        x = 4;
    }
    
    public void method1(int n)
    {
        this.c += n;
        if(this.c < 2)
            this.method1();
    }
    
    public int method2()
    {
        return 1;
    }
}