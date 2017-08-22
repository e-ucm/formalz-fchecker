public class Methods
{
    int c ;

    public Methods(int init) { this.c = init; }

    public int method1() { return 1; }

    public void method2(int n, boolean b) {
        assert b;
        this.c += n;
        if(this.c < 2)
            this.method1();
    }
    
    
    static int x, y;

    public static int staticmethod1() { return 1; }

    public static void staticmethod2() { x = 4 ; }

    public static void main(String[] args) 
    {
        Methods c1, c2;
        c1 = new Methods(1);
        c2 = new Methods(0);
        c1.method2(1, true);
        c2.method2(1, true); 
        x = c1.c + c2.c;
       // staticmethod2();
    }
    
    
    

}
