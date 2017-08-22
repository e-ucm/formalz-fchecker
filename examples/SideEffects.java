public class SideEffects
{
    static int x, y;

    public static void foo() {
        x = 0;
        y = 3;
        int i = 0;
        while(f() < y) { x = f(); }
        x++; 
        x++;
    }
    
    public static int f() { y++; return 3; }
}