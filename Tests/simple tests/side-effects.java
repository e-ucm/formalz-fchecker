import blabla;

public static class Main
{
    static int x, y;
    public static void main(String[] args) 
    {
        x = 0;
        y = 3;
        int i = 0;
        while(f() < y)
        {
            x = f();
        }
        
        x++; 
        x++;
    }
    
    
    
    public static Int f() 
    {
        y++;
        return 3;
    }
}