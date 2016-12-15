public static class Main
{
    static int x, y;
    public static void main(String[] args) 
    {
        x = 0;
        y = 0;
        int i = 0;
        
        while(i < f())
        {
            x += y;
            i++;
        }
    }
    
    
    
    public static Int f() 
    {
        y++;
        return 3;
    }
}