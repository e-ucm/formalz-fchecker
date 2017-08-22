public class Arrays0 
{
    public static void foo() 
    {
        int x;
        int y = 4+8;
        int[] a = new int[12];
        int[] b = new int[0];
        a[5] = 2;
        try 
            {
                x = b[y];
            }
        catch (ArrayIndexOutOfBoundsException e) 
            {
                x = a[5];
            }
    }
}