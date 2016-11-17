public class Arrays 
{
    public static void main(String[] args) 
    {
        int x;
        int y = 4+8;
        int[] a = new int[12];
        a[5] = 2;
        try 
            {
                x = a[y];
            }
        catch (ArrayIndexOutOfBoundsException y) 
            {
                x = a[5];
            }
    }
}