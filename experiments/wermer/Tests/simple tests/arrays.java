public class Arrays 
{
    public static void main(String[] args) 
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
        catch (ArrayIndexOutOfBoundsException y) 
            {
                x = a[5];
            }
    }
}