public class Arrays 
{
    public static void main(String[] args) 
    {
        int x;
        int[] a = new int[12];
        a[5] = 2;
        try 
            {
                x = a[12];
            }
        catch (ArrayIndexOutOfBoundsException y) 
            {
                x = a[5];
            }
    }
}