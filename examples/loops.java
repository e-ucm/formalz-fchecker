public class Loops 
{
    public static void foo() 
    {      
        int i ;
        for(i = 0; i < 6; i++)
        {
            for(int j = 0; j < 6; j++)
            {
                if(true)
                    assert j == 1;
            }
        }
        int x = i;
    }
}