public class Switch {

    public static void foo()
    {
        int x, y;
        
        y = 0;
        
        switch (y) 
        {
            case 0:  x = 0;
            case 1:  x = 1;
                    break;
            case 2:  x = 2;
                     break;
            default: x = 2;
                     break;
        }

    }
}