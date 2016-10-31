public class HelloWorld 
{
    static int c;
    static Circle circle;

    public static void main(String[] args) 
    {
        c = 0;
        int x = 1;
        bool b;
        if(b)
            x = 2;
       // circle = new Circle(1, 1);
    }

    private static void someOtherFunction()
    {
        // increase c, because why not?
        c = c + 1;
    }
}

// The circle class
public class Circle
{
    float center, radius;
    
    public Circle(float center, float radius)
    {
        this.center = center;
        this.radius = radius;
    }
}