public class HelloWorld 
{
    static int c;
    static Circle circle1, circle2;

    public static void main(String[] args) 
    {
        float x = 1;
        circle1 = new Circle(2, 2);
        circle2 = new Circle(2, 2);
        circle2 = circle1;
        circle2.center = 3;
        x = circle1.center;
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
