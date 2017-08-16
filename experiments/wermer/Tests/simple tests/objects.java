public class HelloWorld 
{
    static int c;
    static Chain chain1, chain2;

    public static void main(String[] args) 
    {
        float x = 1;
        Circle circle1, circle2;
        circle1 = new Circle(0, 0);
        circle2 = new Circle(0, 0);
        chain1 = new Chain();
        chain1.tail = chain2;
        chain1.circle = circle1;
        chain2.circle = circle2;
        circle2.center = 2;
        
        x = chain1.tail.circle.center;
    }

    private static void someFunction()
    {
        c = c + 1;
    }
}

// The circle class
public class Circle
{
    public float center, radius;
    
    public Circle(float center, float radius)
    {
        this.center = center;
        this.radius = radius;
    }
}

// A chain of circles
public class Chain
{
    public Circle circle;
    public Chain tail;
}

