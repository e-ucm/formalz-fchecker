public class Objects 
{
    static int c;
    static CircleChain chain1, chain2;

    public static void foo() 
    {
        float x = 1;
        Circle circle1, circle2;
        circle1 = new Circle(0, 0);
        circle2 = new Circle(0, 0);
        chain1 = new CircleChain();
        chain2 = new CircleChain();
        chain1.tail = chain2;
        chain1.circle = circle1;
        chain2.circle = circle2;
        circle2.center = 2;    
        x = chain1.tail.circle.center;
    }

}

class Circle {
    public float center, radius;
    public Circle(float center, float radius) {
        this.center = center;
        this.radius = radius;
    }
}

class CircleChain 
{
    public Circle circle;
    public CircleChain tail;
}



