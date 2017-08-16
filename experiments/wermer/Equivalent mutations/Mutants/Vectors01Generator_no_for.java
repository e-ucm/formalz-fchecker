//http://www.introprogramming.info/english-intro-csharp-book/read-online/chapter-10-recursion/#demos-source-code

class Vectors01Generator
{
    static void Gen01(int index, int[] vector1)
    {
        if (index == -1)
        {
            Print(vector1);
        }
        else
        {
            vector1[index] = 0;
            Gen01(index - 1, vector1);
            vector1[index] = 1;
            Gen01(index - 1, vector1);
        }
    }

    static void Print(int[] vector2)
    {
        for (int i2 = 0; i2 < vector2.length; i2++)
        {
        	System.out.printf("{0} ", i2);
        }
        System.out.println();
    }

    static void main()
    {
        System.out.print("n = ");
        int number = System.in.read();
		
        int[] vector3 = new int[number];
		
        Gen01(number - 1, vector3);
    }
}
