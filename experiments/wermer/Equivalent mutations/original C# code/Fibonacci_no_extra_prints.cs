// http://www.sanfoundry.com/csharp-program-generate-fibonocci-series/

/*
 * C#  Program to Generate Fibonacci Series
 */
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
 
namespace fibonaci
{
    class Program
    {
        static void Main(string[] args)
        {
            int i, count, f1 = 0, f2 = 1, f3 = 0;
            Console.Write("Enter the Limit : ");
            count = int.Parse(Console.ReadLine());

            for (i = 0; i <= count + 2; i++)
            {
                f3 = f1 + f2;
                Console.WriteLine(f1);
                f1 = f2;
                f2 = f3;
            }
            Console.ReadLine();
 
        }
    }
}