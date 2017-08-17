//http://www.sanfoundry.com/csharp-program-convert-2darray-1darray/

/*
 * C# Program to Convert a 2D Array into 1D Array
 */
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
 
namespace Program
{
    class twodmatrix
    {
        int m, n;
        int[,] a;
        twodmatrix(int x, int y)
        {
            m = x;
            n = y;
            a = new int[m, n];
        }
        public void readmatrix()
        {
            for (int i = 0; i < m; i++)
            {
                for (int j = 0; j < n; j++)
                {
                    Console.WriteLine("a[{0},{1}]=", i, j);
                    a[i, j] = Convert.ToInt32(Console.ReadLine());
                }
            }
        }
        public void printd()
        {
            for (int i = 0; i < m; i++)
            {
                for (int j = 0; j < n; j++)
                {
                    Console.Write("{0}\t", a[i, j]);
 
                }
                Console.Write("\n");
            }
        }
        public void convert()
        {
            return;
        }
        public void printoned()
        {
            for (int i = 0; i < m; i++)
            {
                for (int j = 0; j < n; j++)
                {
                    Console.WriteLine("{0}\t", a[i*n+j]);
                }
            }
        }
 
 
        public static void Main(string[] args)
        {
            twodmatrix obj = new twodmatrix(2,3);
            Console.WriteLine("Enter the Elements : ");
            obj.readmatrix();
            Console.WriteLine("\t\t Given 2-D Array(Matrix) is : ");
            obj.printd();
            Console.WriteLine("\t\t Converted 1-D Array is : ");
            obj.printoned();
            Console.ReadLine();
        }
    }
}