// http://www.java2s.com/Code/CSharp/2D-Graphics/MultiplytwoMatrixes.htm

using System;
using System.Collections.Generic;
using System.Text;


public class Matrix
{
    public static double[,] Mul(double[,] a, double[,] b)
    {
        if (a.GetLength(1) != b.GetLength(0))
            throw new ArgumentException();
        double[,] reuslt = new double[a.GetLength(0), b.GetLength(1)];
        for (int i = 0; i < a.GetLength(0); i++)
            for (int j = 0; j < b.GetLength(1); j++)
            {
                for (int k = 0; k < a.GetLength(1); k++)
                {
                    reuslt[i, j] += a[i, k] * b[k, j];
                }
            }
        return reuslt;
    }
    public static double[,] MulDiag(double[,] a, double[] b)
    {
        if (a.GetLength(1) != b.GetLength(0))
            throw new ArgumentException();
        double[,] reuslt = new double[a.GetLength(0), b.GetLength(0)];
        for (int i = 0; i < a.GetLength(0); i++)
            for (int j = 0; j < b.GetLength(0); j++)
                reuslt[i, j] = a[i, j] * b[j];
        return reuslt;
    }
}