//http://www.sanfoundry.com/csharp-program-convert-2darray-1darray/


class twodmatrix
{
    int m, n;
    int[][] a;
    int[] b;
    twodmatrix(int x, int y)
    {
        this.m = x;
        this.n = y;
        this.a = new int[this.m][this.n];
        this.b = new int[this.m * this.n];
    }
    public void readmatrix()
    {
        for (int i1 = 0; i1 < this.m; i1++)
        {
            for (int j1 = 0; j1 < this.n; j1++)
            {
                System.out.printf("a[{0},{1}]=", i1, j1);
                this.a[i1][j1] = System.in.read();
            }
        }
    }
    public void printd()
    {
        for (int i2 = 0; i2 < this.m; i2++)
        {
            for (int j2 = 0; j2 < this.n; j2++)
            {
                System.out.printf("{0}\t", this.a[i2][j2]);
 
                }
                System.out.printf("\n");
        }
    }
    public void convert()
    {
        for (int i3 = 0; i3 < this.m; i3++)
        {
            for (int j3 = 0; j3 < this.n; j3++)
            {
                this.b[i*n+j] = this.a[i3][j3];
            }
        }
    }
    public void printoned()
    {
	    this.convert();
        
        for (int i4 = 0; i4 < this.m * this.n; i4++)
        {
            System.out.printf("{0}\t", this.b[i4]);
        }
    }
 
 
    public static void main(String[] args)
    {
        twodmatrix obj = new twodmatrix(2,3);
        System.out.printf("Enter the Elements : ");
	    obj.readmatrix();
	    System.out.printf("\t\t Given 2-D Array(Matrix) is : ");
	    obj.printd();
	    System.out.printf("\t\t Converted 1-D Array is : ");
	    obj.printoned();
    }
}