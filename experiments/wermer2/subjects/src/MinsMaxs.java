// package Examples.NN_InputVector;

public class MinsMaxs {

	/**
	 * Given an NxM matrix of double-values, calculate the minimum and maximum of every column in the matrix.
	 * The mins and maxs are returned as a pair of arrays.
	 */
	static public double[][] getMinsMaxs(double[][] data) {
        
		int N = data.length ;
		if (N < 1) { throw new IllegalArgumentException() ; }
		int rowWidth = data[0].length ;
		// check if every row has the same length:
		for (int r1=1; r1<N; r1++) 
            if (data[r1].length != rowWidth) { throw new IllegalArgumentException() ; }
		
		double[] mins = new double[rowWidth] ;
		double[] maxs = new double[rowWidth] ;
		// initialize mins and maxs:
		for (int c1=0; c1<rowWidth; c1++) {
			mins[c1] =data[0][c1] ;
			maxs[c1] =data[0][c1] ;
		}
		// iterate over the matrix:
		for (int r=1; r<N; r++) {
			for (int c=0; c<rowWidth; c++) {
				if (data[r][c] <= mins[c]) { mins[c] = data[r][c] ; } // mutate this
				if (data[r][c] >= maxs[c]) { maxs[c] = data[r][c] ; } // mutate this
			}
		}
		double[][] output = new double[2][] ;
		output[0] = mins ;
		output[1] = maxs ;

		return output ;
	}
	
    
    /*
    static double[]  testme(int m) {
		double[] output = new double[2] ;
		output[0] = 1 ;
		output[1] = 2 ;
        int k = 0 ;
        while (k<m) {
            if (k==10) throw new IllegalArgumentException() ;
            k++ ;
        }
		return output ;
    }
    */
	
}
