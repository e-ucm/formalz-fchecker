package Examples.NN_InputVector;

public class MinsMaxs_R3 {

	/**
	 * Refactoring R3: changing the main loop from a row-column loop, to column-row loop.
	 */
	static public double[][] getMinsMaxs(double[][] data) {
		int N = data.length ;
		if (N < 1) throw new IllegalArgumentException() ;
		int rowWidth = data[0].length ;
		// check if every row has the same length:
		for (int r=1; r<N; r++) 
			if (data[r].length != rowWidth) throw new IllegalArgumentException() ;
		
		double[] mins = new double[rowWidth] ;
		double[] maxs = new double[rowWidth] ;
		// initialize mins and maxs:
		for (int c=0; c<rowWidth; c++) {
			mins[c] =data[0][c] ;
			maxs[c] =data[0][c] ;
		}
		// iterate over the matrix:
		for (int c=0; c<rowWidth; c++) {
		    for (int r=1; r<N; r++) {
				if (data[r][c] <= mins[c]) mins[c] = data[r][c] ; // mutate this
				if (data[r][c] >= maxs[c]) maxs[c] = data[r][c] ; // mutate this
			}
		}
		double[][] output = new double[2][] ;
		output[0] = mins ;
		output[1] = maxs ;
		return output ;
	}

}
