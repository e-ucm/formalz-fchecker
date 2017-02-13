package Examples.NN_InputVector;

public class MinsMaxs_R2 {

	/**
	 * Refactoring R2: moving the check for equal length rows to inside the main loop.
	 */
	static public double[][] getMinsMaxs(double[][] data) {
		int N = data.length ;
		if (N < 1) throw new IllegalArgumentException() ;
		int rowWidth = data[0].length ;
	
		double[] mins = new double[rowWidth] ;		
		double[] maxs = new double[rowWidth] ;
		
		// initialize mins and maxs:
		for (int c=0; c<rowWidth; c++) {
			mins[c] =data[0][c] ;
			maxs[c] =data[0][c] ;
		}
		// iterate over the matrix:
		for (int r=1; r<data.length; r++) {
			// first check if the row has the same length as data[0]:
			if (data[r].length != rowWidth) throw new IllegalArgumentException() ;
			for (int c=0; c<rowWidth; c++) {
				if (data[r][c] <= mins[c]) mins[c] = data[r][c] ; 
				if (data[r][c] >= maxs[c]) maxs[c] = data[r][c] ; 
			}
		}
		double[][] output = new double[2][] ;
		output[0] = mins ;
		output[1] = maxs ;
		return output ;
	}

}
