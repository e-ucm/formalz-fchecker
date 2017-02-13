package Examples.NN_InputVector;

public class MinsMaxs_R1 {

	/**
	 * Refactoring R1: replacing some <= and >= to improve efficiency.
	 */
	static public double[][] getMinsMaxs(double[][] data) {
		int N = data.length ;
		if (N < 1) throw new IllegalArgumentException() ;
		int rowWidth = data[0].length ;
		// check if every row has the same lenght:
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
		for (int r=1; r<data.length; r++) {
			for (int c=0; c<rowWidth; c++) {
				if (data[r][c] < mins[c]) mins[c] = data[r][c] ; // more efficient
				if (data[r][c] > maxs[c]) maxs[c] = data[r][c] ; // more efficient
			}
		}
		double[][] output = new double[2][] ;
		output[0] = mins ;
		output[1] = maxs ;
		return output ;
	}

}
