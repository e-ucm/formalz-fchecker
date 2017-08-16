package Examples.NN_InputVector;


public class Normalizer_R4 {

	/**
	 * R4: Re-routing exceptional control flow via a different exception.
	 */
	static public void normalize(double[] mins, double[] maxs, double[][] data) {
		int rowWidth = mins.length ;
		int row = 0 ;
		try {
			if (maxs.length == rowWidth) {
				while (row < data.length) {
					if (data[row].length != rowWidth) { throw new IncompatibleSizeException() ; }
					int col = 0 ;
					while (col < rowWidth) {
						double delta = maxs[col] - mins[col] ;
						if (delta == 0) throw new IncompatibleSizeException() ;
						double x = data[row][col] ;
						data[row][col] = (x - mins[col])/delta ;
						col++ ;
					}
					row++ ;
				}
		}
		else { throw new IllegalArgumentException() ; }
		}
		catch (IncompatibleSizeException e) {
			throw new IllegalArgumentException(e) ;
		}
	}

}
