import java.lang.IllegalArgumentException;

public class Normalizer_R3 {

	/**
	 * R3: converting while loop to for loop.
	 */
	static public void normalize(double[] mins, double[] maxs, double[][] data) {
		int rowWidth = mins.length ;
		if (maxs.length == rowWidth) {
			for (int row=0; row < data.length; row++) {
				if (data[row].length != rowWidth) { throw new IllegalArgumentException() ; }
				for (int col=0; col < rowWidth; col++) {
					double delta = maxs[col] - mins[col] ;
					if (delta == 0) throw new IllegalArgumentException() ;
					double x = data[row][col] ;
					data[row][col] = (x - mins[col])/delta ;
				}
			}
		}
		else { throw new IllegalArgumentException() ; }
	}

}
