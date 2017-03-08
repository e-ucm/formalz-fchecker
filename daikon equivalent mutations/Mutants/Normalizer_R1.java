import java.lang.IllegalArgumentException;

public class Normalizer_R1 {

	/**
	 * R1: Restructuring the check that mins and max should be of the same length.
	 */
	static public void normalize(double[] mins, double[] maxs, double[][] data) {
		int rowWidth = mins.length ;
		if (maxs.length != rowWidth) { throw new IllegalArgumentException() ; }
		int row = 0 ;
		while (row < data.length) {
			if (data[row].length != rowWidth) { throw new IllegalArgumentException() ; }
			int col = 0 ;
			while (col < rowWidth) {
				double delta = maxs[col] - mins[col] ;
				if (delta == 0) throw new IllegalArgumentException() ;
				double x = data[row][col] ;
				data[row][col] = (x - mins[col])/delta ;
				col++ ;
			}
			row++ ;
		}
	}

}
