import java.lang.IllegalArgumentException;

public class Normalizer_R2 {

	/**
	 * R2: moving the element-level normalization to a separate method.
	 */
	static public void normalize(double[] mins, double[] maxs, double[][] data) {
		int rowWidth = mins.length ;
		int row = 0 ;
		if (maxs.length == rowWidth) {
			while (row < data.length) {
				if (data[row].length != rowWidth) { throw new IllegalArgumentException() ; }
				int col = 0 ;
				while (col < rowWidth) {
					data[row][col] = normalizeValue(data[row][col] ,mins[col],maxs[col]) ;
					col++ ;
				}
				row++ ;
			}
		}
		else { throw new IllegalArgumentException() ; }
	}

	static private double normalizeValue(double x, double min, double max) throws IllegalArgumentException {
		double delta = max - min ;
		if (delta == 0) throw new IllegalArgumentException() ;
		return (x - min)/delta ;
	}

}
