package Examples.NN_InputVector;

public class Normalizer {
	
	static public void normalize(double[] mins, double[] maxs, double[][] data) {
		int rowWidth = mins.length ;
		int row = 0 ;
		if (maxs.length == rowWidth) {
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
		else { throw new IllegalArgumentException() ; }
	}

}
