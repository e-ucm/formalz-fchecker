import java.lang.IllegalArgumentException;
import java.lang.ArithmeticException;

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
					if (data[row].length != rowWidth) { throw new ArithmeticException() ; }
					int col = 0 ;
					while (col < rowWidth) {
						double delta = maxs[col] - mins[col] ;
						if (delta == 0) throw new ArithmeticException() ;
						double x = data[row][col] ;
						data[row][col] = (x - mins[col])/delta ;
						col++ ;
					}
					row++ ;
				}
		}
		else { throw new IllegalArgumentException() ; }
		}
		catch (ArithmeticException e) {
			throw new IllegalArgumentException(e) ;
		}
	}

}
