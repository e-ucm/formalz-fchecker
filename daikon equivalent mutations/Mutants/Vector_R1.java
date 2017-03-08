
public class Vector_R1 {
	
	private double[] vector;
	
	public Vector_R1(double[] vec) {
		this.vector = new double[vec.length] ;
		System.arraycopy(vec, 0, this.vector, 0, vec.length);
	}
	
	public double[] getVector() {
		int N = this.vector.length ;
		double[] V = new double[N] ;
		for (int k=0; k<N; k++) V[k] = this.vector[k] ;
		return V ;
	}

	public int getSize() { return this.vector.length ; }

	
	/**
	 * R1 replaces the return in the INPRODUCT-case with a break.
	 */
	public Vector_R1 combine(int operation, Vector_R1 Z) {
        final int ACCUMULATE = 1;
        final int INPRODUCT = 2;
        final int PLUS = 3;
		if (Z.getSize() != this.vector.length) throw new IllegalArgumentException() ;
		double[] result = new double[this.vector.length] ;
		double[] vector2 = Z.getVector() ;
		switch (operation) {
			case ACCUMULATE: ; // does nothing, deliberately falling through to the code of INPRODUCT
			case INPRODUCT: {
				int r = 0 ;
				for (int k1=0; k1<this.vector.length; k1++) r += this.vector[k1]*vector2[k1] ;
				double[] rr = new double[1];
                rr[0] = r;
				result = rr ;
				break ;
			}
			case PLUS: {
				for (int k2=0; k2<this.vector.length; k2++) result[k2] = this.vector[k2] + vector2[k2] ;
				break ;
			}
			default: return null ;
		}
		return new Vector_R1(result) ;
	}

}
