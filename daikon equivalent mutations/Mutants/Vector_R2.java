
public class Vector_R2  {
	
	private double[] vector;
	
	public Vector_R2(double[] vec) {
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
	 * R2 replaces the fall through in the ACCUMULATE case with a recursive call with INPRODUCT.
	 */
	public Vector_R2 combine(int operation, Vector_R2 Z) {
        final int ACCUMULATE = 1;
        final int INPRODUCT = 2;
        final int PLUS = 3;
		if (Z.getSize() != this.vector.length) throw new IllegalArgumentException() ;
		double[] result = new double[this.vector.length] ;
		double[] vector2 = Z.getVector() ;
		switch (operation) {
			case ACCUMULATE: return this.combine(INPRODUCT,Z) ;
			case INPRODUCT: {
				int r = 0 ;
				for (int k1=0; k1<this.vector.length; k1++) r += this.vector[k1]*vector2[k1] ;
				double[] rr = new double[1];
                rr[0] = r;
				return new Vector_R2(rr) ;
			}
			case PLUS: {
				for (int k2=0; k2<this.vector.length; k2++) result[k2] = this.vector[k2] + vector2[k2] ;
				break ;
			}
			default: return null ;
		}
		return new Vector_R2(result) ;
	}

}
