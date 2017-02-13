package Examples.NN_InputVector;

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
	public Vector_R1 combine_R1(int operation, Vector_R1 Z) {
		if (Z.getSize() != this.vector.length) throw new IllegalArgumentException() ;
		double[] result = new double[this.vector.length] ;
		double[] vector2 = Z.getVector() ;
		switch (operation) {
			case VectorCONST.ACCUMULATE: ; // does nothing, deliberately falling through to the code of INPRODUCT
			case VectorCONST.INPRODUCT: {
				int r = 0 ;
				for (int k=0; k<this.vector.length; k++) r += this.vector[k]*vector2[k] ;
				double[] rr = {r} ;
				result = rr ;
				break ;
			}
			case VectorCONST.PLUS: {
				for (int k=0; k<this.vector.length; k++) result[k] = this.vector[k] + vector2[k] ;
				break ;
			}
			default: return null ;
		}
		return new Vector_R1(result) ;
	}

}
