package Examples.NN_InputVector;

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
	public Vector_R2 combine_R2(int operation, Vector_R2 Z) {
		if (Z.getSize() != this.vector.length) throw new IllegalArgumentException() ;
		double[] result = new double[this.vector.length] ;
		double[] vector2 = Z.getVector() ;
		switch (operation) {
			case VectorCONST.ACCUMULATE: return combine_R2(VectorCONST.INPRODUCT,Z) ;
			case VectorCONST.INPRODUCT: {
				int r = 0 ;
				for (int k=0; k<this.vector.length; k++) r += this.vector[k]*vector2[k] ;
				double[] rr = {r} ;
				return new Vector_R2(rr) ;
			}
			case VectorCONST.PLUS: {
				for (int k=0; k<this.vector.length; k++) result[k] = this.vector[k] + vector2[k] ;
				break ;
			}
			default: return null ;
		}
		return new Vector_R2(result) ;
	}

}
