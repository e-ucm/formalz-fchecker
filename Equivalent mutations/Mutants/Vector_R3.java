package Examples.NN_InputVector;

public class Vector_R3 {
	
	private double[] vector;
	
	public Vector_R3(double[] vec) {
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
	 * R3 reorder the cases, and remove the use of return from the case arms.
	 */
	public Vector_R3 combine_R3(int operation, Vector_R3 Z) {
		if (Z.getSize() != this.vector.length) throw new IllegalArgumentException() ;
		double[] result = new double[this.vector.length] ;
		double[] vector2 = Z.getVector() ;
		Vector_R3 resultingVector = null ;
		switch (operation) {
	   	    case VectorCONST.PLUS: {
			    for (int k=0; k<this.vector.length; k++) result[k] = this.vector[k] + vector2[k] ;
			    resultingVector = new Vector_R3(result) ;
			    break ; }
			case VectorCONST.ACCUMULATE: ; // does nothing, deliberately falling through to the code of INPRODUCT
			case VectorCONST.INPRODUCT: {
				int r = 0 ;
				for (int k=0; k<this.vector.length; k++) r += this.vector[k]*vector2[k] ;
				double[] rr = {r} ;
				resultingVector = new Vector_R3(rr) ; // we will just let it fall through
			}
			default:  ;
		}
		return resultingVector ;
	}

}
