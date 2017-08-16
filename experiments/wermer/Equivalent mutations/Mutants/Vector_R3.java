package Examples.NN_InputVector;

public class Vector {
	
	private double[] vector;
	
	public Vector(double[] vec) {
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
	public Vector combine(int operation, Vector Z) {
        int ACCUMULATE = 1;
        int INPRODUCT = 2;
        int PLUS = 3;
		if (Z.getSize() != this.vector.length) throw new IllegalArgumentException() ;
		double[] result = new double[this.vector.length] ;
		double[] vector2 = Z.getVector() ;
		Vector resultingVector = null ;
		switch (operation) {
	   	    case PLUS: {
			    for (int k1=0; k1<this.vector.length; k1++) result[k1] = this.vector[k1] + vector2[k1] ;
			    resultingVector = new Vector(result) ;
			    break ; }
			case ACCUMULATE: ; // does nothing, deliberately falling through to the code of INPRODUCT
			case INPRODUCT: {
				int r = 0 ;
				for (int k2=0; k2<this.vector.length; k2++) r += this.vector[k2]*vector2[k2] ;
				double[] rr = new double[1];
                rr[0] = r;
				resultingVector = new Vector(rr) ; // we will just let it fall through
			}
			default:  ;
		}
		return resultingVector ;
	}

}
