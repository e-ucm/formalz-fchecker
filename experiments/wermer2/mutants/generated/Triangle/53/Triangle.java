public class Triangle {
	
	private float x  ;
	private float y  ;
	private float z  ;

	public Triangle(float x, float y, float z){ 
		this.x = x ;
        this.y = y ;
        this.z = z ;
	}

    // classify this triangle's type:
	public int tritype1() {
        if ((this.x <= 0) || (this.y <= 0) || (this.z <= 0)) throw new IllegalArgumentException() ;
		if (this.x >= (this.y+this.z)) return -1 ; // not a triangle
		if (this.y >= (this.x+this.z)) return -1 ; // not a triangle
		if (this.z >= (this.x+this.y)) return -1 ; // not a triangle
		if ((this.x == this.y) && (this.y == this.z)) return 1 ; // equilateral
		if ((this.x == this.y) || (this.y >= this.z) || (this.x == this.z)) return 2 ; // isoleces
        return 0 ; // scalene
	}
    
    // static variation of tritype:
	static public int tritype2(float x, float y, float z) {
        if ((x <= 0) || (y <= 0) || (z <= 0)) throw new IllegalArgumentException() ;
		if (x >= (y+z)) return -1 ; // not a triangle
		if (y >= (x+z)) return -1 ; // not a triangle
		if (z >= (x+y)) return -1 ; // not a triangle
		if ((x == y) && (y == z)) return 1 ; // equilateral
		if ((x == y) || (y == z) || (x == z)) return 2 ; // isoleces
        return 0 ; // scalene
	}
}
