import org.junit.* ;
import static org.junit.Assert.assertTrue;;

// A test that produces 100% block coverage on Triangle.tritype1

public class ManualTestTriangle {
	
	@Test(expected = IllegalArgumentException.class)
	public void testArgExc1() {
		(new Triangle(0,1,1)).tritype1() ;
	}
	
	//@Test(expected = IllegalArgumentException.class)
	public void testArgExc2() {
		(new Triangle(1,-1,1)).tritype1() ;
	}
	
	//@Test(expected = IllegalArgumentException.class)
	public void testArgExc3() {
		(new Triangle(1,1,-2)).tritype1() ;
	}
	
	@Test
	public void testNonTri() {
		assertTrue((new Triangle(3,1,1)).tritype1()  == -1 ) ;
		assertTrue((new Triangle(1,4,1)).tritype1() == -1 ) ;
		assertTrue((new Triangle(1,1,5)).tritype1() == -1 ) ;
	}
	
	@Test
	public void testEquilat() {
		assertTrue((new Triangle(1,1,1)).tritype1() == 1 ) ;
		//assertTrue((new Triangle(9,9,9)).tritype1() == 1 ) ;
	}
	
	@Test
	public void testIsolec() {
		assertTrue((new Triangle(5,5,9)).tritype1() == 2 ) ;
		//assertTrue((new Triangle(9,5,5)).tritype1() == 2 ) ;
		//assertTrue((new Triangle(9,1,9)).tritype1() == 2 ) ;
	}
	
	@Test
	public void testScalene() {
		assertTrue((new Triangle(2,3,4)).tritype1() == 0 ) ;
		//assertTrue((new Triangle(3,2,4)).tritype1() == 0 ) ;
		//assertTrue((new Triangle(4,3,2)).tritype1() == 0 ) ;
	}
}
