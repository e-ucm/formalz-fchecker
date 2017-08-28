import org.junit.* ;
import static org.junit.Assert.assertTrue;;

// A test that produces 100% block coverage on MinsMaxs.getMinsMaxs

public class ManualTestMinsMaxs {
	
	@Test(expected = IllegalArgumentException.class)
	public void testArgExc1() {
		MinsMaxs.getMinsMaxs(new double[0][0]) ;
	}

	@Test(expected = IllegalArgumentException.class)
	public void testArgExc2() {
		double[][] data = new double[2][] ;
		data[0] = new double[0] ;
		data[1] = new double[1] ;
		MinsMaxs.getMinsMaxs(data) ;
	}
	
	@Test
	public void testMins() {
		double[][] data = new double[2][] ;
		double[] row0 = { 0, 4 } ;
		double[] row1 = { 3, 1 } ;
		data[0] = row0 ;
		data[1] = row1 ;	
		assertTrue(MinsMaxs.getMinsMaxs(data)[0][0] == 0) ;
		assertTrue(MinsMaxs.getMinsMaxs(data)[0][1] == 1) ;
	}
}
