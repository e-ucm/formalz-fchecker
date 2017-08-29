import org.junit.* ;
import static org.junit.Assert.assertTrue;;


public class T3TestTriangle {
	

	
	@Test
	public void test() throws Exception {
		String CUT = "Triangle" ;
		String suitefile = XConf.XT3suitedir + "/" + CUT + ".tr" ;
		String invfile   = XConf.XT3suitedir + "/" + CUT + ".inv" ;
		assertTrue(MyDaikonUtils.checkInvariants(suitefile,"tritype1",invfile)) ;
	}

}
