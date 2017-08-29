import org.junit.* ;
import static org.junit.Assert.assertTrue;;

public class T3TestMinsMaxs {
	@Test
	public void test() throws Exception {
		String CUT = "MinsMaxs" ;
		String suitefile = XConf.XT3suitedir + "/" + CUT + ".tr" ;
		String invfile = XConf.XT3suitedir + "/" + CUT + ".inv" ;
		assertTrue(MyDaikonUtils.checkInvariants(suitefile,"getMinsMaxs",invfile)) ;
	}
}
