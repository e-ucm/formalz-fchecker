

import Sequenic.T3.Daikon.*;
import Sequenic.T3.Sequence.Datatype.SUITE;

/**
 * Provide top-level functions to infer and check Daikon invariants from/on a T3 test suite.
 */
public class MyDaikonUtils {
	
	/**
	 * Use a T3 test suite to drive executions on the suite's target class, and then infer 
	 * Daikon-invariants from these executions. The inferred invariants will be saved on the 
	 * specified file name (should end with .inv).
	 * 
	 * This will also generate a trace file containing collected run-time values of the parameters
	 * passed to methods when they are called during the executions, and the values they return.
	 * The trace file has .dtrace extension.
	 */
	static void mineInvariants(SUITE S, String targetMethod, String invariantsFile) throws Exception {
		String prefix = invariantsFile ;
		if (invariantsFile.endsWith(".inv")) prefix = invariantsFile.substring(0, invariantsFile.length() - 4) ;
		(new T3Daikon()).infer(S, targetMethod, prefix + ".dtrace", prefix + ".inv");
	}
	
    void mineInvariants(String saved_T3_testsuite_file, String targetMethod, String invariantsFile) throws Exception {
		SUITE S = SUITE.load(saved_T3_testsuite_file) ;
		mineInvariants(S,targetMethod,invariantsFile) ;
	}
	
	/**
	 * Read invariants from the specified file; then check the invariants on the executions generated 
	 * by the given T3 test suite. If no violation is found, true is returned, else false. This 
	 * is also echoed to System.out.
	 * 
	 * If there are violations, a _violations.txt file is generated that contains further details
	 * on the violations.
	 */
	static boolean checkInvariants(SUITE S, String targetMethod, String invariantsFile) throws Exception{
		String prefix = invariantsFile ;
		if (invariantsFile.endsWith(".inv")) prefix = invariantsFile.substring(0, invariantsFile.length() - 4) ;
		invariantsFile = prefix + ".inv" ;
		String violationsReportFile = prefix + "_violations.txt" ;
		return (new T3Daikon()).check(S,targetMethod,invariantsFile,violationsReportFile) ;
	}
	
	static boolean checkInvariants(String saved_T3_testsuite_file, String targetMethod, String invariantsFile) throws Exception{
		SUITE S = SUITE.load(saved_T3_testsuite_file) ;
		return checkInvariants(S,targetMethod,invariantsFile) ;
		
	}

}
