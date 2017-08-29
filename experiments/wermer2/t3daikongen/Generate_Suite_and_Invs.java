import static Sequenic.T3.Generator.Value.ValueMGCombinators.*;

import Sequenic.T3.Sequence.Datatype.*;

public class Generate_Suite_and_Invs {
	
	public static String ExperimentHome = "/Users/iswbprasetya/workshop/projects/koenwlp/repo/javawlp/experiments/wermer2" ;
	public static String XCUTrootdir = ExperimentHome + "/subjects/compiled" ;
	public static String XDatadir = ExperimentHome + "/data" ;
	public static String XT3suitedir = ExperimentHome + "/tests/t3suite" ;
	
	static int XdesiredSuiteSize = 1000 ;
	static int XmaximumNumberOfTestSequences_to_try = 5000 ;
	
	static void genSuiteAndInvs(Sequenic.T3.Generator.Generator<PARAM,STEP> customgen,
			String CUT,
			String targetMethod) throws Exception 
	{
		SUITE S = MyT3.generate(customgen,XCUTrootdir,CUT,targetMethod,
				       XdesiredSuiteSize,
				       XmaximumNumberOfTestSequences_to_try,
				       XT3suitedir + "/" + CUT + ".tr",
				       XDatadir + "/" + CUT + "_t3gen.txt") ;
		MyDaikonUtils.mineInvariants(S, targetMethod, XT3suitedir + "/" + CUT) ;
	}
	
	static void genSuiteTriangle() throws Exception {
		Sequenic.T3.Generator.Generator<PARAM,STEP> customgen = Float(OneOf(-2.1f,-1f,0f,0.01f,1f,1.1f,2f,3f,4.5f,9f)) ;
		genSuiteAndInvs(customgen,"Triangle","tritype1") ;
	}
	
	static void genSuiteMinsMaxs() throws Exception {
		genSuiteAndInvs(null,"MinsMaxs","getMinsMaxs") ;
	}
	


	static public void main(String[] args) throws Exception {
		genSuiteTriangle() ;
		genSuiteMinsMaxs() ;
	}

}
