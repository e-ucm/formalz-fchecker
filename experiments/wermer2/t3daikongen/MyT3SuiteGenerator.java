import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Collectors;

import org.apache.commons.io.IOUtils;

import Sequenic.T3.Config;
import Sequenic.T3.Pool;
import Sequenic.T3.T3SuiteGenAPI;
import Sequenic.T3.Info.CodeCoverage;
import Sequenic.T3.Info.FieldPairsCoverage;
import Sequenic.T3.Info.JacocoInstrumenter;
import Sequenic.T3.Sequence.Datatype.*;
import static Sequenic.T3.Generator.Value.ValueMGCombinators.* ;

public class MyT3SuiteGenerator {

	/**
	 * T3, which is the actual worker of this generator.
	 */
	public T3SuiteGenAPI t3 ;
	//JacocoInstrumenter ji ;
	private CodeCoverage CC ;
	
	/**
	 * To produce a template of a standard T3 configuration. This is just for convenience, so that you can just call
	 * this method to create such a standard conf. Modify this if you want a different standard configuration.
	 */
	static public Config mkStandardTemplateOfT3Configuration() {
		Config config = new Config() ;
		config.injectOracles = false ;
		config.fieldUpdateProbability = 0.0 ;
		config.maxCollectionSize = 2 ;
		config.maxPrefixLength = 6 ;
		config.maxSuffixLength = 0 ;
		config.regressionMode = true ;	
		config.surpressPairwiseTesting = true ;
		return config ;
	}
	
	/**
	 * Make an instance of this generator, using the given configuration, targeting the specified 
	 * Class Under Test (CUT). In addition, you can pass on a custom primitive-values generator. 
	 * Use null if you do not want to pass a custom generator.
	 */
	public MyT3SuiteGenerator(Config configTemplate, 
			         String CUTrootDir, 
			         String classname, 
			         Sequenic.T3.Generator.Generator<PARAM,STEP> custom_values_generator) 
			         throws Exception 
	{
		JacocoInstrumenter ji = new JacocoInstrumenter(CUTrootDir,classname) ;
		CC = new CodeCoverage(ji) ;
		configTemplate.CUT = ji.getInstrumentedCUT() ;
		t3 = T3SuiteGenAPI.mkT3SuiteGenAPI(custom_values_generator,configTemplate) ;
		
	}

	private SUITE generate1(String targetMethodName) {
		Method[] methods = t3.config.CUT.getMethods() ;
		boolean targetIsStatic = false ;
		for (Method m : methods) {
			if (m.getName().equals(targetMethodName)) {
				if (Modifier.isStatic(m.getModifiers())) targetIsStatic = true ;
				break ;
			}
		}
    	SUITE S = t3.suite(!targetIsStatic)  ;
    	S = Xutils.removeInstrumentation(S) ;
    	S.suite = S.suite.stream()
    			  .filter(seq -> { if (seq.steps.isEmpty()) return false ;
    				               STEP last = seq.steps.getLast() ;
    			                   return Xutils.getSTEPName(last).equals(targetMethodName) ; })
    			  .collect(Collectors.toList()) ;
    	return S ;
	}
	
	private void info(SUITE S) throws Exception  { 
		t3.reportCoverage(t3.config.CUT.getClassLoader(),S) ;
	}
	
	/**
	 * Generate a test suite targeting the specified targetMethodName. Specify the desired suite-size. The generator 
	 * tries to deliver a test suite with maximum coverage on the target method. If 100% coverage is achieved while
	 * the current suite size is less that the desired size, the generator will return the suite.
	 * 
	 * Since achieving 100% can be very hard, or even impossible if the target method contains dead code, you also
	 * need to specify the maximum number of test sequences to try. If this maximum is achieved, the generator stops.
	 * If the resulting test suite is larger than the desired size, it will be minimized by throwing away test cases
	 * that can be thrown away without decreasing the coverage over the target method. This will be done up to the
	 * specified desired-size.
	 * 
	 * You can also specify a file to which the generated test suite will be saved. If null, the suite will not be saved
	 * (but still returned).
	 * 
	 * NOTE: this generator assumes that the target method is public and is uniquely identified by its name.
	 */
	public SUITE generate_(String targetMethodName, 
			int desiredSuiteSize, 
			int maximumNumberOfTestSequences_to_try, 
			String fileToSaveTheGeneratedSuite,
			String fileToWriteSummary) 
			throws Exception 
	{
		SUITE S = new SUITE(t3.config.CUT.getName()) ;
		double cov = 0 ;
		int maxBudget = maximumNumberOfTestSequences_to_try ;
		while(S.suite.size() < desiredSuiteSize || cov < 1) {
			SUITE T = generate1(targetMethodName) ;
			maxBudget = maxBudget - T.suite.size() ;
			S.plus(T) ;
			S.dropDuplicates() ;
			cov = sampleCoverage(S,targetMethodName) ;
			System.out.println("** suite size after dropping duplicate: " + S.suite.size()) ;
			System.out.println("** remaining budget: " + maxBudget) ;
			System.out.println("** coverage on " + targetMethodName + ": " + cov) ;
			
			if (maxBudget <= 0) break ;
		}
		if (S.suite.size() > desiredSuiteSize) S = shrink(desiredSuiteSize,S,targetMethodName) ;
		
		info(S);	
		System.out.println("** generating #suite: " + S.suite.size() + ", coverage on "
				         + targetMethodName + ": " + cov) ;
		
		if (fileToSaveTheGeneratedSuite != null) {
			Path P = Paths.get(fileToSaveTheGeneratedSuite); 
			Path parent = P.getParent() ;
			String dir = "." ;
			if (parent != null) dir = parent.toString() ;
			String filename = P.getFileName().toString() ;
			S.save(dir,filename,false) ;
		}
		
		if (fileToWriteSummary != null) {
			OutputStream os = new FileOutputStream(new File(fileToWriteSummary));
			String header = "Name, suite-size, coverage\n" ;
			String info   = "" + t3.config.CUT.getName() + "." + targetMethodName
			                   + "," + S.suite.size()
			                   + "," + cov + "\n" ;
			
			IOUtils.write(header,os);
			IOUtils.write(info,os);
		}
		
		return S ;
	}
	
	private double sampleCoverage(SUITE S, String targetMethodName) throws Exception {
		CC.clear(); 
		S.exec(new Pool(), t3.config.CUT.getClassLoader(), null, 3, 3, false, true, true, null) ;
		CC.collectRTdata() ;		
		CC.analyze() ;
		double cov = CC.getCoverageInfo().getCov(t3.config.CUT.getName(), targetMethodName, null, "ratio") ;
		return cov ;
	}
	
	private SUITE shrink(int desiredSuiteSize, SUITE S, String targetMethodName) throws Exception {
		int N0 = S.suite.size() ;
		int N = N0 ;
		int k = 0 ;
		double cov = sampleCoverage(S,targetMethodName) ; 
		while (k<N) {
			if (S.suite.size() == desiredSuiteSize) break ;
			SEQ seq = S.suite.remove(k) ;
			double cov2 = sampleCoverage(S,targetMethodName) ;
			if (cov2 < cov) {
				// seq has impact, put it back:
				S.suite.add(k,seq);
				k++ ;
			}
			else {
				// we can safely throw away k
				N-- ;
			}
		}
		System.out.println("** shrinking #suite to: " + S.suite.size() + ", throwing away " + (N0 - S.suite.size())) ;
		return S ;
	}
	
	
	/**
	 * The function to generate T3 suite for a given target.
	 */
	static public void generate(
			Sequenic.T3.Generator.Generator<PARAM,STEP> my_custom_values_generator, 
			String CUTrootDir,
			String CUT, 
			String targetMethod,
			int desiredSuiteSize,
			int maximumNumberOfTestSequences_to_try,
			String saveFile,
			String reportFile) throws Exception 
	{
		MyT3SuiteGenerator gen = new MyT3SuiteGenerator(mkStandardTemplateOfT3Configuration(),CUTrootDir,CUT,my_custom_values_generator) ;
		gen.generate_(targetMethod,desiredSuiteSize,maximumNumberOfTestSequences_to_try,saveFile,reportFile) ;	
	}
	
	
	static String ExperimentHome = "/Users/iswbprasetya/workshop/projects/koenwlp/repo/javawlp/experiments/wermer2" ;
	static String XCUTrootdir = ExperimentHome + "/subjects/compiled" ;
	static String XDatadir = ExperimentHome + "/data" ;
	static String XT3suitedir = ExperimentHome + "/tests/t3suite" ;
	static int XdesiredSuiteSize = 1000 ;
	static int XmaximumNumberOfTestSequences_to_try = 5000 ;
	
	static void genSuiteTriangle() throws Exception {
		Sequenic.T3.Generator.Generator<PARAM,STEP> customgen = Float(OneOf(-1f,0f,1f,1.1f,2f,3f,9f)) ;
		String CUT = "Triangle" ;
		generate(customgen,XCUTrootdir,CUT,"tritype1",
				XdesiredSuiteSize,
				XmaximumNumberOfTestSequences_to_try,
				XT3suitedir + "/" + CUT + ".tr",
				XDatadir + "/" + CUT + "_t3gen.txt") ;
	}
	
	static void genSuiteMinsMaxs() throws Exception {
		String CUT = "MinsMaxs" ;
		generate(null,XCUTrootdir,
				CUT,
				"getMinsMaxs",
				XdesiredSuiteSize,
				XmaximumNumberOfTestSequences_to_try,
				XT3suitedir + "/" + CUT + ".tr",
				XDatadir + "/" + CUT + "_t3gen.txt") ;
	}
	


	static public void main(String[] args) throws Exception {
		//genSuiteTriangle() ;
		genSuiteMinsMaxs() ;
	}
	
}
