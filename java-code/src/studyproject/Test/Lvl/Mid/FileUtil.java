package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.fail;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * class to create and remove files for the Junit tests
 * @author Michael
 *
 */
public class FileUtil {

	public static final String dir = "test";
	public static final String sourceFile = "sourceFile";
	public static final String destFile = "destFile";
	public static final String fileContents = "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed"
			+ " eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim"
			+ " veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi"
			+ " consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu"
			+ " fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in"
			+ " culpa qui officia deserunt mollit anim id est laborum.";

	/**
	 * create a file for testing
	 */
	public static void createFile() {
		File file = new File(dir);
		file.mkdir();
		try (BufferedWriter writer = new BufferedWriter(new FileWriter(dir
				+ "/" + sourceFile))) {
			writer.write(fileContents);
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
	}

	/**
	 * delete the created files
	 */
	public static void cleanUp(){
		File file = new File(dir + "/" + sourceFile);
		if(!file.delete()){
			System.err.println("could not delete file " + sourceFile);
		}
		file = new File(dir + "/" + destFile);
		if(!file.delete()){
			System.err.println("could not delete file " + destFile);
		}
		file = new File(dir);
		if(!file.delete()){
			System.err.println("could not delete directory " + dir);
		}
	}

}
