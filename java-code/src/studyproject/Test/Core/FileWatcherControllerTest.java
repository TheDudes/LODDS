package studyproject.Test.Core;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Date;

import org.junit.Test;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.Watcher.FileWatcherController;

public class FileWatcherControllerTest {
	
	private String testDirectory = "java-code/testData/FileWatcherController/";
	private String virtualRoot = "FileWatcherController/";

	@Test
	public void listShouldContainOneFile() throws Exception {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(testDirectory+"oneFile",testDirectory);
		assertEquals(1,controller.fileInfoList.size());
	}
	
	@Test
	public void listShouldContainTwoFiles() throws Exception {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(testDirectory+"twoFiles",testDirectory);
		assertEquals(2,controller.fileInfoList.size());
	}
	
	@Test
	public void shouldGetCorrectFileInfoString() throws NoSuchAlgorithmException, IOException {
		FileInfo fileInfo = new FileInfo("983x89j23897rw9789n87we9r78w798", 1000, testDirectory+"oneFile/testFile.txt", testDirectory, FileAction.add);
		
		FileWatcherController controller = new FileWatcherController();
		String actualOutput = controller.convertFileInfoToString(fileInfo);
		String expectedOutput = "add "+fileInfo.checksum+" "+fileInfo.size+" "+fileInfo.relativeFileName+"\n";
		assertEquals(expectedOutput, actualOutput);
	}	
	
	@Test
	public void shouldGetCorrectFileInfoMsgWithTimestampZero() throws Exception {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(testDirectory+"oneFile", testDirectory);
		
		/**
		 *  example output
		 *      all 1464269498 1\n
			    add 38e80faf7... 421341231 /first-file.txt\n 
		 */

		String actualResponse = controller.getInfo(0);
		String expectedResponse = 
				"all "+System.currentTimeMillis() / 1000L+" 1\n"
				+ "add 736bf95996d40c71307e3727931b721dfb17bd27c441b903a6dd483b37021ac1 8 "+virtualRoot+"oneFile/testFile.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
	}
	
	@Test(timeout=20000)
	/**
	 * We only want to match one file out of two
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public void shouldGetCorrectFileInfoMsgWithTimestampThatMatchesOneOfTwoFiles() throws Exception {
		cleanupTempFolder();
		
		System.out.println("\n\nshouldGetCorrectFileInfoMsgWithTimestampThatMatchesOneOfTwoFiles:\n");
		
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(testDirectory+"temp", testDirectory);
		
		// Create first file
		Long firstFileCreatedTimestampSec = new Date().getTime() / 1000L;
		touch(testDirectory+"temp/1.txt");
		
		// Sleep 5 seconds (give FileWatcher time to detect the change & to make the second file be added on another time)
		Thread.sleep(5000);
		
		// Create second file
		touch(testDirectory+"temp/2.txt");
		
		// Sleep 5 seconds (give FileWatcher time to detect the change)
		Thread.sleep(10000);
		
		// Split response by line
		String response = controller.getInfo(firstFileCreatedTimestampSec+1);
		System.out.println("Debug response: "+response);
		String[] actualResponseLines = response.split("\n");
		
		// Get second line and compare with expected response	
		String actualLineTwo = actualResponseLines[1]; 	
		String expectedLineTwo = "add e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 0 "+virtualRoot+"temp/2.txt";
		assertEquals(expectedLineTwo,actualLineTwo);
		
		// Should be only two lines long cause header + one file
		assertEquals(2, actualResponseLines.length);
		
		cleanupTempFolder();
	}
	
	@Test(timeout=20000)
	public void shouldDetectFileAddedDuringRuntime() throws Exception {
		System.out.println("shouldGetListWithOneFileAddedDuringRuntime");
		
		cleanupTempFolder();
		
		String path = testDirectory+"temp/";
		
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(path, testDirectory);
		
		assertEquals(0,controller.fileInfoList.size());
		
		// Wait short till fileController was initialized
		Thread.sleep(1000);
		
		File f = new File(path+"temp.txt");
		f.getParentFile().mkdirs(); 
		f.createNewFile();
		
		while (controller.fileInfoList.size() == 0) {
			
		}
		
		assertEquals(1,controller.fileInfoList.size());
		
		String actualResponse = controller.getInfo(0);
		String expectedResponse = 
				"all "+System.currentTimeMillis() / 1000L+" 1\n"
				+ "add e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 0 "+virtualRoot+"temp/temp.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
		
		cleanupTempFolder();
	}
	
	@Test(timeout=20000)
	public void shouldDetectFileDeletedDuringRuntime() throws Exception {
		cleanupTempFolder();
		
		// Create dummy file
		String path = testDirectory+"temp/";
		File f = new File(path+"temp.txt");
		f.getParentFile().mkdirs(); 
		f.createNewFile();
		
		// Start watching
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(path, testDirectory);
		
		// Wait short till fileController was initialized
		Thread.sleep(1000);
		
		// List should contain one file
		assertEquals(1,controller.fileInfoList.size());
		
		// Delete file
		cleanupTempFolder();
		
		// List should contain two files
		while (controller.fileInfoList.size() != 2) {
			// Test will time out if list will not contain zero files
			// System.out.println(controller.fileInfoList.size());
		}
		
		String actualResponse = controller.getInfo(0);
		String expectedResponse = 
				"all "+System.currentTimeMillis() / 1000L+" 2\n"
				+ "del e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 0 "+virtualRoot+"temp/temp.txt\n"
				+ "add e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 0 "+virtualRoot+"temp/temp.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
	}
	
	private void touch(String fileName) throws IOException{
		File newFile = new File(fileName);
	    touch(newFile, System.currentTimeMillis());
	}
	
	private void touch(File file, long timestamp) throws IOException{
	    if (!file.exists()) {
	       new FileOutputStream(file).close();
	    }

	    file.setLastModified(timestamp);
	}
	
	private void cleanupTempFolder() {
		File dir = new File(testDirectory+"temp/");
	
		for(File file: dir.listFiles()) 
		    if (!file.isDirectory())
		    	file.delete(); 
	}
	

}
