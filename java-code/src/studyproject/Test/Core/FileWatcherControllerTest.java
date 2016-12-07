package studyproject.Test.Core;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Date;

import org.junit.Test;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.Watcher.FileWatcherController;

public class FileWatcherControllerTest {
	
	private String testDirectory = "java-code/testData/FileWatcherController/";

	@Test
	public void listShouldContainOneFile() throws NoSuchAlgorithmException, IOException {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(testDirectory+"oneFile",testDirectory);
		assertEquals(1,controller.fileInfoList.size());
	}
	
	@Test
	public void listShouldContainTwoFiles() throws NoSuchAlgorithmException, IOException {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(testDirectory+"twoFiles",testDirectory);
		assertEquals(2,controller.fileInfoList.size());
	}
	
	@Test
	public void shouldGetCorrectFileInfoString() throws NoSuchAlgorithmException, IOException {
		FileInfo fileInfo = new FileInfo("983x89j23897rw9789n87we9r78w798", 1000, testDirectory+"oneFile/testFile.txt", testDirectory, FileAction.add);
		
		FileWatcherController controller = new FileWatcherController();
		String actualOutput = controller.convertFileInfoToString(fileInfo);
		String expectedOutput = "add "+fileInfo.checksum+" "+fileInfo.size+" "+fileInfo.fileName+"\n";
		assertEquals(expectedOutput, actualOutput);
	}	
	
	@Test
	public void shouldGetCorrectFileInfoMsgWithTimestampZero() throws NoSuchAlgorithmException, IOException {
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
				+ "add 736bf95996d40c71307e3727931b721dfb17bd27c441b903a6dd483b37021ac1 8 "+testDirectory+"oneFile/testFile.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
	}
	
	@Test
	/**
	 * We only want to match one file out of two that was modified after our time stamp
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public void shouldGetCorrectFileInfoMsgWithTimestampThatMatchesOneOfTwoFiles() throws NoSuchAlgorithmException, IOException {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(testDirectory+"twoFiles", testDirectory);
	     
		Date dt = new Date();
		long millisec = dt.getTime();
				
		File file = new File(testDirectory+"twoFiles/1.txt");
		file.setLastModified(millisec);
		
		File file2 = new File(testDirectory+"twoFiles/2.txt");
		file2.setLastModified(millisec+5000);
		
		Long lastModTime = (millisec+3000) / 1000L;
		
		String actualResponse = controller.getInfo(lastModTime);
		String expectedResponse = 
				"upd "+lastModTime+" 1\n"
				+ "add 7c52011daa0bf2983c4687c2ee6a8d7759503a29f64624fa52970516d9ec45b2 9 "+testDirectory+"twoFiles/2.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
	}
	
	@Test(timeout=20000)
	public void shouldDetectFileAddedDuringRuntime() throws NoSuchAlgorithmException, IOException, InterruptedException {
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
				+ "add e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 0 "+testDirectory+"temp/temp.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
		
		cleanupTempFolder();
	}
	
	@Test(timeout=20000)
	public void shouldDetectFileDeletedDuringRuntime() throws NoSuchAlgorithmException, IOException, InterruptedException {
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
				+ "del e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 0 "+testDirectory+"temp/temp.txt\n"
				+ "add e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 0 "+testDirectory+"temp/temp.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
	}
	
	private void cleanupTempFolder() {
		File dir = new File(testDirectory+"temp/");
	
		for(File file: dir.listFiles()) 
		    if (!file.isDirectory())
		    	file.delete(); 
	}
	

}
