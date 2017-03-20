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
	public void shouldNotAddChildOfAlreadyWatchedDirToList() throws Exception {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(testDirectory);
		assertEquals(4,controller.getWatchedDirectories().size());
		controller.watchDirectoryRecursively(testDirectory+"oneFile");
		assertEquals(4,controller.getWatchedDirectories().size());	
	}

	@Test
	public void listShouldContainOneFile() throws Exception {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(testDirectory+"oneFile",testDirectory);
		assertEquals(1,controller.fileInfoHistory.size());
	}
	
	@Test
	public void listShouldContainTwoFiles() throws Exception {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(testDirectory+"twoFiles",testDirectory);
		assertEquals(2,controller.fileInfoHistory.size());
	}
	
	@Test
	public void shouldGetCorrectFileInfoString() throws NoSuchAlgorithmException, IOException {
		FileInfo fileInfo = new FileInfo("983x89j23897rw9789n87we9r78w798", 1000, "/"+testDirectory+"oneFile/testFile.txt", testDirectory, FileAction.add);
		
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
				+ "add 8a691dcb75083df7c836d0b4e895c83bc12b6ce6 8 /"+virtualRoot+"oneFile/testFile.txt\n";
		
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
		String expectedLineTwo = "add da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/2.txt";
		assertEquals(expectedLineTwo,actualLineTwo);
		
		assertEquals(3, actualResponseLines.length);
		
		cleanupTempFolder();
	}
	
	@Test(timeout=20000)
	public void shouldDetectFileAddedDuringRuntime() throws Exception {
		System.out.println("shouldGetListWithOneFileAddedDuringRuntime");
		
		cleanupTempFolder();
		
		String path = testDirectory+"temp/";
		
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(path, testDirectory);
		
		assertEquals(0,controller.fileInfoHistory.size());
		
		// Wait short till fileController was initialized
		Thread.sleep(1000);
		
		createDummyFile(path);
		
		while (controller.fileInfoHistory.size() == 0) {
			
		}
		
		assertEquals(1,controller.fileInfoHistory.size());
		
		String actualResponse = controller.getInfo(0);
		String expectedResponse = 
				"all "+System.currentTimeMillis() / 1000L+" 1\n"
				+ "add da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/temp.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
		
		cleanupTempFolder();
	}
	
	@Test(timeout=20000)
	public void shouldDetectFolderWithFileAddedDuringRuntime() throws Exception {
		System.out.println("shouldDetectFolderWithFileAddedDuringRuntime");
		
		cleanupTempFolder();
		
		String path = testDirectory+"temp/";
		
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(path, testDirectory);
		
		assertEquals(0,controller.fileInfoHistory.size());
		
		// Wait short till fileController was initialized
		Thread.sleep(1000);
		
		File f = new File(path+"folder/temp.txt");
		f.getParentFile().mkdirs(); 
		f.createNewFile();
		
		while (controller.fileInfoHistory.size() == 0) {
			
		}
		
		assertEquals(1,controller.fileInfoHistory.size());
		
		String actualResponse = controller.getInfo(0);
		String expectedResponse = 
				"all "+System.currentTimeMillis() / 1000L+" 1\n"
				+ "add da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/folder/temp.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
		
		cleanupTempFolder();
	}
	
	@Test(timeout=20000)
	public void shouldDetectFileDeletedDuringRuntime() throws Exception {
		cleanupTempFolder();
		
		// Create dummy file
		String path = testDirectory+"temp/";
		createDummyFile(path);
		
		// Start watching
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively(path, testDirectory);
		
		// Wait short till fileController was initialized
		Thread.sleep(1000);
		
		// List should contain one file
		assertEquals(1,controller.fileInfoHistory.size());
		
		// Delete file
		cleanupTempFolder();
		
		// List should contain two files
		while (controller.fileInfoHistory.size() != 2) {
			// Test will time out if list will not contain zero files
			// System.out.println(controller.fileInfoList.size());
		}
		
		String actualResponse = controller.getInfo(0);
		String expectedResponse = 
				"all "+System.currentTimeMillis() / 1000L+" 2\n"
				+ "del da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/temp.txt\n"
				+ "add da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/temp.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
	}
	
	@Test(timeout=25000)
	public void shouldDetectFolderWithSubDirsDeletedDuringRuntime() throws Exception {
		cleanupTempFolder();
		
		// Create dummy file
		String path = testDirectory+"temp/";
		createDummyFile(path);
		createDummyFile(path+"subfolder/");
		createDummyFile(path+"subfolder/subfolder2/");

		// Start watching
		FileWatcherController c = new FileWatcherController();
		c.watchDirectoryRecursively(path, testDirectory);
		
		// Wait short till fileController was initialized
		Thread.sleep(1000);
		
		// List should contain three entries
		assertEquals(3,c.fileInfoHistory.size());
		
		// Delete file
		cleanupTempFolder();
				
		// List should contain six files, 3 add + 3 del
		while (c.fileInfoHistory.size() != 6) {
			// Test will time out if list will not contain zero files
			//System.out.println(c.fileInfoHistory.size());
		}
		
		Thread.sleep(2000);

		
		String actualResponse = c.getInfo(0);
		String expectedResponse = 
				"all "+System.currentTimeMillis() / 1000L+" 6\n"
				+ "del da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/subfolder/temp.txt\n"
				+ "del da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/subfolder/subfolder2/temp.txt\n"
				+ "del da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/temp.txt\n"
				+ "add da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/temp.txt\n"
				+ "add da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/subfolder/temp.txt\n"
				+ "add da39a3ee5e6b4b0d3255bfef95601890afd80709 0 /"+virtualRoot+"temp/subfolder/subfolder2/temp.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
		
		// Watched internal directories should only have one entry (root)
		System.out.println(c.getWatchedDirectories());
		assert(c.getWatchedDirectories().size()==1);

	}

	private void createDummyFile(String path) throws IOException {
		File f = new File(path+"temp.txt");
		f.getParentFile().mkdirs(); 
		f.createNewFile();
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
		if (dir.exists())
			purgeDirectory(dir);
	}
	
	void purgeDirectory(File dir) {
	    for (File file: dir.listFiles()) {
	        if (file.isDirectory()) purgeDirectory(file);
	        file.delete();
	    }
	}
	

}
