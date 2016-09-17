package studyproject.API.Core.File.Watcher;

import static org.junit.Assert.*;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;

import org.junit.Test;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;

public class FileWatcherControllerTest {

	@Test
	public void listShouldContainOneFile() throws NoSuchAlgorithmException, IOException {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively("/Users/robinhood/Desktop/testDirectory/oneFile");
		assertEquals(1,controller.fileInfoList.size());
	}
	
	@Test
	public void listShouldContainTwoFiles() throws NoSuchAlgorithmException, IOException {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively("/Users/robinhood/Desktop/testDirectory/twoFiles");
		assertEquals(2,controller.fileInfoList.size());
	}
	
	@Test
	public void shouldGetCorrectFileInfoString() throws NoSuchAlgorithmException, IOException {
		// Create dummy FileInfo
		FileInfo fileInfo = new FileInfo();
		fileInfo.checksum = "983x89j23897rw9789n87we9r78w798";
		fileInfo.size = 1000;
		fileInfo.fileName = "\\home\\peter\\abc\\";
		fileInfo.fileAction = FileAction.add;
		fileInfo.parentDirectory = "\\home\\peter\\";
		
		FileWatcherController controller = new FileWatcherController();
		String actualOutput = controller.convertFileInfoToString(fileInfo);
		String expectedOutput = "add "+fileInfo.checksum+" "+fileInfo.size+" "+fileInfo.fileName+"\n";
		assertEquals(expectedOutput, actualOutput);
	}	
	
	@Test
	public void shouldGetCorrectFileInfoMessageWithTimestampZero() throws NoSuchAlgorithmException, IOException {
		FileWatcherController controller = new FileWatcherController();
		controller.watchDirectoryRecursively("/Users/robinhood/Desktop/testDirectory/oneFile");
		
		/**
		 *  example output
		 *      all 1464269498 1\n
			    add 38e80faf7... 421341231 /first-file.txt\n 
		 */
		String actualResponse = controller.getInfo(0);
		String expectedResponse = 
				"all "+System.currentTimeMillis() / 1000L+" 1\n"
				+ "add 736bf95996d40c71307e3727931b721dfb17bd27c441b903a6dd483b37021ac1 8 /Users/robinhood/Desktop/testDirectory/oneFile/testFile.txt\n";
		
		assertEquals(expectedResponse,actualResponse);
	}
	

}
