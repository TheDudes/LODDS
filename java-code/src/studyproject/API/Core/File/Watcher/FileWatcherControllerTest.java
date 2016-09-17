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
		String response = controller.getInfo(0);
		
		assertEquals(2,controller.fileInfoList.size());
	}
	

}
